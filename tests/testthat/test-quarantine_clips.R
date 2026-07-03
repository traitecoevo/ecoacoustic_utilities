make_plan_lib <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "A a_Aye"))
  dir.create(file.path(root, "B b_Bee"))
  for (f in c("keep.wav", "drop1.wav", "drop2.wav")) {
    writeLines("x", file.path(root, "A a_Aye", f))
  }
  writeLines("x", file.path(root, "B b_Bee", "gone_ref.wav"))  # ref only

  plan <- data.frame(
    species = c("A a_Aye", "A a_Aye", "A a_Aye", "B b_Bee"),
    clip    = c("keep.wav", "drop1.wav", "drop2.wav", "missing.wav"),
    action  = c("keep", "remove", "remove", "remove"),
    stringsAsFactors = FALSE
  )
  plan$path   <- file.path(root, plan$species, plan$clip)
  plan$exists <- file.exists(plan$path)   # "missing.wav" -> FALSE
  list(root = root, plan = plan)
}

test_that("quarantine_clips moves only removable, on-disk clips", {
  s <- make_plan_lib()
  q <- withr::local_tempdir()
  res <- suppressMessages(quarantine_clips(s$plan, q))

  # 2 removable + on disk (drop1, drop2); missing.wav skipped; keep.wav untouched
  expect_equal(nrow(res), 2L)
  expect_true(all(res$moved))
  expect_true(all(file.exists(file.path(q, "A a_Aye", c("drop1.wav", "drop2.wav")))))
  expect_false(any(file.exists(file.path(s$root, "A a_Aye", c("drop1.wav", "drop2.wav")))))
  expect_true(file.exists(file.path(s$root, "A a_Aye", "keep.wav")))
  expect_true(file.exists(file.path(q, "quarantine_manifest.csv")))
})

test_that("dry_run moves nothing and writes no manifest", {
  s <- make_plan_lib()
  q <- withr::local_tempdir()
  res <- suppressMessages(quarantine_clips(s$plan, q, dry_run = TRUE))
  expect_equal(nrow(res), 2L)
  expect_false(file.exists(file.path(q, "quarantine_manifest.csv")))
  expect_true(file.exists(file.path(s$root, "A a_Aye", "drop1.wav")))
})

test_that("quarantine is reversible via the manifest", {
  s <- make_plan_lib()
  q <- withr::local_tempdir()
  suppressMessages(quarantine_clips(s$plan, q))
  man <- read.csv(file.path(q, "quarantine_manifest.csv"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(man))) file.rename(man$dest[i], man$path[i])
  expect_true(all(file.exists(file.path(s$root, "A a_Aye",
                                        c("drop1.wav", "drop2.wav")))))
})

test_that("a read-only source dir leaves no duplicate and reports failure", {
  s <- make_plan_lib()
  q <- withr::local_tempdir()
  ro <- file.path(s$root, "A a_Aye")
  Sys.chmod(ro, mode = "0555")                     # revoke write on source dir
  withr::defer(Sys.chmod(ro, mode = "0755"))       # restore so tempdir cleans up

  res <- suppressMessages(quarantine_clips(s$plan, q))
  # removal must fail (can't delete from read-only dir) ...
  expect_false(any(res$moved))
  # ... and crucially must NOT leave a copy behind in quarantine
  expect_false(any(file.exists(res$dest)))
  # source files remain intact
  expect_true(all(file.exists(res$path)))
})

test_that("a second call appends to the manifest rather than clobbering it", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "A a_Aye"))
  dir.create(file.path(root, "B b_Bee"))
  writeLines("x", file.path(root, "A a_Aye", "d1.wav"))
  writeLines("x", file.path(root, "B b_Bee", "d2.wav"))
  mk <- function(sp, clip) {
    p <- data.frame(species = sp, clip = clip, action = "remove",
                    stringsAsFactors = FALSE)
    p$path <- file.path(root, sp, clip); p$exists <- file.exists(p$path); p
  }
  q <- withr::local_tempdir()
  suppressMessages(quarantine_clips(mk("A a_Aye", "d1.wav"), q))
  suppressMessages(quarantine_clips(mk("B b_Bee", "d2.wav"), q))

  man <- read.csv(file.path(q, "quarantine_manifest.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(man), 2L)                       # both calls recorded
  expect_setequal(man$clip, c("d1.wav", "d2.wav"))
})

test_that("quarantine_clips handles an empty selection", {
  s <- make_plan_lib()
  s$plan$action <- "keep"
  q <- withr::local_tempdir()
  res <- suppressMessages(quarantine_clips(s$plan, q))
  expect_equal(nrow(res), 0L)
})

test_that("quarantine_clips validates input", {
  expect_error(quarantine_clips(data.frame(x = 1), tempdir()),
               "reconciled removal plan")
})
