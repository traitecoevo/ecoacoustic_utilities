test_that("check_clips_on_disk flags present and missing clips", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "Genus sp_Common"))
  writeLines("x", file.path(root, "Genus sp_Common", "here.wav"))

  df <- data.frame(
    species = "Genus sp_Common",
    clip    = c("here.wav", "gone.wav"),
    stringsAsFactors = FALSE
  )

  out <- suppressMessages(check_clips_on_disk(df, root))
  expect_equal(out$exists, c(TRUE, FALSE))
  expect_true(all(c("path", "exists") %in% colnames(out)))
  expect_equal(nrow(out), 2L)
})

test_that("check_clips_on_disk preserves row order and original columns", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "A"))
  writeLines("x", file.path(root, "A", "b.wav"))
  df <- data.frame(species = "A", clip = "b.wav", extra = 99,
                   stringsAsFactors = FALSE)
  out <- suppressMessages(check_clips_on_disk(df, root))
  expect_equal(out$extra, 99)
})

test_that("check_clips_on_disk supports custom column names", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "A"))
  writeLines("x", file.path(root, "A", "b.wav"))
  df <- data.frame(sp = "A", file = "b.wav", stringsAsFactors = FALSE)
  out <- suppressMessages(
    check_clips_on_disk(df, root, species_col = "sp", clip_col = "file")
  )
  expect_true(out$exists)
})

test_that("check_clips_on_disk errors on bad input", {
  root <- withr::local_tempdir()
  expect_error(check_clips_on_disk(list(), root), "data frame")
  expect_error(
    check_clips_on_disk(data.frame(x = 1), root),
    "missing column"
  )
  expect_error(
    check_clips_on_disk(data.frame(species = "A", clip = "b.wav"), "/no/such/dir"),
    "does not exist"
  )
})
