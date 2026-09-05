make_stereo_wav <- function(dir, samp_rate = 44100L) {
    n <- samp_rate # 1 second
    t <- seq_len(n) / samp_rate
    # distinct tones per channel so a downmix is detectable
    left <- round(16000 * sin(2 * pi * 440 * t))
    right <- round(16000 * sin(2 * pi * 880 * t))
    w <- tuneR::Wave(left = left, right = right, samp.rate = samp_rate, bit = 16)
    f <- file.path(dir, "stereo_input.wav")
    tuneR::writeWave(w, f)
    f
}

has_converter <- function() {
    requireNamespace("av", quietly = TRUE) || Sys.which("ffmpeg") != ""
}

test_that("convert_to_wav downmixes to mono 48 kHz by default", {
    skip_if_not(has_converter(), "needs 'av' or system ffmpeg")
    in_dir <- withr::local_tempdir()
    out_dir <- withr::local_tempdir()
    f <- make_stereo_wav(in_dir)

    out <- suppressMessages(convert_to_wav(f, out_dir = out_dir))

    expect_length(out, 1)
    expect_true(file.exists(out))
    w <- tuneR::readWave(out)
    expect_false(w@stereo)
    expect_equal(w@samp.rate, 48000)
    expect_equal(w@bit, 16)
})

test_that("convert_to_wav preserves channel layout when channels = NULL", {
    skip_if_not(has_converter(), "needs 'av' or system ffmpeg")
    in_dir <- withr::local_tempdir()
    out_dir <- withr::local_tempdir()
    f <- make_stereo_wav(in_dir)

    out <- suppressMessages(convert_to_wav(f, out_dir = out_dir, channels = NULL))

    w <- tuneR::readWave(out)
    expect_true(w@stereo)
    expect_equal(w@samp.rate, 48000)
})

test_that("convert_to_wav rejects invalid channel counts", {
    in_dir <- withr::local_tempdir()
    f <- make_stereo_wav(in_dir)

    expect_error(convert_to_wav(f, channels = 0), "positive whole number")
    expect_error(convert_to_wav(f, channels = 1.5), "positive whole number")
    expect_error(convert_to_wav(f, channels = c(1, 2)), "positive whole number")
    expect_error(convert_to_wav(f, channels = NA), "positive whole number")
})
