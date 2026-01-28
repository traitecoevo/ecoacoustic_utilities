test_that("analyze_call_properties works on synthetic data", {
    # create a synthetic wave: 1 second, 440Hz sine wave (simulating a continuous call)
    # mixed with some pulses

    # 1. Pure Tone (Continuous)
    # 2 pulses of 0.1s duration at 0.0 and 0.5s
    sr <- 44100
    t <- seq(0, 1, 1 / sr)

    # Pulse 1
    s1 <- sin(2 * pi * 2000 * t) # 2kHz tone
    # Envelope: 0.1s on, 0.4s off, 0.1s on, 0.4s off
    env <- numeric(length(t))
    env[1:(0.1 * sr)] <- 1
    env[(0.5 * sr):(0.6 * sr)] <- 1

    sig <- s1 * env

    # Create Wave object
    w <- tuneR::Wave(left = sig, samp.rate = sr, bit = 16)

    # Test without selection table
    res <- analyze_call_properties(w, threshold = 5)

    expect_equal(res$file, "WaveObject")
    expect_equal(res$selec, "all")
    expect_gt(res$pulse_rate, 0)
    # Expect roughly 2 pulses in 1 second = 2 pps
    # Note: timer might detect start/end, so 2 pulses.
    expect_equal(res$num_pulses, 2)
    expect_equal(res$pulse_rate, 2, tolerance = 0.1)

    # Frequency check: 2kHz.
    # Q05 and Q95 should be around 2kHz
    # Peak should be exactly around 2kHz
    expect_true(abs(res$freq_peak - 2.0) < 0.1)
    expect_true(abs(res$freq_min - 2.0) < 0.5)
    expect_true(abs(res$freq_max - 2.0) < 0.5)


    # Test with selection table
    # Using slightly larger selections to ensure the pulse is fully captured despite smoothing
    # Pulse 1: 0.0 to 0.1. Selection: 0.0 to 0.15
    # Pulse 2: 0.5 to 0.6. Selection: 0.45 to 0.65
    sels <- data.frame(
        selec = c(1, 2),
        start = c(0, 0.45),
        end = c(0.15, 0.65)
    )

    res_sel <- analyze_call_properties(w, selection_table = sels, threshold = 5)

    expect_equal(nrow(res_sel), 2)
    expect_equal(res_sel$selec, c(1, 2))
    # Each selection has 1 pulse
    expect_equal(res_sel$num_pulses, c(1, 1))

    # Duration of sel 1 is 0.15s. Rate = 1/0.15 = 6.66
    # Duration of sel 2 is 0.2s. Rate = 1/0.2 = 5
    expect_equal(res_sel$pulse_rate, c(1 / 0.15, 1 / 0.2), tolerance = 0.1)
})

test_that("analyze_call_properties detects valid bioacoustic ranges (30-120Hz)", {
    # Simulate a 120Hz Trill
    # 1 second duration, 120 pulses
    sr <- 44100
    t <- seq(0, 1, 1 / sr)

    # Carrier: 3kHz
    carrier <- sin(2 * pi * 3000 * t)

    # Modulator: 120Hz Square wave (approx)
    # sin(2*pi*120*t) > 0
    mod <- ifelse(sin(2 * pi * 120 * t) > 0, 1, 0)

    sig <- carrier * mod
    w_trill <- tuneR::Wave(left = sig, samp.rate = sr, bit = 16)

    # Analyze
    # auto_clean=TRUE might filter it slightly but freq is clear (3kHz)
    res <- analyze_call_properties(w_trill, threshold = 5, plot = FALSE)

    # Should find close to 120 pulses
    expect_equal(res$num_pulses, 120, tolerance = 5) # Tolerance for start/end artifacts
    expect_equal(res$pulse_rate, 120, tolerance = 5)

    # Verify bandwidth is reasonable (carrier is 3kHz)
    expect_true(res$freq_min < 3.0 && res$freq_max > 3.0)
})

test_that("analyze_call_properties handles errors", {
    expect_error(analyze_call_properties("non_existent_file.wav"))

    # Invalid selection table
    w <- tuneR::silence(duration = 1, samp.rate = 44100)
    bad_sels <- data.frame(id = 1, from = 0, to = 1) # Wrong colnames
    expect_error(analyze_call_properties(w, selection_table = bad_sels))
})
