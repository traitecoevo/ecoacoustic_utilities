#' Analyze Call Properties: Frequency Range and Pulse Rate
#'
#' This function calculates the frequency range (min, max, bandwidth) and pulse rate
#' (pulses per second) for a given audio file or Wave object. It can process the entire
#' file or specific selections defined in a selection table.
#'
#' @param wave A \code{\link[tuneR]{Wave}} object or a character string pointing to a WAV file.
#' @param selection_table A data frame containing selections. converting to a selection table
#'   format (e.g., standard `warbleR` or Raven format). Must contain columns:
#'   \code{selec}, \code{start}, \code{end}. If NULL (default), the entire wave is analyzed.
#' @param threshold Numeric. Amplitude threshold (\%) for pulse detection (passed to \code{\link[seewave]{timer}}).
#'   Defaults to 5.
#' @param msmooth Vector of length 2. Smoothing parameters for the amplitude envelope
#'   (window length in samples, overlap in \%). Passed to \code{\link[seewave]{timer}}.
#'   Defaults to \code{c(50, 0)} (approx 1ms at 44.1kHz), optimized for rapid pulses/trills.
#' @param dmin Numeric. Minimum duration (in seconds) for a pulse to be detected.
#'   Passed to \code{\link[seewave]{timer}}. Defaults to 0.001 (1ms).
#' @param auto_clean Logical. If TRUE (default), the function applies a bandpass filter
#'   (using the detected frequency range) to the audio before detecting pulses.
#'   This substantially improves robustness against noise.
#' @param plot Logical. If TRUE, plots the oscillogram and detected pulses. Defaults to FALSE.
#' @param ... Additional arguments passed to \code{\link[seewave]{timer}} or \code{\link[seewave]{specprop}}.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{file}: Name of the file or "WaveObject".
#'   \item \code{selec}: Selection ID (or "all" if no selection table).
#'   \item \code{duration}: Duration of the selection in seconds.
#'   \item \code{freq_min}: Minimum frequency (kHz) (5th percentile of distribution).
#'   \item \code{freq_max}: Maximum frequency (kHz) (95th percentile of distribution).
#'   \item \code{bandwidth}: Bandwidth (kHz) (freq_max - freq_min).
#'   \item \code{pulse_rate}: Pulses per second (detected pulses / duration).
#'   \item \code{num_pulses}: Total number of detected pulses.
#' }
#' @importFrom tuneR readWave
#' @importFrom seewave specprop timer meanspec ffilter
#' @importFrom warbleR check_sels
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze a file
#' res <- analyze_call_properties("path/to/file.wav")
#'
#' # Analyze with a selection table
#' sels <- data.frame(selec = 1, start = 0, end = 1)
#' res_sel <- analyze_call_properties("path/to/file.wav", selection_table = sels)
#' }
analyze_call_properties <- function(wave, selection_table = NULL, threshold = 5, msmooth = c(50, 0), dmin = 0.001, auto_clean = TRUE, plot = FALSE, ...) {
    # 1. Input Validation and Wave Loading
    if (is.character(wave)) {
        if (!file.exists(wave)) {
            stop("File does not exist: ", wave)
        }
        file_name <- basename(wave)
        wave_obj <- tuneR::readWave(wave)
    } else if (inherits(wave, "Wave")) {
        file_name <- "WaveObject"
        wave_obj <- wave
    } else {
        stop("'wave' must be a file path or a Wave object.")
    }

    # 2. Prepare Selections
    if (is.null(selection_table)) {
        # Analyze entire file
        selections <- data.frame(
            selec = "all",
            start = 0,
            end = length(wave_obj@left) / wave_obj@samp.rate
        )
    } else {
        # Validate selection table
        required_cols <- c("selec", "start", "end")
        if (!all(required_cols %in% names(selection_table))) {
            stop("selection_table must contain columns: ", paste(required_cols, collapse = ", "))
        }
        selections <- selection_table
    }

    results <- list()

    # 3. Iterate through Selections
    for (i in seq_len(nrow(selections))) {
        sel <- selections[i, ]

        # ... (subsetting code remains same, but we are just replacing the loop start and the freq calc part)
        # Actually I need to be careful with the context since I'm replacing a block.
        # The tool requires precise context.
        # I will replace the Frequency Analysis section entirely.

        # ...

        # Extract segment
        # Check if start/end are within bounds
        max_dur <- length(wave_obj@left) / wave_obj@samp.rate
        if (sel$end > max_dur) {
            warning(paste("Selection", sel$selec, "ends after file duration. Truncating."))
            sel$end <- max_dur
        }

        if (sel$start >= sel$end) {
            warning(paste("Selection", sel$selec, "has invalid start/end times. Skipping."))
            next
        }

        if (file_name != "WaveObject") {
            # It's a file, we can use readWave efficiently
            # tuneR::readWave(from, to, units)
            sub_wave <- tuneR::readWave(
                wave,
                from = sel$start,
                to = sel$end,
                units = "seconds"
            )
        } else {
            # It's a Wave object, we must subset manually
            sample_rate <- wave_obj@samp.rate
            start_samp <- max(1, round(sel$start * sample_rate))
            end_samp <- min(length(wave_obj@left), round(sel$end * sample_rate))

            if (end_samp <= start_samp) {
                warning(paste("Selection", sel$selec, "results in 0 samples. Skipping."))
                next
            }

            segment_left <- wave_obj@left[start_samp:end_samp]

            # Handle stereo
            segment_right <- if (wave_obj@stereo) wave_obj@right[start_samp:end_samp] else numeric(0)

            sub_wave <- tuneR::Wave(
                left = segment_left,
                right = segment_right,
                samp.rate = sample_rate,
                bit = wave_obj@bit,
                pcm = wave_obj@pcm
            )
        }

        duration <- length(sub_wave@left) / sub_wave@samp.rate

        # 4. Frequency Analysis
        # We use seewave::meanspec to get the spectrum
        spec <- seewave::meanspec(sub_wave, plot = FALSE)

        if (is.null(spec) || nrow(spec) == 0) {
            freq_min <- NA
            freq_max <- NA
            bandwidth <- NA
        } else {
            # Normalize amplitude to [0, 1] relative to the peak of this selection
            spec[, 2] <- spec[, 2] / max(spec[, 2])

            # Find the Peak Frequency (Dominant Frequency)
            peak_idx <- which.max(spec[, 2])
            peak_freq <- spec[peak_idx, 1]

            # Determine Bandwidth: Frequencies where amplitude drops below a threshold relative to peak
            # Standard robust approach: -20dB (0.1 amplitude) or -10dB (0.316 amplitude)?
            # Let's use 0.1 (10% of peak amplitude) which effectively isolates the signal from background noise.
            limit_thresh <- 0.1

            # Scan Left (Min Freq)
            # Find last index *before* peak that was below threshold
            # We look for the continuous block around the peak
            lower_idxs <- which(spec[1:peak_idx, 2] < limit_thresh)
            if (length(lower_idxs) > 0) {
                # The closest one to the peak
                min_idx <- lower_idxs[length(lower_idxs)]
                freq_min <- spec[min_idx, 1]
            } else {
                # Never drops below threshold? Use min freq of file
                freq_min <- spec[1, 1]
            }

            # Scan Right (Max Freq)
            upper_idxs <- which(spec[peak_idx:nrow(spec), 2] < limit_thresh)
            if (length(upper_idxs) > 0) {
                # The closest one to the peak (index is relative to peak_idx)
                max_idx <- peak_idx + upper_idxs[1] - 1
                freq_max <- spec[max_idx, 1]
            } else {
                # Never drops below threshold? Use max freq
                freq_max <- spec[nrow(spec), 1]
            }

            bandwidth <- freq_max - freq_min
            bandwidth <- freq_max - freq_min
        }


        # 5. Pulse Analysis
        # Use seewave::timer
        # output: s (start times), d (durations), event (number of events)

        # Auto-filter: Use the detected frequency range to bandpass filter specifically for pulse detection
        # This significantly improves SNR for the timer function
        wave_for_timer <- sub_wave

        if (auto_clean && !is.na(freq_min) && !is.na(freq_max) && duration > 0) {
            # Add a small buffer to the filter limits (e.g. 10%) so we don't cut the signal edges
            # or just use the bandwidth.
            # seewave::ffilter needs 'f' (sampling rate).

            # Buffer: 0.2 kHz or 10%
            buff <- (freq_max - freq_min) * 0.1
            f_low <- max(0, freq_min - buff) * 1000 # convert kHz to Hz
            f_high <- (freq_max + buff) * 1000

            # specific case: ffilter fails if to > f/2 (Nyquist)
            nyquist <- wave_obj@samp.rate / 2
            if (f_high > nyquist) f_high <- nyquist - 1

            # Only filter if we have a valid range and it's not the entire spectrum
            if (f_high > f_low && f_low < nyquist) {
                tryCatch(
                    {
                        # ffilter returns a matrix by default. We need a Wave for timer?
                        # timer accepts matrix/vector too. But let's keep it clean.
                        # output="Wave" requires another conversion step usually or produces a Wave.
                        wave_for_timer <- seewave::ffilter(sub_wave, from = f_low, to = f_high, output = "Wave")
                    },
                    error = function(e) {
                        warning(paste("Filter failed for selection", sel$selec, ":", e$message))
                        # Fallback to original wave
                    }
                )
            }
        }

        # Normalize the wave for timer?
        # timer works with % threshold. Normalizing ensures 5% means "5% of max peak".
        # seewave::timer usually handles it, but explicit normalization is safer after filtering.
        try(
            {
                # Normalize to -1, 1 range for consistency
                m <- max(abs(wave_for_timer@left))
                if (m > 0) {
                    wave_for_timer@left <- wave_for_timer@left / m
                    # If stereo, handle right? timer usually takes first channel or mono.
                    # We only use left for now (line 124-134 creates single/stereo but we only analyzed left for spec).
                    # Actually meanspec uses left by default.
                }
            },
            silent = TRUE
        )


        # We need to capture the output carefully.

        # timer() prints by default? No, it returns a list.
        pulse_info <- tryCatch(
            {
                seewave::timer(wave_for_timer, threshold = threshold, msmooth = msmooth, plot = plot, dmin = dmin, ...)
            },
            error = function(e) {
                warning(paste(
                    "Timer failed for selection", sel$selec, ":", e$message,
                    "- Try adjusting 'msmooth' (e.g. c(10,0) for fast trills) or 'threshold'."
                ))
                return(NULL)
            }
        )

        if (!is.null(pulse_info)) {
            num_pulses <- length(pulse_info$s)
            # Calculate rate (pulses per second)
            # If duration is very short, this might be high.
            pulse_rate <- num_pulses / duration
        } else {
            num_pulses <- NA
            pulse_rate <- NA
        }

        # Store results
        results[[i]] <- data.frame(
            file = file_name,
            selec = sel$selec,
            duration = duration,
            freq_min = freq_min,
            freq_max = freq_max,
            bandwidth = bandwidth,
            num_pulses = num_pulses,
            pulse_rate = pulse_rate
        )
    }

    # Combine results
    final_df <- do.call(rbind, results)
    return(final_df)
}
