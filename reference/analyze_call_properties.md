# Analyze Call Properties: Frequency Range and Pulse Rate

This function calculates the frequency range (min, max, bandwidth) and
pulse rate (pulses per second) for a given audio file or Wave object. It
can process the entire file or specific selections defined in a
selection table.

## Usage

``` r
analyze_call_properties(
  wave,
  selection_table = NULL,
  threshold = 5,
  msmooth = c(50, 0),
  dmin = 0.001,
  auto_clean = TRUE,
  plot = FALSE,
  ...
)
```

## Arguments

- wave:

  A [`Wave`](https://rdrr.io/pkg/tuneR/man/Wave.html) object or a
  character string pointing to a WAV file.

- selection_table:

  A data frame containing selections. converting to a selection table
  format (e.g., standard \`warbleR\` or Raven format). Must contain
  columns: `selec`, `start`, `end`. If NULL (default), the entire wave
  is analyzed.

- threshold:

  Numeric. Amplitude threshold (%) for pulse detection (passed to
  [`timer`](https://rdrr.io/pkg/seewave/man/timer.html)). Defaults to 5.

- msmooth:

  Vector of length 2. Smoothing parameters for the amplitude envelope
  (window length in samples, overlap in %). Passed to
  [`timer`](https://rdrr.io/pkg/seewave/man/timer.html). Defaults to
  `c(50, 0)` (approx 1ms at 44.1kHz), optimized for rapid pulses/trills.

- dmin:

  Numeric. Minimum duration (in seconds) for a pulse to be detected.
  Passed to [`timer`](https://rdrr.io/pkg/seewave/man/timer.html).
  Defaults to 0.001 (1ms).

- auto_clean:

  Logical. If TRUE (default), the function applies a bandpass filter
  (using the detected frequency range) to the audio before detecting
  pulses. This substantially improves robustness against noise.

- plot:

  Logical. If TRUE, plots the oscillogram and detected pulses. Defaults
  to FALSE.

- ...:

  Additional arguments passed to
  [`timer`](https://rdrr.io/pkg/seewave/man/timer.html) or
  [`specprop`](https://rdrr.io/pkg/seewave/man/specprop.html).

## Value

A data frame with the following columns:

- `file`: Name of the file or "WaveObject".

- `selec`: Selection ID (or "all" if no selection table).

- `duration`: Duration of the selection in seconds.

- `freq_peak`: Peak/Dominant frequency (kHz) (frequency with maximum
  amplitude).

- `freq_min`: Minimum frequency (kHz) (5th percentile of distribution).

- `freq_max`: Maximum frequency (kHz) (95th percentile of distribution).

- `bandwidth`: Bandwidth (kHz) (freq_max - freq_min).

- `pulse_rate`: Pulses per second (detected pulses / duration).

- `num_pulses`: Total number of detected pulses.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze a file
res <- analyze_call_properties("path/to/file.wav")

# Analyze with a selection table
sels <- data.frame(selec = 1, start = 0, end = 1)
res_sel <- analyze_call_properties("path/to/file.wav", selection_table = sels)
} # }
```
