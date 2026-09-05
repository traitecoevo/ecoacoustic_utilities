# Convert audio files to WAV format

This function converts audio files (MP3, M4A, etc.) to WAV format using
the \`av\` package or a system \`ffmpeg\` installation. This is useful
for resolving compatibility issues with bioacoustic software like
BirdNET that may struggle with compressed audio headers.

## Usage

``` r
convert_to_wav(
  path,
  out_dir = NULL,
  recursive = FALSE,
  delete_original = FALSE,
  channels = 1
)
```

## Arguments

- path:

  Character. Path to a single audio file or a directory containing audio
  files.

- out_dir:

  Character. Optional. Directory to save the converted WAV files.
  Defaults to the same directory as the input.

- recursive:

  Logical. Should the function search for audio files recursively?
  Defaults to FALSE.

- delete_original:

  Logical. Should the original files be deleted after successful
  conversion? Defaults to FALSE.

- channels:

  Integer or NULL. Number of output channels. Defaults to 1 (mono),
  matching what BirdNET and training libraries expect. Use \`channels =
  NULL\` to preserve the source channel layout.

## Value

Character vector of paths to the converted WAV files.

## Details

Output is normalised to BirdNET's expected shape: 16-bit PCM at 48 kHz,
and mono by default (see \`channels\`). Training libraries are
conventionally mono, so a stereo source is downmixed rather than passed
through.

## See also

Other audio:
[`analyze_call_properties()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/analyze_call_properties.md),
[`biggest_files()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/biggest_files.md),
[`check_clips_on_disk()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/check_clips_on_disk.md),
[`find_duplicate_wavs()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/find_duplicate_wavs.md),
[`print.training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/print.training_dataset_summary.md),
[`quarantine_clips()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/quarantine_clips.md),
[`training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/training_dataset_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
convert_to_wav("path/to/audio_file.mp3")
convert_to_wav("path/to/audio_dir", recursive = TRUE)
# keep the source's stereo layout instead of downmixing
convert_to_wav("path/to/audio_dir", channels = NULL)
} # }
```
