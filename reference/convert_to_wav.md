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
  delete_original = FALSE
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

## Value

Character vector of paths to the converted WAV files.

## Examples

``` r
if (FALSE) { # \dontrun{
convert_to_wav("path/to/audio_file.mp3")
convert_to_wav("path/to/audio_dir", recursive = TRUE)
} # }
```
