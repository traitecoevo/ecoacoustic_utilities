#' Convert audio files to WAV format
#'
#' This function converts audio files (MP3, M4A, etc.) to WAV format using the `av` package
#' or a system `ffmpeg` installation. This is useful for resolving compatibility issues
#' with bioacoustic software like BirdNET that may struggle with compressed audio headers.
#'
#' @param path Character. Path to a single audio file or a directory containing audio files.
#' @param out_dir Character. Optional. Directory to save the converted WAV files. Defaults to the same directory as the input.
#' @param recursive Logical. Should the function search for audio files recursively? Defaults to FALSE.
#' @param delete_original Logical. Should the original files be deleted after successful conversion? Defaults to FALSE.
#'
#' @return Character vector of paths to the converted WAV files.
#' @export
#'
#' @examples
#' \dontrun{
#' convert_to_wav("path/to/audio_file.mp3")
#' convert_to_wav("path/to/audio_dir", recursive = TRUE)
#' }
convert_to_wav <- function(path, out_dir = NULL, recursive = FALSE, delete_original = FALSE) {
    if (!dir.exists(path) && !file.exists(path)) {
        stop("Path does not exist: ", path)
    }

    # Identify files to convert
    if (dir.exists(path)) {
        # Include .wav so that non-standard WAVs (ADPCM, 24-bit, 32-bit float,
        # wrong sample rate, etc.) are normalised to 48 kHz 16-bit PCM as well.
        pattern <- "\\.(mp3|m4a|ogg|flac|wma|aiff|aif|wav)$"
        files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = recursive, ignore.case = TRUE)
    } else {
        files <- path
    }

    if (length(files) == 0) {
        message("No audio files found to convert in: ", path)
        return(character(0))
    }

    # Check for conversion tools
    has_av <- requireNamespace("av", quietly = TRUE)
    has_ffmpeg <- Sys.which("ffmpeg") != ""

    if (!has_av && !has_ffmpeg) {
        stop("Audio conversion requires the 'av' R package or a system installation of 'ffmpeg'.")
    }

    converted_files <- c()

    for (f in files) {
        is_wav_input <- tolower(tools::file_ext(f)) == "wav"

        # Determine output path
        if (is.null(out_dir)) {
            output_f <- paste0(tools::file_path_sans_ext(f), ".wav")
        } else {
            if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
            output_f <- file.path(out_dir, paste0(tools::file_path_sans_ext(basename(f)), ".wav"))
        }

        # For in-place WAV re-encoding we must write to a temp file first,
        # otherwise the input is clobbered before conversion finishes.
        if (is_wav_input && output_f == f) {
            write_to <- paste0(f, ".converting.wav")
        } else {
            write_to <- output_f
        }

        message("Converting: ", basename(f), " -> ", basename(output_f))

        success <- FALSE
        if (has_av) {
            tryCatch(
                {
                    av::av_audio_convert(f, write_to, sample_rate = 48000L, verbose = FALSE)
                    success <- TRUE
                },
                error = function(e) {
                    warning("Failed to convert ", f, " using 'av': ", e$message)
                    if (file.exists(write_to)) unlink(write_to)
                }
            )
        } else if (has_ffmpeg) {
            # -acodec pcm_s16le -ar 48000: 16-bit PCM at 48 kHz (BirdNET compatible)
            cmd <- sprintf(
                "ffmpeg -y -i %s -acodec pcm_s16le -ar 48000 %s",
                shQuote(f), shQuote(write_to)
            )
            res <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
            if (res == 0) {
                success <- TRUE
            } else {
                warning("Failed to convert ", f, " using system 'ffmpeg'.")
                if (file.exists(write_to)) unlink(write_to)
            }
        }

        if (success) {
            # Move temp file into place for in-place WAV re-encoding
            if (is_wav_input && write_to != output_f) {
                file.rename(write_to, output_f)
            }
            converted_files <- c(converted_files, output_f)
            if (delete_original && !is_wav_input) {
                file.remove(f)
            }
        }
    }

    return(converted_files)
}
