# ecoacoustic_utilities (EcoacousticUtilities) — Project Notes for Claude

R package of **generic, reusable** ecoacoustic utilities. Remote:
`traitecoevo/ecoacoustic_utilities`.

## Where this repo sits — read before building anything new

**This repo owns:** anything useful to someone who is *not* building
`reallybig` — audio sourcing (`get_xc_sounds`, `get_inat_sounds`,
`get_ala_sound`, `get_anwc_sounds`), clip quarantine, duplicate-wav
finding, audio conversion, call-property analysis. **It does NOT own:**
anything recognizer- or library-specific. That’s
`Training_library_assembly_pipeline` (curation), `BirdNET-Analyzer`
(engine), `soundscape-eval` (evaluation).

**The test of whether code belongs here: would it help someone building
a different library?** If yes, here. If it only makes sense for
`reallybig`, it belongs upstream. This seam is currently healthy —
`Training_library/species/download_all.R` does
[`library(EcoacousticUtilities)`](https://traitecoevo.github.io/ecoacoustic_utilities/)
and calls
[`get_xc_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_xc_sounds.md)
rather than reimplementing the Xeno-Canto fetch. Keep it that way; a
second XC downloader anywhere is a bug.

Full ownership table, seams, venvs, shared data:
**`~/Documents/ECOACOUSTICS.md`**.

## Gotchas

- **[`get_inat_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_inat_sounds.md)
  resolves by fuzzy name match and takes the first hit** — it can
  silently source the *wrong species* (a grebe query returning
  swamphen). Verify the taxon it resolved to before trusting a batch.
- **This is a ~43 MB package sitting in a ~6 GB directory.**
  `clips_run/` (4 G), `clips_for_sarah/` (1.2 G) and `clips_run.zip`
  (905 M) are transient audio, all gitignored. Never `git add -f` them.
  Note `clips_run/` is written to from *another repo*
  (`Training_library/species/download_all.R`) — don’t assume it’s local
  output.
- `birdnet_test/` is **not** ignored — it holds tracked test fixtures.
- API keys (`$XC_API_KEY`) live in `~/.zshrc`, interactive shells only →
  `zsh -ic`.
