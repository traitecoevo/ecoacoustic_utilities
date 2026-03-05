# Setting Up Codecov (Optional)

The test coverage workflow is configured but requires a Codecov token to upload results.

## Option 1: Enable Codecov (Recommended for public repos)

1. Go to https://codecov.io
2. Sign in with GitHub
3. Enable your repository `wcornwell/ecoacoustic_utilities`
4. Copy the upload token
5. Add it to GitHub Secrets:
   - Go to your repo on GitHub
   - Settings → Secrets and variables → Actions
   - Click "New repository secret"
   - Name: `CODECOV_TOKEN`
   - Value: paste your token
   - Click "Add secret"

## Option 2: Disable Coverage Upload

If you don't want to use Codecov, the workflow will continue to run tests but skip the upload step (it won't fail CI).

## Option 3: Remove Coverage Workflow

Delete `.github/workflows/test-coverage.yaml` if you don't need coverage tracking.

## Current Status

The workflow is configured to **not fail** if the token is missing, so your CI will still pass. Coverage upload is optional.
