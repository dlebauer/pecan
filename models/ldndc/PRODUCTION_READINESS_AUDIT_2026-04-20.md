# PEcAn.LDNDC production readiness audit (2026-04-20)

## Verdict

**Not Ready**

The package is now **usable for local execution**. In a clean audit workspace, the following all completed successfully:

- direct LDNDC sample execution from the bundled macOS archive
- `testthat::test_dir('models/ldndc/tests/testthat')` with `PASS 23`
- a strict `met2model.LDNDC` smoke test
- a package-generated `write.config.LDNDC` + `job.sh` run
- `model2netcdf.LDNDC` output generation for `2010.nc` through `2015.nc`

It is still **not production-ready for unattended use** because operational recovery and deployment guarantees are not yet hardened enough for repeatable worker execution.

## Local environment

- Host: macOS 26.3 (`Darwin 25.3.0`, `arm64`)
- `R`: 4.5.2
- `Rscript`: 4.5.2
- `quarto`: 1.8.26
- LDNDC archive: `models/ldndc/vignettes/local-assets/ldndc-1.37.mac64.2026-03-03.tar.bz2`
- LDNDC revision observed locally: `12193`
- Audit workspace: `/tmp/ldndc-audit.uWzh5x`

## What was executed

- Extracted the bundled LDNDC runtime into an isolated workspace under `/tmp`.
- Removed macOS quarantine attributes from the extracted runtime copy.
- Verified direct model execution with an explicit runtime config rooted at the extracted LDNDC tree.
- Installed the local `models/ldndc` package into `/tmp/ldndc-audit.uWzh5x/Rlib`.
- Ran the package test directory with the current branch code.
- Ran a strict `met2model.LDNDC` smoke test using a repo-derived netCDF fixture.
- Ran `write.config.LDNDC` to generate a full PEcAn run directory.
- Ran the generated `job.sh` end-to-end with `R_LIBS` set to the audit library so the launched `Rscript` used the package under test.
- Verified creation of yearly netCDF outputs for 2010 through 2015.
- Updated and rerendered the Quarto audit workflow in `models/ldndc/vignettes/`.

## Validated fixes in this rerun

### 1) The generated PEcAn run directory now produces valid XML

Observed blocker before the fix:

- `setup.xml` contained a literal quoted XML fragment for `@reportarable@`
- `project.ldndc` contained a literal quoted placeholder for `@Groundwater@`
- `site.xml` left `@Info_Use_History@`, `@Soil_Type@`, and `@Litter_Height@` unresolved when no pool-init file was provided

Implemented fix:

- removed the template-side quoting around `@reportarable@` and `@Groundwater@`
- populated default site metadata in the no-pool-init path of `write.config.LDNDC`
- added regression checks that assert the generated `project.ldndc`, `setup.xml`, and `site.xml` are populated and syntactically plausible

Files changed:

- `models/ldndc/inst/project.ldndc`
- `models/ldndc/inst/setup_template.xml`
- `models/ldndc/R/write.config.LDNDC.R`
- `models/ldndc/tests/testthat/test.write.config.R`

### 2) The packaged launcher path is usable locally

Validated behavior:

- the generated `job.sh` uses an explicit runtime config rooted at the extracted LDNDC installation
- the packaged default `airchemistry.txt` is present and copied into the generated run directory
- a fresh generated run completed with normal LDNDC termination and produced yearly netCDF files

Evidence:

- raw output directory: `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/Output`
- netCDF outputs:
  - `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/2010.nc`
  - `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/2011.nc`
  - `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/2012.nc`
  - `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/2013.nc`
  - `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/2014.nc`
  - `/tmp/ldndc-audit.uWzh5x/generated-outdir-3/GENERATED-RUN/2015.nc`

### 3) The current tests and audit workflow exercise the live package contracts

Validated behavior:

- `testthat` now covers generated config writing, current converter expectations, and the strict meteorology path
- the Quarto workflow now uses the actual package-generated run path instead of a hand-built project workaround
- the notebook keeps review artifacts in `models/ldndc/vignettes/`

Files updated:

- `models/ldndc/tests/testthat/test.met2model.R`
- `models/ldndc/tests/testthat/test.model2netcdf.R`
- `models/ldndc/tests/testthat/test.write.config.R`
- `models/ldndc/vignettes/ldndc_minimal_coupler_audit.qmd`

## Execution evidence

- ✅ `uname -a`
- ✅ `sw_vers`
- ✅ `R --version`
- ✅ `Rscript --version`
- ✅ `quarto --version`
- ✅ `tar -xjf models/ldndc/vignettes/local-assets/ldndc-1.37.mac64.2026-03-03.tar.bz2 -C /tmp/ldndc-audit.uWzh5x`
- ⚠️ `xattr -dr com.apple.quarantine /tmp/ldndc-audit.uWzh5x/ldndc-1.37.mac64`
  Required on this macOS host for the downloaded binary to launch.
- ✅ `HOME=/tmp/ldndc-audit.uWzh5x/home sh install.sh`
- ✅ `cd /tmp/ldndc-audit.uWzh5x/ldndc-1.37.mac64 && ./bin/ldndc -c /tmp/ldndc-audit.uWzh5x/ldndc-direct.conf projects/grassland/DE_graswang/DE_graswang.ldndc`
- ✅ `TMPDIR=/tmp R CMD INSTALL -l /tmp/ldndc-audit.uWzh5x/Rlib models/ldndc`
- ✅ `TMPDIR=/tmp Rscript -e ".libPaths(c('/tmp/ldndc-audit.uWzh5x/Rlib', .libPaths())); testthat::test_dir('models/ldndc/tests/testthat')"`
  - Result: `PASS 23`
- ✅ `TMPDIR=/tmp Rscript -e ".libPaths(c('/tmp/ldndc-audit.uWzh5x/Rlib', .libPaths())); PEcAn.LDNDC::write.config.LDNDC(...)"`
  Generated fresh `project.ldndc`, `setup.xml`, `site.xml`, `job.sh`, and packaged default inputs under `/tmp/ldndc-audit.uWzh5x/generated-rundir-3/GENERATED-RUN`
- ✅ `R_LIBS=/tmp/ldndc-audit.uWzh5x/Rlib sh /tmp/ldndc-audit.uWzh5x/generated-rundir-3/GENERATED-RUN/job.sh`
  - Result: normal model termination plus `2010.nc` through `2015.nc`
- ✅ `quarto render models/ldndc/vignettes/ldndc_minimal_coupler_audit.qmd`
- ✅ `quarto render models/ldndc/vignettes/ldndc_minimal_coupler_audit.qmd --to gfm`

## Current model-wrapper alignment

LDNDC follows the same PEcAn wrapper pattern used by SIPNET and ED2: the
model-specific `write.config.*` function writes run files and a generated
`job.sh`, the job script launches the external model binary, and
`model2netcdf.*` converts raw model output into PEcAn-standard yearly NetCDF.
The launcher should therefore key rerun decisions off expected model outputs,
not broad directory existence.

## Remaining gaps before production

### P1

1. There is still no automated integration gate that launches the real generated `job.sh` against a local LDNDC runtime.
   - The current unit and regression tests are materially better than before.
   - A release gate still needs one opt-in integration test or scripted audit target that validates the exact launcher path on macOS and Linux.

### P2

2. Local macOS execution is in scope.
   - `download.LDNDC()` handles the quarantine-removal step for downloaded macOS archives.
   - Linux execution still needs to be verified on a Linux host or CI runner.

3. `write.config.LDNDC` can now use settings-provided LDNDC support files, but the legacy template-generation fallback still contains species and module assumptions.
   - The getting-started path should use support files derived from the LDNDC example.
   - Broader unattended production use across additional ecosystems still needs explicit support files or coverage for each supported class.

## Production-readiness requirements

1. Add one automated integration target that covers `write.config.LDNDC` + generated `job.sh` + netCDF outputs using the same local-archive pattern validated in this audit.
2. Verify the generated run path on Linux.
3. Define the supported production scope for species/site types and add support-file fixtures or coverage for each supported class before broadening deployment claims.
