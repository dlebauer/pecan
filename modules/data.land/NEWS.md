# PEcAn.data.land 1.8.1

* Dependency `datapack` is now optional. It is only used by `dataone_download()` (#3373).
* `soilgrids_soilC_extract()` no longer returns an empty dataframe when none of the queried locations are missing data. (#3409)
* Functions that name output folders from `settings$siteID` no longer assume IDs are numeric


# PEcAn.data.land 1.8.0

## Added

* New function `soilgrids_soilC_extract` retrieves soil C estimates with uncertainty from the ISRIC SoilGrids 250m data. (#3040, @Qianyuxuan)

## Fixed

* `gSSURGO.Query()` now always returns all the columns requested, even ones that are all NA. It also now always requires `mukeys` to be specified.
* Updated `gSSURGO.Query()` and `extract_soil_gssurgo()` to work again after formatting changes in the underlying gSSURGO API

## Removed

* `find.land()` has been removed. It is not used anywhere we know if, has apparently not been working for some time, and relied on the `maptools` package which is scheduled for retirement.
* Removed dependency on `PEcAn.data.atmosphere`, notably by retrieving site latitude and longitude directly from `PEcAn.DB::query.site` instead of custom lookups (#3300, Abhinav Pandey).


# PEcAn.data.land 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
