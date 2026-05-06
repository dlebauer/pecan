# PEcAn.LDNDC 1.0.3.9000

* Updated local LDNDC runtime launch and configuration generation for the
  current packaged binary layout.
* Fixed `model2netcdf.LDNDC()` to read the current heterotrophic respiration
  output column name.
* Added a runnable `met2model.LDNDC()` test using a temporary local fixture.
* Added a getting-started Quarto vignette for the LDNDC-specific PEcAn
  workflow.

# PEcAn.LDNDC 1.0.2.9000

* Removed `tests/Rcheck_reference.log`, which was used to ignore historic check messages that have now been fixed.



# PEcAn.LDNDC 1.0.2

* model2netcdf.LDNDC no longer writes separate `<year>.nc.var` files for every year of output. Use `PEcAn.utils::nc_write_varfiles()` to create these as needed.


# PEcAn.LDNDC 1.0.1

## License change
* PEcAn.LDNDC is now distributed under the BSD three-clause license instead of the NCSA Open Source license.


# PEcAn.LDNDC 1.0.0

First public release
