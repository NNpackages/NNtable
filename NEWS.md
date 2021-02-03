# NNtable (development version)

* bugfixes 

    + 336364 addFormat fails when used together with addExposure and first column is not mentioned in addFormat

# NNtable 0.0.3

* Ability to print multiple pages with one call to, e.g. `print.NNTable(table, page = 1:5)` or `print.NNTable(table, page = "all")`

* Feture request 332974

    + Per default duplicated values of grouped columns are removed from the stub

* bugfixes 

    + 285824 When adding nested columns for tables with columns that already includes line shift it breaks on top
    + 319823 addExposure interferes with addFilling
    + 319830 addExposure interferes with formats from addFormat
    + 319832 ddFilling duplicates data if addGroupedColumns is not used and table is sorted on a column that is transposed to wide

# NNtable 0.0.2

* Split the functions into several smaller files, such that functionality is kept together

# NNtable 0.0.1

* Added a `NEWS.md` file to track changes to the package.
