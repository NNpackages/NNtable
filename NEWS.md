# NNtable (development version)

# NNtable 0.0.6

## Features

* As standard footnotes are now removed from empty outputs. Use `addWrapping()` to circumvent.  

## Bugfixes

* 431616 When `addFilling()` was used together with `addExposure()` the filled column would be added to the exposure table even though the column was not present in the original data.

* 465486 When `addTransWide()` was used to create empty columns within a nesting all would be removed. Now only the empty columns will be removed.

# NNtable 0.0.5

## Bugfixes

* 388785 When NA is used in subcategories they are now treated as blanks as expected

# NNtable 0.0.4

## Features

Exported a subsetting function for an NNTable object. It is now possible to subset to e.g. females by NNTable[SEX = "F"], check out the functionality here `'[.NNTable'()`

When `addTransWide()` produces rows that are not uniquely defined by other variables, all rows are now printed.

* Feature request 302334

    + The function `addTranslate()` have been added to NNtable to make it easy to change values. This could for instance be for SEX where "Male" should replace "M" and "Female" should replace "F"

* Feature request 285820
    
    + The function `addPageSplit()` have been added to NNtable to make it possible to split pages that are too wide to fit one page
 
## Bugfixes 
    
* 336364 addFormat fails when used together with addExposure and first column is not mentioned in addFormat
* 340549 addUnderScore messes up when addTranspose is used on columns with non-similar subcolumns
* 342059 addOrder fails when a space exists in the name of a column that is not used in either grouped columns or transposed
* 343559 NNTable fails when column names have more than three matches in data
* 343919 NNtable fails when only one column is formatted alone and others are formatted jointly
    
# NNtable 0.0.3

## Features

* Ability to print multiple pages with one call to, e.g. `print.NNTable(table, page = 1:5)` or `print.NNTable(table, page = "all")`

* Feture request 332974

    + Per default duplicated values of grouped columns are removed from the stub

## Bugfixes 

* 285824 When adding nested columns for tables with columns that already includes line shift it breaks on top
* 319823 addExposure interferes with addFilling
* 319830 addExposure interferes with formats from addFormat
* 319832 ddFilling duplicates data if addGroupedColumns is not used and table is sorted on a column that is transposed to wide

# NNtable 0.0.2

* Split the functions into several smaller files, such that functionality is kept together

# NNtable 0.0.1

* Added a `NEWS.md` file to track changes to the package.
