### =========================================================================
### Low-level manipulation of HDF5 Dimension Scale datasets
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrisdimscale()
###

zarrisdimscale <- function(filepath, name)
{
  .Call2("C_zarrisdimscale", filepath, name, PACKAGE="HDF5Array")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrgetdimscales() / zarrsetdimscales()
###

### Retrieve the names of the existing HDF5 datasets (1 per dimension in
### dataset 'name') currently attached along the dimensions of dataset 'name'
### for Dimension Scale 'scalename'.
zarrgetdimscales <- function(filepath, name, scalename=NA)
{
  stopifnot(isSingleStringOrNA(scalename))
  scalename <- as.character(scalename)
  .Call2("C_zarrgetdimscales", filepath, name, scalename,
         PACKAGE="HDF5Array")
}

### name:      The name of the dataset on which to set Dimension Scales.
### dimscales: A character vector containing the names of the existing HDF5
###            datasets (1 per dimension in dataset 'name') to attach along
###            the dimensions of dataset 'name'. NAs are allowed and, if
###            present, nothing gets attached along the corresponding
###            dimensions.
### scalename: The name of the Dimension Scale (analog to the name of an
###            attribute in R).
zarrsetdimscales <- function(filepath, name, dimscales, scalename=NA,
                           dry.run=FALSE)
{
  stopifnot(isSingleStringOrNA(scalename),
            is.character(dimscales),
            isTRUEorFALSE(dry.run))
  scalename <- as.character(scalename)
  .Call2("C_zarrsetdimscales", filepath, name, dimscales, scalename, dry.run,
         PACKAGE="HDF5Array")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Get/set the "dimension labels" of an HDF5 dataset
###
### The "dimension labels" the HDF5 equivalent of the names on 'dimnames(a)'
### in R.
###

zarrgetdimlabels <- function(filepath, name)
{
  .Call2("C_zarrgetdimlabels", filepath, name, PACKAGE="HDF5Array")
}

zarrsetdimlabels <- function(filepath, name, dimlabels)
{
  stopifnot(is.character(dimlabels))
  invisible(.Call2("C_zarrsetdimlabels", filepath, name, dimlabels,
                   PACKAGE="HDF5Array"))
}