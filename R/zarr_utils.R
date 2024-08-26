### =========================================================================
### Some low-level HDF5 utilities
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrexists()
###

zarrexists <- function(filepath, name)
{
  zarr.array <- pizzarr::zarr_open(store = filepath, mode = "r")
  grepl(".zarr$", filepath)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrisdataset()
###

zarrisdataset <- function(filepath, name)
{
  zarr.array <- pizzarr::zarr_open(store = filepath, mode = "r")
  did <- try(zarr.array$get_item(name))
  ans <- !inherits(did, "try-error")
  ans
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrdim() and zarrchunkdim()
###

zarrdim <- function(filepath, name)
{
  zarr.array <- pizzarr::zarr_open(store = filepath, mode = "r")
  zarrmat <- zarr.array$get_item(name)
  zarrmat$get_shape()
}

zarrchunkdim <- function(filepath, name, adjust=FALSE)
{
  zarr.array <- pizzarr::zarr_open(store = filepath, mode = "r")
  zarrmat <- zarr.array$get_item(name)
  chunkdim <- zarrmat$get_chunks()
  if (adjust) {
    dim <- zarrmat$get_shape()
    stopifnot(length(chunkdim) == length(dim))
    chunkdim <- as.integer(pmin(dim, chunkdim))
  }
  chunkdim
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### normarg_zarr_filepath() and normarg_zarr_name()
###

normarg_zarr_filepath <- function(path, what1="'filepath'", what2="the dataset")
{
  if (!isSingleString(path))
    stop(wmsg(what1, " must be a single string specifying the path ",
              "to the Zarr directory where ", what2, " is located"))
  tools::file_path_as_absolute(path)  # return absolute path in canonical form
}

normarg_zarr_name <- function(name, what1="'name'",
                            what2="the name of a dataset",
                            what3="")
{
  if (!isSingleString(name))
    stop(wmsg(what1, " must be a single string specifying ",
              what2, " in the Zarr directory", what3))
  if (name == "")
    stop(wmsg(what1, " cannot be the empty string"))
  if (substr(name, start=1L, stop=1L) == "/") {
    name <- sub("^/*", "/", name)  # only keep first leading slash
  } else {
    name <- paste0("/", name)
  }
  name
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Used in validity methods
###

### 'path' is expected to be the **absolute** path to a local zarr array.
validate_zarr_absolute_path <- function(path, what="'path'")
{
  if (!(isSingleString(path) && nzchar(path)))
    return(paste0(what, " must be a single non-empty string"))
  
  ## Check that 'path' points to an Zarr directory that is accessible.
  if (!file.exists(path))
    return(paste0(what, " (\"", path, "\") must be the path to ",
                  "an existing zarr array"))
  if (!grepl(".zarr$", path))
    return(paste0(what, " (\"", path, "\") doesn't seem to be ",
                  "the path to a valid zarr array"))
  if (path != tools::file_path_as_absolute(path))
    return(paste0(what, " (\"", path, "\") must be the absolute ",
                  "canonical path the Zarr array"))
  TRUE
}

validate_zarr_dataset_name <- function(path, name, what="'name'")
{
  if (!(isSingleString(name) && nzchar(name)))
    return(paste0(what, " must be a single non-empty string"))
  
  if (!zarrexists(path, name))
    return(paste0(what, " (\"", name, "\") doesn't exist ",
                  "in Zarr directory \"", path, "\""))
  if (!zarrisdataset(path, name))
    return(paste0(what, " (\"", name, "\") is not a dataset ",
                  "in Zarr directory \"", path, "\""))
  zarr_dim <- try(zarrdim(path, name), silent=TRUE)
  if (inherits(zarr_dim, "try-error"))
    return(paste0(what, " (\"", name, "\") is a dataset with ",
                  "no dimensions in Zarr directory \"", path, "\""))
  TRUE
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ZarrCreateDataset()
###

compute_max_string_size <- function(x)
{
  if (type(x) != "character")
    return(NULL)
  if (length(x) == 0L)
    return(0L)
  max(nchar(x, type="bytes", keepNA=FALSE))
}

ZarrCreateDataset <- function(filepath, zarrarray, name, dim, maxdim=dim,
                             type="double", H5type=NULL, size=NULL,
                             chunkdim=dim, level=6L)
{
  stopifnot(is.numeric(dim),
            is.numeric(maxdim), length(maxdim) == length(dim))
  if (!is.null(chunkdim)) {
    stopifnot(is.numeric(chunkdim), length(chunkdim) == length(dim))
    chunkdim <- pmin(chunkdim, maxdim)
  }
  zarrarray$create_dataset(name = name, shape = dim, chunks = chunkdim)
  # if (!ok)
  #   stop(wmsg("failed to create dataset '", name, "' ",
  #             "in file '", filepath, "'"), call.=FALSE)
}