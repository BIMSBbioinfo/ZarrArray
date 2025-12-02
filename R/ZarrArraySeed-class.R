### =========================================================================
### ZarrArraySeed objects
### -------------------------------------------------------------------------

#' @exportClass ZarrArraySeed ZarrArray
.ZarrArraySeed <- setClass("ZarrArraySeed",
         contains="Array",
         representation(
           ## ----------------- user supplied slots -----------------
           
           ## **absolute** path to a local Zarr directory so the
           ## object won't break when the user changes the working directory
           ## (e.g. with setwd()). The path must also be in its canonical
           ## form so comparing paths from different objects is meaningful
           ## (required by quickResaveHDF5SummarizedExperiment()).
           filepath="character",
           
           ## Name of dataset in the Zarr directory.
           name="character",
           
           ## Whether the Zarr dataset should be considered sparse (and treated
           ## as such) or not. Slot added in ZarrArray 1.17.8.
           as_sparse="logical",  # TRUE or FALSE
           
           ## NA or the desired type. Slot added in ZarrArray 1.15.6.
           type="character",
           
           ## ------------ automatically populated slots ------------
           
           dim="integer",
           chunkdim="integer_OR_NULL",
           first_val="ANY"  # first value in the dataset
         ),
         prototype(
           as_sparse=FALSE,
           type=NA_character_
         )
)

# class union taken from HelenaLC/SpatialData
setClassUnion(
  "Array_OR_array_OR_df",
  c("Array", "data.frame", "ZarrArraySeed")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

### Check that 'x' points to an Zarr dataset that has the expected dimensions
### and chunk dimensions.
validate_ZarrArraySeed_dataset_geometry <- function(x, what="object")
{
  zarr_dim <- zarrdim(x@filepath, x@name)
  if (!identical(zarr_dim, x@dim))
    return(paste0(what, " points to a Zarr dataset (\"", x@name, "\") ",
                  "in Zarr array \"", x@filepath, "\" ",
                  "that does not have the expected dimensions"))
  TRUE
}

.validate_ZarrArraySeed <- function(x)
{
  ## 'filepath' and 'name' slots.
  x_filepath <- x@filepath
  x_name <- x@name
  
  # validate path and name
  msg <- validate_zarr_absolute_path(x_filepath, "'filepath' slot")
  if (!isTRUE(msg))
    return(msg)
  msg <- validate_zarr_dataset_name(x_filepath, x_name, "'name' slot")
  if (!isTRUE(msg))
    return(msg)
  
  ## 'as_sparse' slot.
  x_as_sparse <- x@as_sparse
  if (!isTRUEorFALSE(x_as_sparse))
    return("'as_sparse' slot must be TRUE or FALSE")
  
  ## 'dim' slot.
  msg <- S4Arrays:::validate_dim_slot(x, "dim")
  if (!isTRUE(msg))
    return(msg)
  
  ## 'chunkdim' slot.
  x_chunkdim <- x@chunkdim
  if (!is.null(x_chunkdim)) {
    msg <- S4Arrays:::validate_dim_slot(x, "chunkdim")
    if (!isTRUE(msg))
      return(msg)
  }
  
  # if (!is(x_filepath, "H5File")) {
  #   ## Check that the dataset has the expected dimensions and
  #   ## chunk dimensions.
  #   msg <- validate_ZarrArraySeed_dataset_geometry(x)
  #   if (!isTRUE(msg))
  #     return(msg)
  # }
  
  ## Check that the dimnames stored in the file are consistent with
  ## the dimensions of the Zarr dataset.
  # msg <- validate_lengths_of_zarrdimnames(x_filepath, x_name)
  # if (!isTRUE(msg))
  #   return(msg)
  
  TRUE
}

setValidity2("ZarrArraySeed", .validate_ZarrArraySeed)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### path() getter/setter
###

### Does NOT access the file.
setMethod("path", "ZarrArraySeed",
          function(object)
          {
            object@filepath
          }
)

### Return a fake value (of the correct type) if the dataset is empty i.e.
### if at least one of its dimensions is 0.
.read_zarrdataset_first_val <- function(filepath, name, dim)
{
  if (any(dim == 0L)) {
    # type <- get_zarrmread_returned_type(filepath, name)
    # val <- vector(type, 1L)  # fake value
    vector("double",1L)
  } else {
    index <- rep.int(list(1L), length(dim))
    val <- zarr_mread(filepath, name, index, as.vector=TRUE)
    stopifnot(length(val) == 1L)  # sanity check
  }
  val
}

# TODO: update the path replace method for ZarrArraySeed
setReplaceMethod("path", "ZarrArraySeed",
                 function(object, value)
                 {
                   new_filepath <- normarg_zarr_filepath(value,
                                                         what1="the supplied path",
                                                         what2="the Zarr dataset")
                   ## Check dim compatibility.
                   new_dim <- zarrdim(new_filepath, object@name)
                   object_dim <- object@dim
                   if (!identical(new_dim, object_dim)) {
                     new_dim_in1string <- paste0(new_dim, collapse=" x ")
                     dim_in1string <- paste0(object_dim, collapse=" x ")
                     stop(wmsg("dimensions (", new_dim_in1string, ") ",
                               "of Zarr dataset '", object@name, "' ",
                               "from file '", value, "' are not ",
                               "as expected (", dim_in1string, ")"))
                   }
                   
                   ## Check first val compatibility.
                   new_first_val <- .read_zarrdataset_first_val(new_filepath,
                                                                object@name,
                                                                object_dim)
                   if (!identical(new_first_val, object@first_val))
                     stop(wmsg("first value in Zarr dataset '", object@name, "' ",
                               "from file '", value, "' is not as expected"))
                   
                   ## Set new path.
                   object@filepath <- new_filepath
                   object
                 }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### type() getter
###

### Override the default method (defined in the DelayedArray package) with
### a much faster one.
setMethod("type", "ZarrArraySeed",
          function(x)
          {
            ## Prior to ZarrArray 1.15.6 ZarrArraySeed objects didn't have
            ## the "type" slot.
            if (!.hasSlot(x, "type"))
              return(type(x@first_val))
            type <- x@type
            # if (is.na(type))
            #   type <- get_zarrmread_returned_type(x@filepath, x@name)
            type
          }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dim() getter
###

### Does NOT access the file.
setMethod("dim", "ZarrArraySeed", function(x) x@dim)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### dimnames() getter
###

### Does access the file!
setMethod("dimnames", "ZarrArraySeed",
          function(x) {
            zarrreadDimnames(x@filepath, x@name, as.character=TRUE)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### extract_array()
###

.extract_array_from_ZarrArraySeed <- function(x, index)
{
  # check indices
  as_int <- !is.na(x@type) && x@type == "integer"
  
  # open zarr
  zarr.array <- pizzarr::zarr_open(store = x@filepath, mode = "r")
  zarrmat <- zarr.array$get_item(x@name)

  # create slices
  ind <- mapply(function(x,y){
    if(is.null(x)){
      return(pizzarr::slice(1,y))
    } else if(length(x) == 0){
      return(pizzarr::slice(0,0))
    } else if(length(x) == 1){
      return(pizzarr::slice(x,x))
    } else if(length(x) > 1){
      return(pizzarr::int(x))
    }
  }, index, zarrmat$get_shape(), SIMPLIFY = FALSE)
  
  # get zarr values given slices or indices
  ans <- zarrmat$get_orthogonal_selection(ind)$data
  ans
}

# check if indices are sequential
is.sequential <- function(index){
  check_list <- lapply(index, function(x){
    if(length(x) > 0 && !is.null(x)){
      tmp <- x[-1] - x[1:(length(x)-1)]
      return(all(tmp == 1))
    } else {
      return(TRUE)
    }
  })
  all(check_list)
}

setMethod("extract_array", "ZarrArraySeed", .extract_array_from_ZarrArraySeed)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### chunkdim() getter
###

### Does NOT access the file.
setMethod("chunkdim", "ZarrArraySeed", function(x) x@chunkdim)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

ZarrArraySeed <- function(filepath, name, as.sparse=FALSE, type=NA)
{
  name <- normarg_zarr_name(name)
  
  ## Check 'as.sparse'.
  if (!isTRUEorFALSE(as.sparse))
    stop(wmsg("'as.sparse' must be TRUE or FALSE"))
  
  ## Check 'type'
  if (!isSingleStringOrNA(type))
    stop(wmsg("'type' must be a single string or NA"))
  if (is.na(type)) {
    type <- as.character(type)
  } else if (type != "list") {
    tmp <- try(vector(type), silent=TRUE)
    if (inherits(tmp, "try-error") || !is.atomic(tmp))
      stop(wmsg("'type' must be an R atomic type ",
                "(e.g. \"integer\") or \"list\""))
  }
  
  # get zarr
  zarr.array <- pizzarr::zarr_open(store = filepath, mode = "r")
  
  # get attributes
  dim <- zarr.array$get_item(name)$get_shape()
  chunkdim <- zarrchunkdim(filepath, name, adjust=TRUE)
  first_val <- .read_zarrdataset_first_val(filepath, name, dim)
  
  new2("ZarrArraySeed", 
       filepath=filepath,
       name=name,
       as_sparse=as.sparse,
       type=type,
       dim=dim,
       chunkdim=chunkdim,
       first_val = first_val)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### as.array()
###

# as.array.ZarrArraySeed <- function(x) {
# 
#   # open zarr
#   zarr.array <- pizzarr::zarr_open(store = x@filepath, mode = "r")
#   zarrmat <- zarr.array$get_item(x@name)
# 
#   # return
#   as.array(zarrmat$get_item("...")$data)
# }

#' #' @rdname ZarrArraySeed
#' #' @export
#' setMethod("as.array", "ZarrArraySeed", as.array.ZarrArraySeed)