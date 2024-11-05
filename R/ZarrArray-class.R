### =========================================================================
### Zattr object
### -------------------------------------------------------------------------
###

# All of the below is designated to .zattrs handling,
# in order to guarantee that we can eventually produce
# a valid OME-Zarr store with the same specs as pySpatialData.
# Ideally, if OME specs change, only this part of the code will
# have to be adapted (with potentially version-specific methods).

# this class is defined solely for the purpose
# of validity checks & method dispatching...

#' @exportClass ZarrArray Zattrs
.Zattrs <- setClass(
  Class="Zattrs",
  contains="list")

#' @export
Zattrs <- \(x=list()) {
  y <- .Zattrs(x)
  if (length(x)) 
    attr(y, "names") <- names(x)
  return(y)
}

.validityZattrs <- \(object) {
  msg <- NULL
  # TODO: write specs checker so that we don't accidentally do
  # anything stupid & can eventually write out a valid file...
  
  ms <- "multiscales"
  ct <- "coordinateTransformations"
  df <- if (is.null(ms <- object[[ms]]))
    object[[ct]] else ms[[ct]][[1]]
  for (fd in split(df, seq_len(nrow(df)))) {
    stopifnot(
      is.character(fd$type), 
      fd$type %in% c(
        "identity", "sequence", "affine", 
        "scale", "rotate", "translation"),
      is.data.frame(ip <- fd$input), 
      is.data.frame(op <- fd$output),
      names(ip) == c("axes", "name"),
      names(op) == c("axes", "name"),
      is.character(ip$name), 
      is.character(op$name))
  }
  
  if (is.null(msg)) TRUE else msg
}

setValidity("Zattrs", .validityZattrs)

# set ----

setGeneric("zattrs", \(x, ...) standardGeneric("zattrs"))
setGeneric("zattrs<-", \(x, value) standardGeneric("zattrs<-"))

setReplaceMethod("zattrs", "Zattrs",
                 \(x, value) { x@zattrs <- value; x })

setReplaceMethod("zattrs", "list",
                 \(x, value) `zattrs<-`(x, Zattrs(value)))



### =========================================================================
### ZarrArray objects
### -------------------------------------------------------------------------
###
### Note that we could just wrap an ZarrArraySeed object in a DelayedArray
### object to represent and manipulate an HDF5 dataset as a DelayedArray
### object. So, strictly speaking, we don't really need the ZarrArray and
### ZarrMatrix classes. However, we define these classes mostly for cosmetic
### reasons, that is, to hide the DelayedArray and DelayedMatrix classes
### from the user. So the user will see and manipulate ZarrArray and
### ZarrMatrix objects instead of DelayedArray and DelayedMatrix objects.
###

#' @exportClass ZarrArray ZarrArray
.ZarrArray <- setClass(
  Class="ZarrArray",
   
  # can we replace 'Array' with DelayedArray here ?
  # contains=c("Array", "Annotated"),
  contains = c("DelayedArray", "Annotated"),
  
  # temporarily supporting pointers,
  # for the purpose of development...
  slots=c(seed="Array_OR_array_OR_df", zattrs="Zattrs")
  # slots=c(data="Array_OR_array_OR_df", zattrs="Zattrs")
)

#' @rdname ZarrArray
#' @importFrom S4Vectors metadata
#' @export
setMethod("metadata", "ZarrArray", function(x) {
  x@metadata
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### as.array()
###

as.array.ZarrArray <- function(x) {
  as.array(x@seed)
}

setMethod("as.array", "ZarrArray", as.array.ZarrArray)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

setMethod("DelayedArray", "ZarrArraySeed",
          function(seed) new_DelayedArray(seed, Class="ZarrArray")
)

### Works directly on an ZarrArraySeed object, in which case it must be
### called with a single argument.
ZarrArray <- function(data, name, as.sparse=FALSE, type=NA)
{
  if (is(data, "ZarrArraySeed")) {
    if (!(missing(name) &&
          identical(as.sparse, FALSE) &&
          identical(type, NA)))
      stop(wmsg("ZarrArray() must be called with a single argument ",
                "when passed an ZarrArraySeed object"))
    seed <- data
  } else if (is(data, "Array") || is(data, "array")) {
    seed <- data
  } else {
    seed <- ZarrArraySeed(filepath = data, name, as.sparse=as.sparse, type=type)
  }
  # .ZarrArray(data = DelayedArray(seed))
  .ZarrArray(seed = DelayedArray(seed))
}

setReplaceMethod("is_sparse", "ZarrArray",
                 function(x, value)
                 {
                   is_sparse(x@seed) <- value
                   # is_sparse(x@data) <- value
                   x
                 }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ZarrMatrix objects
###

setClass("ZarrMatrix", contains=c("ZarrArray", "DelayedMatrix"))

### Required for DelayedArray internal business.
setMethod("matrixClass", "ZarrArray", function(x) "ZarrMatrix")

### Automatic coercion method from ZarrArray to ZarrMatrix silently returns
### a broken object (unfortunately these dummy automatic coercion methods
### don't bother to validate the object they return). So we overwrite it.
setAs("ZarrArray", "ZarrMatrix", function(from) new("ZarrMatrix", from))

### The user should not be able to degrade an ZarrMatrix object to
### an ZarrArray object so 'as(x, "ZarrArray", strict=TRUE)' should
### fail or be a no-op when 'x' is an ZarrMatrix object. Making this
### coercion a no-op seems to be the easiest (and safest) way to go.
setAs("ZarrMatrix", "ZarrArray", function(from) from)  # no-op

setAs("ANY", "ZarrMatrix",
      function(from) as(as(from, "ZarrArray"), "ZarrMatrix")
)