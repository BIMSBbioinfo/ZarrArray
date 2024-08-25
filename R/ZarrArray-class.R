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


setClass("ZarrArray",
         contains="DelayedArray",
         representation(seed="ZarrArraySeed")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

setMethod("DelayedArray", "ZarrArraySeed",
          function(seed) new_DelayedArray(seed, Class="ZarrArray")
)

### Works directly on an ZarrArraySeed object, in which case it must be
### called with a single argument.
ZarrArray <- function(filepath, name, as.sparse=FALSE, type=NA)
{
  if (is(filepath, "ZarrArraySeed")) {
    if (!(missing(name) &&
          identical(as.sparse, FALSE) &&
          identical(type, NA)))
      stop(wmsg("ZarrArray() must be called with a single argument ",
                "when passed an ZarrArraySeed object"))
    seed <- filepath
  } else {
    seed <- ZarrArraySeed(filepath, name, as.sparse=as.sparse, type=type)
  }
  DelayedArray(seed)
}

setReplaceMethod("is_sparse", "ZarrArray",
                 function(x, value)
                 {
                   is_sparse(x@seed) <- value
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show
###

setMethod("show",
          "ZarrArray",
          definition = function(object){
            cat(paste0("<", paste(object@seed@dim, collapse = " x "), "> ", class(object), " object of type '", object@seed@type, "':\n"))
          }
)
