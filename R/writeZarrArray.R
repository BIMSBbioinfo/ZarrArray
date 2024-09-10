### =========================================================================
### writeZarrArray()
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### ZarrRealizationSink objects
###
### The ZarrRealizationSink class is a concrete RealizationSink subclass that
### implements an ZarrArray realization sink.
###

setClass("ZarrRealizationSink",
         contains="RealizationSink",
         representation(
           ## Slots that support the RealizationSink constructor contract.
           dim="integer",              # Naming this slot "dim" makes dim() work
           # out of the box.
           dimnames="list",
           type="character",           # Single string.
           as_sparse="logical",        # TRUE or FALSE.
           
           ## Other slots.
           filepath="character",       # Single string.
           name="character",           # Dataset name.
           chunkdim="integer_OR_NULL"  # An integer vector parallel to the 'dim'
           # slot or NULL.
         )
)

setMethod("dimnames", "ZarrRealizationSink",
          function(x)
          {
            ans <- x@dimnames
            if (all(S4Vectors:::sapply_isNULL(ans)))
              return(NULL)
            ans
          }
)

setMethod("type", "ZarrRealizationSink", function(x) x@type)

setMethod("chunkdim", "ZarrRealizationSink", function(x) x@chunkdim)

setMethod("is_sparse", "ZarrRealizationSink", function(x) x@as_sparse)

.normarg_chunkdim <- function(chunkdim, dim)
{
  if (!(is.numeric(chunkdim) || is.logical(chunkdim) && all(is.na(chunkdim))))
    stop(wmsg("'chunkdim' must be NULL or an integer vector"))
  if (!is.integer(chunkdim))
    chunkdim <- as.integer(chunkdim)
  if (length(chunkdim) != length(dim))
    stop(wmsg("'chunkdim' must be an integer vector of length ",
              "the number of dimensions of the object to write"))
  if (!all(chunkdim <= dim, na.rm=TRUE))
    stop(wmsg("the chunk dimensions specified in 'chunkdim' exceed ",
              "the dimensions of the object to write"))
  if (any(chunkdim == 0L & dim != 0L, na.rm=TRUE))
    stop(wmsg("'chunkdim' must contain nonzero values unless ",
              "the zero values correspond to dimensions in the ",
              "object to write that are also zero"))
  na_idx <- which(is.na(chunkdim))
  chunkdim[na_idx] <- dim[na_idx]
  if (prod(chunkdim) > .Machine$integer.max)
    stop(wmsg("The chunk dimensions in 'chunkdim' are too big. The ",
              "product of the chunk dimensions should always be <= ",
              ".Machine$integer.max"))
  chunkdim
}

ZarrRealizationSink <- function(dim, 
                                dimnames=NULL, 
                                type="double",
                                as.sparse=FALSE,
                                filepath=NULL, name=NULL,
                                H5type=NULL, size=NULL,
                                chunkdim=NULL, level=NULL)
{
  if (!isTRUEorFALSE(as.sparse))
    stop(wmsg("'as.sparse' must be TRUE or FALSE"))
  if (is.null(filepath)) {
    filepath <- getZarrDumpFile()
  } else {
    filepath <- normalize_dump_filepath(filepath)
  }
  if (is.null(name)) {
    name <- getZarrDumpName(for.use=TRUE)
  } else {
    name <- normalize_dump_name(name)
  }
  if (is.null(chunkdim)) {
    chunkdim <- getZarrDumpChunkDim(dim)
  } else if (isSingleNumber(chunkdim) && chunkdim == 0) {
    chunkdim <- NULL  # no chunking
  } else {
    chunkdim <- .normarg_chunkdim(chunkdim, dim)
  }
  # if (is.null(level)) {
  #   if (is.null(chunkdim)) {
  #     level <- 0L
  #   } else {
  #     level <- getZarrDumpCompressionLevel()
  #   }
  # } else {
  #   level <- normalize_compression_level(level)
  # }
  create_and_log_Zarr_dataset(filepath, name, dim,
                              type=type, H5type=H5type, size=size,
                              chunkdim=chunkdim, level=level)
  if (is.null(dimnames)) {
    dimnames <- vector("list", length(dim))
  } else {
    zarrwriteDimnames(dimnames, filepath, name)
  }
  new2("ZarrRealizationSink", 
       dim=dim, 
       dimnames=dimnames, 
       type=type,
       as_sparse=as.sparse,
       filepath=filepath, 
       name=name,
       chunkdim=chunkdim)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Writing data to an ZarrRealizationSink object
###

setMethod("write_block", "ZarrRealizationSink",
          function(sink, viewport, block)
          {
            if (!is.array(block))
              block <- as.array(block)
            if(sink@type == "character"){
              sink_type <- "<U20" 
            } else{
              sink_type <- NA
            }
            zarrarray <- pizzarr::zarr_open_array(store = sink@filepath, 
                                                  path = sink@name, 
                                                  shape = dim(block),
                                                  mode = "w", 
                                                  dtype = sink_type)
            zarrarray$set_item("...", block)
            sink
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercing an ZarrRealizationSink object
###

setAs("ZarrRealizationSink", "ZarrArraySeed",
      function(from) ZarrArraySeed(from@filepath, from@name,
                                   as.sparse=from@as_sparse)
)

setAs("ZarrRealizationSink", "ZarrArray",
      function(from) DelayedArray(as(from, "ZarrArraySeed"))
)

setAs("ZarrRealizationSink", "DelayedArray",
      function(from) DelayedArray(as(from, "ZarrArraySeed"))
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### writeZarrArray()
###

### If 'filepath' and 'name' are NULL (the default), write the dataset to
### the current dump.
### If 'chunkdim' is NULL, an automatic chunk geometry will be used.
### To write "unchunked data" (a.k.a. contiguous data), 'chunkdim' must be
### set to 0.
### Return an ZarrArray object pointing to the newly written Zarr dataset
### on disk.
writeZarrArray <- function(x, filepath=NULL, name=NULL,
                           H5type=NULL, chunkdim=NULL, level=NULL,
                           as.sparse=NA,
                           with.dimnames=TRUE, verbose=NA)
{
  if (!(is.logical(as.sparse) && length(as.sparse) == 1L))
    stop(wmsg("'as.sparse' must be NA, TRUE or FALSE"))
  if (!isTRUEorFALSE(with.dimnames))
    stop("'with.dimnames' must be TRUE or FALSE")
  verbose <- DelayedArray:::normarg_verbose(verbose)
  
  if (is.na(as.sparse))
    as.sparse <- is_sparse(x)
  sink_dimnames <- if (with.dimnames) dimnames(x) else NULL
  ## compute_max_string_size() will trigger block processing if 'x' is a
  ## DelayedArray object of type "character", so it could take a while.
  size <- compute_max_string_size(x)
  sink <- ZarrRealizationSink(dim(x), sink_dimnames, type(x), as.sparse,
                              filepath=filepath, name=name,
                              H5type=H5type, size=size,
                              chunkdim=chunkdim, level=level)
  sink <- BLOCK_write_to_sink(sink, x, verbose=verbose)
  as(sink, "ZarrArray")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion to ZarrArray
###
### The methods below write the object to disk. Note that coercion from
### ZarrRealizationSink to ZarrArray is already taken care of by the specific
### method above and doesn't write anything to disk. So coercing to ZarrArray
### in general writes the object to disk *except* when the object to coerce is
### an ZarrRealizationSink object.
###

### Write to current dump.
.as_ZarrArray <- function(from) writeZarrArray(from)

setAs("ANY", "ZarrArray", .as_ZarrArray)

### Automatic coercion methods from DelayedArray to ZarrArray and from
### DelayedMatrix to ZarrMatrix silently return broken objects (unfortunately
### these dummy automatic coercion methods don't bother to validate the object
### they return). So we overwrite them.
setAs("DelayedArray", "ZarrArray", .as_ZarrArray)
setAs("DelayedMatrix", "ZarrMatrix", .as_ZarrArray)