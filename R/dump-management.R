### =========================================================================
### Zarr dump management
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### 2 global internal counters: one for the dump files, one for the dump
### names
###
### The 2 counters are safe to use in the context of parallel execution e.g.
###
###   library(BiocParallel)
###   bplapply(1:5, function(i) .get_dump_files_global_counter(increment=TRUE))
###   bplapply(1:5, function(i) .get_dump_names_global_counter(increment=TRUE))
###
### Jan 31, 2024: Hmm... actually they are not. They are when parallelizing
### via a MulticoreParam, but not via a SnowParam:
###
###   register(SnowParam(workers=2))
###   res <- bplapply(1:5, function(i) .get_dump_files_global_counter(TRUE))
###   unlist(res)
###   # [1] 1 2 3 1 2
###
### So we need to move away from this broken global counter thing.
### We actually started to do so in .get_dump_autofile() (see IMPORTANT
### CHANGE IN ZarrArray 1.31.4 below in this file). With this change,
### .get_dump_files_global_counter() is no longer used or needed.
### TODO: What about the .get_dump_names_global_counter_filepath()? Do we
### still need this or should we move away from this too?

#.get_dump_files_global_counter_filepath <- function()
#{
#    file.path(tempdir(), "ZarrArray_dump_files_global_counter")
#}

.get_dump_names_global_counter_filepath <- function()
{
  file.path(tempdir(), "ZarrArray_dump_names_global_counter")
}

### Called by .onLoad() hook (see zzz.R file).
#init_Zarr_dump_files_global_counter <- function()
#{
#    filepath <- .get_dump_files_global_counter_filepath()
#    init_global_counter(filepath)
#    filepath
#}

### Called by .onLoad() hook (see zzz.R file).
init_Zarr_dump_names_global_counter <- function()
{
  filepath <- .get_dump_names_global_counter_filepath()
  init_global_counter(filepath)
  filepath
}

#.get_dump_files_global_counter <- function(increment=FALSE)
#{
#    filepath <- .get_dump_files_global_counter_filepath()
#    get_global_counter(filepath, increment=increment)
#}

.get_dump_names_global_counter <- function(increment=FALSE)
{
  filepath <- .get_dump_names_global_counter_filepath()
  get_global_counter(filepath, increment=increment)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Normalization (with basic checking) of an Zarr file path or dataset name
###

### Return the *absolute path* to the dump file.
### Has the side effect of creating the file as an empty Zarr file if it does
### not exist yet.
normalize_dump_filepath <- function(filepath)
{
  if (!isSingleString(filepath) || filepath == "")
    stop(wmsg("'filepath' must be a non-empty string specifying the ",
              "path to a new or existing Zarr file"))
  if (!file.exists(filepath))
    pizzarr::zarr_open(store = filepath)
  tools::file_path_as_absolute(filepath)
}

normalize_dump_name <- function(name)
{
  if (!isSingleString(name) || name == "")
    stop(wmsg("'name' must be a non-empty string specifying the name ",
              "of the Zarr dataset to write"))
  trim_trailing_slashes(name)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Very low-level stuff used in this file only
###

.set_ZarrArray_global_option <- function(name, value)
{
  stopifnot(isSingleString(name))
  ZarrArray_options <- getOption("ZarrArray", default=list())
  ZarrArray_options[[name]] <- value
  options(ZarrArray=ZarrArray_options)
  invisible(value)
}

.get_ZarrArray_global_option <- function(name)
{
  stopifnot(isSingleString(name))
  getOption("ZarrArray", default=list())[[name]]
}

ZarrArray_global_option_is_set <- function(name)
{
  stopifnot(isSingleString(name))
  name %in% names(getOption("ZarrArray", default=list()))
}

### Create directory 'dir' if it doesn't exist yet.
.set_dump_dir <- function(dir)
{
  ## Even though file_path_as_absolute() will trim the trailing slashes,
  ## we need to do this early. Otherwise, checking for the existence of a
  ## file of the same name as the to-be-created directory will fail.
  if (nchar(dir) > 1L)
    dir <- trim_trailing_slashes(dir)
  if (!dir.exists(dir)) {
    if (file.exists(dir))
      stop(wmsg("\"", dir, "\" already exists and is a file, ",
                "not a directory"))
    if (!suppressWarnings(dir.create(dir)))
      stop("cannot create directory \"", dir, "\"")
  }
  dir <- tools::file_path_as_absolute(dir)
  .set_ZarrArray_global_option("dump.dir", dir)
}

.set_dump_autofiles_mode <- function()
{
  .set_ZarrArray_global_option("dump.specfile", NULL)
}

### Create file as an empty Zarr file if it doesn't exist yet.
.set_dump_specfile <- function(filepath)
{
  filepath <- normalize_dump_filepath(filepath)
  .set_ZarrArray_global_option("dump.specfile", filepath)
}

.set_dump_autonames_mode <- function()
{
  .set_ZarrArray_global_option("dump.specname", NULL)
}

.set_dump_specname <- function(name)
{
  .set_ZarrArray_global_option("dump.specname", name)
}

### Return the user-specified file of the dump or an error if the user didn't
### specify a file.
.get_dump_specfile <- function()
{
  .get_ZarrArray_global_option("dump.specfile")
}

.get_dump_autoname <- function(increment=FALSE)
{
  counter <- .get_dump_names_global_counter(increment=increment)
  sprintf("/ZarrArrayAUTO%05d", counter)
}

### Return the user-specified name of the dump or an error if the user didn't
### specify a name.
.get_dump_specname <- function()
{
  .get_ZarrArray_global_option("dump.specname")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### get/setZarrDumpDir()
###

getZarrDumpDir <- function()
{
  .get_ZarrArray_global_option("dump.dir")
}

.get_dump_autofile <- function()
{
  ## Replacing the global counter approach used in ZarrArray < 1.31.4
  ## with the tempfile() approach.
  #counter <- .get_dump_files_global_counter(increment=increment)
  #filepath <- file.path(getZarrDumpDir(), sprintf("auto%05d.h5", counter))
  filepath <- tempfile(pattern="auto", tmpdir=getZarrDumpDir(), fileext=".h5")
  if (!file.exists(filepath))
    h5createFile(filepath)
  filepath
}

### Called by .onLoad() hook (see zzz.R file).
setZarrDumpDir <- function(dir)
{
  if (missing(dir)) {
    dir <- file.path(tempdir(), "ZarrArray_dump")
  } else if (!isSingleString(dir) || dir == "") {
    stop(wmsg("'dir' must be a non-empty string specifying the path ",
              "to a new or existing directory"))
  }
  dir <- .set_dump_dir(dir)
  .set_dump_autofiles_mode()
  #.get_dump_autofile()
  invisible(dir)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### set/getZarrDumpFile()
###

### Set the current Zarr dump file. Create it as an empty Zarr file if it
### doesn't exist yet.
setZarrDumpFile <- function(filepath)
{
  if (missing(filepath)) {
    .set_dump_autofiles_mode()
    filepath <- .get_dump_autofile()
  } else {
    if (!isSingleString(filepath) || filepath == "")
      stop("'filepath' must be a non-empty string")
    if (has_trailing_slash(filepath)) {
      setZarrDumpDir(filepath)
      filepath <- .get_dump_autofile()
    } else {
      filepath <- .set_dump_specfile(filepath)
    }
  }
  file_content <- h5ls(filepath)
  if (nrow(file_content) == 0L)
    return(invisible(file_content))
  file_content
}

### Return the *absolute path* to the dump file.
getZarrDumpFile <- function()
{
  filepath <- .get_dump_specfile()
  if (is.null(filepath))
    filepath <- .get_dump_autofile()
  filepath
}

### A convenience wrapper.
lsZarrDumpFile <- function() h5ls(getZarrDumpFile())


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### set/getZarrDumpName()
###

setZarrDumpName <- function(name)
{
  if (missing(name)) {
    .set_dump_autonames_mode()
    name <- .get_dump_autoname()
    return(invisible(name))
  }
  name <- normalize_dump_name(name)
  .set_dump_specname(name)
}

getZarrDumpName <- function(for.use=FALSE)
{
  if (!isTRUEorFALSE(for.use))
    stop("'for.use' must be TRUE or FALSE")
  name <- .get_dump_specname()
  if (is.null(name)) {
    name <- .get_dump_autoname(increment=for.use)
  } else if (for.use) {
    ## If the dump file is a user-specified file, we switch back to
    ## automatic dump names.
    filepath <- .get_dump_specfile()
    if (!is.null(filepath))
      .set_dump_autonames_mode()
  }
  name
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### set/getZarrDumpChunkLength() and set/getZarrDumpChunkShape()
###

### Called by .onLoad() hook (see zzz.R file).
setZarrDumpChunkLength <- function(length=1000000L)
{
  .set_ZarrArray_global_option("dump.chunk.length", length)
}

getZarrDumpChunkLength <- function()
{
  .get_ZarrArray_global_option("dump.chunk.length")
}

### Called by .onLoad() hook (see zzz.R file).
setZarrDumpChunkShape <- function(shape="scale")
{
  .set_ZarrArray_global_option("dump.chunk.shape", shape)
}

getZarrDumpChunkShape <- function()
{
  .get_ZarrArray_global_option("dump.chunk.shape")
}

### TODO: Replace 'dim' argument with 'x'. This will allow the default
### **write** chunk dim to be set to the **read** chunk dim (i.e. to
### 'chunkdim(x)') if this is known (i.e. if 'chunkdim(x)' is not NULL).
getZarrDumpChunkDim <- function(dim)
{
  chunk_len <- getZarrDumpChunkLength()
  chunk_shape <- getZarrDumpChunkShape()
  makeCappedVolumeBox(chunk_len, dim, chunk_shape)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### set/getZarrDumpCompressionLevel
###

normalize_compression_level <- function(level)
{
  if (!isSingleNumber(level))
    stop("'level' must be a single number")
  if (!is.integer(level))
    level <- as.integer(level)
  if (level < 0L || level > 9L)
    stop(wmsg("'level' must be between 0 (no compression) ",
              "and 9 (highest and slowest compression)"))
  level
}

### Called by .onLoad() hook (see zzz.R file).
setZarrDumpCompressionLevel <- function(level=6L)
{
  level <- normalize_compression_level(level)
  .set_ZarrArray_global_option("dump.compression.level", level)
}

getZarrDumpCompressionLevel <- function()
{
  .get_ZarrArray_global_option("dump.compression.level")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Dump log
###

### Called by .onLoad() hook (see zzz.R file).
get_Zarr_dump_logfile <- function()
{
  file.path(tempdir(), "ZarrArray_dump_log")
}

.get_dataset_creation_global_counter_filepath <- function()
{
  file.path(tempdir(), "ZarrArray_dataset_creation_global_counter")
}

### Called by .onLoad() hook (see zzz.R file).
init_Zarr_dataset_creation_global_counter <- function()
{
  filepath <- .get_dataset_creation_global_counter_filepath()
  init_global_counter(filepath)
  filepath
}

.get_dataset_creation_global_counter <- function(increment=FALSE)
{
  filepath <- .get_dataset_creation_global_counter_filepath()
  get_global_counter(filepath, increment=increment)
}

### Use a lock mechanism so is safe to use in the context of parallel
### execution.
appendDatasetCreationToZarrDumpLog <- function(filepath, name, dim, type,
                                               chunkdim, level)
{
  logfile <- get_Zarr_dump_logfile()
  locked_path <- lock_file(logfile)
  on.exit(unlock_file(logfile))
  counter <- .get_dataset_creation_global_counter(increment=TRUE)
  dims <- paste0(dim, collapse="x")
  chunkdims <- paste0(chunkdim, collapse="x")
  cat(as.character(Sys.time()), counter,
      filepath, name, dims, type, chunkdims, level,
      sep="\t", file=locked_path, append=TRUE)
  cat("\n", file=locked_path, append=TRUE)
}

showZarrDumpLog <- function()
{
  COLNAMES <- c("time", "counter",
                "filepath", "name", "dims", "type", "chunkdims", "level")
  ## The nb of lines in the log file is the current value of the dataset
  ## creation counter minus one.
  counter <- .get_dataset_creation_global_counter()
  if (counter == 1L) {
    dump_log <- data.frame(time=character(0),
                           counter=integer(0),
                           filepath=character(0),
                           name=character(0),
                           dims=character(0),
                           type=character(0),
                           chunkdims=character(0),
                           level=integer(0),
                           stringsAsFactors=FALSE)
  } else {
    logfile <- get_Zarr_dump_logfile()
    locked_path <- lock_file(logfile)
    on.exit(unlock_file(logfile))
    dump_log <- read.table(locked_path,
                           sep="\t", stringsAsFactors=FALSE)
    colnames(dump_log) <- COLNAMES
    fmt <- "[%s] #%d In file '%s': creation of dataset '%s' (%s:%s, chunkdims=%s, level=%d)"
    message(paste0(sprintf(fmt,
                           dump_log$time, dump_log$counter,
                           dump_log$filepath, dump_log$name,
                           dump_log$dims, dump_log$type,
                           dump_log$chunkdims, dump_log$level),
                   "\n"),
            appendLF=FALSE)
  }
  invisible(dump_log)
}

create_and_log_Zarr_dataset <- function(filepath, name, dim, maxdim=dim,
                                        type="double", H5type=NULL, size=NULL,
                                        chunkdim=dim, level=6L)
{
  zarr_array <- pizzarr::zarr_open(store = filepath, mode = "w")
  ZarrCreateDataset(filepath, zarr_array, name, dim, maxdim=maxdim,
                   type=type, H5type=H5type, size=size,
                   chunkdim=chunkdim, level=level)
  # TODO: appendDatasetCreationToZarrDumpLog? ####
  # appendDatasetCreationToZarrDumpLog(filepath, name, dim,
  #                                    type, chunkdim, level)
}