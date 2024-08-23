### Placeholders, initialized in .onLoad()
#.DUMP_FILES_GLOBAL_COUNTER_FILEPATH <- NULL
.DUMP_NAMES_GLOBAL_COUNTER_FILEPATH <- NULL
.DATASET_CREATION_GLOBAL_COUNTER_FILEPATH <- NULL

# .onLoad <- function(libname, pkgname)
# {
#   #.DUMP_FILES_GLOBAL_COUNTER_FILEPATH <<-
#   #    init_Zarr_dump_files_global_counter()
#   .DUMP_NAMES_GLOBAL_COUNTER_FILEPATH <<-
#     init_Zarr_dump_names_global_counter()
#   if (!ZarrArray_global_option_is_set("dump.dir"))
#     setZarrDumpDir()
#   if (!ZarrArray_global_option_is_set("dump.chunk.length"))
#     setZarrDumpChunkLength()
#   if (!ZarrArray_global_option_is_set("dump.chunk.shape"))
#     setZarrDumpChunkShape()
#   if (!ZarrArray_global_option_is_set("dump.compression.level"))
#     setZarrDumpCompressionLevel()
#   file.create(get_Zarr_dump_logfile())
#   .DATASET_CREATION_GLOBAL_COUNTER_FILEPATH <<-
#     init_Zarr_dataset_creation_global_counter()
# }

.onUnload <- function(libpath)
{
  #file.remove(.DUMP_FILES_GLOBAL_COUNTER_FILEPATH)
  file.remove(.DUMP_NAMES_GLOBAL_COUNTER_FILEPATH)
  file.remove(.DATASET_CREATION_GLOBAL_COUNTER_FILEPATH)
}

# .test <- function() BiocGenerics:::testPackage("ZarrArray")