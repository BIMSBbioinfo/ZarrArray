### =========================================================================
### zarrwriteDimnames() and other related functions
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### get_zarrdimnames() / set_zarrdimnames()
###

get_zarrdimnames <- function(filepath, name)
{
    zarrgetdimscales(filepath, name, scalename="dimnames")
}

### Fail if 'name' is a Dimension Scale dataset or has Dimension Scales on it.
.check_filepath_and_name <- function(filepath, name)
{
    if (zarrisdimscale(filepath, name))
        stop(wmsg("Zarr dataset \"", name, "\" contains the dimnames for ",
                  "another dataset in the Zarr file so dimnames cannot ",
                  "be set on it"))
    current_zarrdimnames <- get_zarrdimnames(filepath, name)
    if (!all(is.na(current_zarrdimnames))) {
        ds <- current_zarrdimnames[!is.na(current_zarrdimnames)]
        stop(wmsg("the dimnames for Zarr dataset \"", name, "\" are already ",
                  "stored in Zarr file \"", filepath, "\" (in dataset(s): ",
                  paste(paste0("\"", ds, "\""), collapse=", "), ")"))
    }
    # TODO: check if you need get/set methods for dimlabels
    # dimlabels <- zarrgetdimlabels(filepath, name)
    # if (!is.null(dimlabels))
    #     stop(wmsg("Zarr dataset \"", name, "\" already has dimension labels"))
}

.validate_zarrdimnames_lengths <- function(filepath, name, zarrdimnames)
{
    dim <- zarrdim(filepath, name)
    for (along in which(!is.na(zarrdimnames))) {
        zarrdn <- zarrdimnames[[along]]
        zarrdn_len <- prod(zarrdim(filepath, zarrdn))
        if (zarrdn_len != dim[[along]])
            return(paste0("length of Zarr dataset \"", zarrdn, "\" ",
                          "(", zarrdn_len, ") is not equal to the ",
                          "extent of dimension ", along, " in Zarr ",
                          "dataset \"", name, "\" (", dim[[along]], ")"))
    }
    TRUE
}

.check_zarrdimnames <- function(filepath, name, zarrdimnames)
{
    dim <- zarrdim(filepath, name)
    ndim <- length(dim)
    if (!is.character(zarrdimnames))
        stop(wmsg("'zarrdimnames' must be a character vector containing ",
                  "the names of the Zarr datasets to set as the ",
                  "dimnames of dataset \"", name, "\" (one per dimension ",
                  "in \"", name, "\")"))
    if (length(zarrdimnames) > ndim)
        stop(wmsg("length of 'zarrdimnames' must equal the number of ",
                  "dimensions (", ndim, ") in Zarr dataset \"", name, "\""))
    for (along in which(!is.na(zarrdimnames))) {
        zarrdn <- zarrdimnames[[along]]
        if (!zarrexists(filepath, zarrdn))
            stop(wmsg("Zarr dataset \"", zarrdn, "\" does not exist ",
                      "in this Zarr file"))
    }
    msg <- .validate_zarrdimnames_lengths(filepath, name, zarrdimnames)
    if (!isTRUE(msg))
        stop(wmsg("invalid 'zarrdimnames': ", msg))
}

set_zarrdimnames <- function(filepath, name, zarrdimnames, dry.run=FALSE)
{
    .check_filepath_and_name(filepath, name)
    .check_zarrdimnames(filepath, name, zarrdimnames)
    zarrsetdimscales(filepath, name, dimscales=zarrdimnames,
                   scalename="dimnames", dry.run=dry.run)
    invisible(NULL)
}

### For internal use only (not exported).
### FIXME: Make this work on an zarrFile object!
validate_lengths_of_zarrdimnames <- function(filepath, name)
{
    if (is(filepath, "zarrFile"))
        return(TRUE)  # we cheat for now (fix this!)
    zarrdimnames <- get_zarrdimnames(filepath, name)
    msg <- .validate_zarrdimnames_lengths(filepath, name, zarrdimnames)
    if (!isTRUE(msg))
        return(paste0("invalid dimnames found in Zarr file \"", filepath, "\" ",
                      "for dataset \"", name, "\": ", msg))
    TRUE
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrwriteDimnames() / zarrreadDimnames()
###

.check_dimnames <- function(dimnames, filepath, name)
{
    if (!is.list(dimnames))
        stop(wmsg("'dimnames' must be a list"))
    dim <- zarrdim(filepath, name)
    ndim <- length(dim)
    if (length(dimnames) > ndim)
        stop(wmsg("'dimnames' cannot have more list elements than ",
                  "the number of dimensions in dataset \"", name,"\""))
    not_NULL <- !S4Vectors:::sapply_isNULL(dimnames)
    for (along in which(not_NULL)) {
        dn <- dimnames[[along]]
        if (!(is.vector(dn) && is.atomic(dn)))
            stop(wmsg("each list element in the supplied 'dimnames' ",
                      "must an atomic vector or a NULL"))
        if (length(dn) != dim[[along]])
            stop(wmsg("length of 'dimnames[[", along, "]]' ",
                      "(", length(dn), ") must equal the ",
                      "extent of the corresponding dimension in ",
                      "Zarr dataset \"", name, "\" (", dim[[along]], ")"))
    }
    dimlabels <- names(dimnames)
    if (!is.null(dimlabels) && any(is.na(dimlabels)))
        stop(wmsg("'names(dimnames)' cannot contain NAs"))
    not_NULL
}

.normarg_group <- function(group, name)
{
    if (!isSingleStringOrNA(group))
        stop(wmsg("'group' must be a single string or NA"))
    if (is.na(group)) {
        group <- add_prefix_to_basename(name, prefix=".")
        group <- paste0(group, "_dimnames")
    }
    group
}

.normarg_zarrdimnames <- function(zarrdimnames, group, not_NULL, filepath, name)
{
    ndim <- length(not_NULL)
    if (is.null(zarrdimnames)) {
        ## Generate automatic dataset names.
        digits <- as.integer(log10(ndim + 0.5)) + 1L
        fmt <- paste0("%0", digits, "d")
        zarrdimnames <- sprintf(fmt, seq_len(ndim))
    } else {
        if (!is.character(zarrdimnames) || length(zarrdimnames) != ndim)
            stop(wmsg("'zarrdimnames' must be a character vector containing ",
                      "the names of the Zarr datasets where to write the ",
                      "dimnames of dataset \"", name, "\" (one per dimension ",
                      "in \"", name, "\")"))
        if (any(not_NULL & is.na(zarrdimnames)))
            stop(wmsg("'zarrdimnames' cannot have NAs associated with ",
                      "list elements in 'dimnames' that are not NULL"))
    }
    if (nzchar(group))
        zarrdimnames <- paste0(group, "/", zarrdimnames)
    zarrdimnames[!not_NULL] <- NA_character_
    for (along in which(not_NULL)) {
        zarrdn <- zarrdimnames[[along]]
        if (zarrexists(filepath, zarrdn))
            stop(wmsg("Zarr dataset \"", zarrdn, "\" already exists"))
    }
    zarrdimnames
}

### dimnames:   A list (possibly named) with 1 list element per dimension in
###             dataset 'name'.
### name:       The name of the Zarr dataset on which to set the dimnames.
### group:      The name of the Zarr group where to write the dimnames.
###             If NA, the group name is automatically generated from 'name'.
###             An empty string ("") means that no group should be used.
###             Otherwise, the names in 'zarrdimnames' must be relative to the
###             specified group name.
### zarrdimnames: A character vector containing the names of the Zarr datasets
###             (1 per list element in 'dimnames') where to write the dimnames.
###             Names associated with NULL list elements in 'dimnames' are
###             ignored.
zarrwriteDimnames <- function(dimnames, filepath, name, group=NA, zarrdimnames=NULL)
{
    ## 1. Lots of checks.

    ## Before we start writing to the file we want some guarantees that
    ## the full operation will succeed. The checks we make access the file
    ## in read-only mode.
    .check_filepath_and_name(filepath, name)

    not_NULL <- .check_dimnames(dimnames, filepath, name)

    group <- .normarg_group(group, name)

    zarrdimnames <- .normarg_zarrdimnames(zarrdimnames, group, not_NULL,
                                      filepath, name)

    ## 2. Write to the Zarr file.

    ## Create group if needed.
    if (!is.na(group) && !zarrexists(filepath, group))
      pizzarr::zarr_open_group(store = filepath, path = group, mode = "a")
        
    ## Write dimnames.
    for (along in which(not_NULL)) {
        dn <- dimnames[[along]]
        zarrdn <- zarrdimnames[[along]]
        zarrdimname <- pizzarr::zarr_open_array(store = filepath, path = zarrdn, 
                                                mode = "a", shape = length(dn), 
                                                dtype = "<U20")
        zarrdimname$set_item("...", array(dn))
    }
}

zarrreadDimnames <- function(filepath, name, as.character=FALSE)
{
    if (!isTRUEorFALSE(as.character))
        stop(wmsg("'as.character' must be TRUE or FALSE"))
    zarrdimnames <- get_zarrdimnames(filepath, name)
    if (all(is.na(zarrdimnames)))
        return(NULL)
    # TODO: check if you need get/set methods for dimlabels
    # dimlabels <- zarrgetdimlabels(filepath, name) 
    lapply(zarrdimnames,
           function(zarrdn) {
               if (all(is.na(zarrdn)))
                   return(NULL)
               dn <- zarrdn
               if (as.character) as.character(dn) else dn
           })
}

