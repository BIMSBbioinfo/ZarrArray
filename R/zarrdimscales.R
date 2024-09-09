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
  # .Call2("C_zarrisdimscale", filepath, name, PACKAGE="HDF5Array")
  
  # open zarr and check
  file.status <- try(zarr <- pizzarr::zarr_open(store = filepath, mode = "r"))
  if(inherits(file.status, "try-error")){
    file.remove(filepath)
    return(FALSE)
  }

  # check group 
  if(zarr$contains_item(name)){    
    
    # get dimnames
    zarr_group <- zarr$get_item(name)
    if(inherits(zarr_group, "ZarrGroup")){
      
      # check ZarrArray names
      zarr_group <- zarr_open_group(paste0(filepath, "/", name), mode = "r")
      zarr_group_mem <- zarr_group$get_store()$listdir()
      zarr_group_mem <- zarr_group_mem[!grepl("^\\.", zarr_group_mem)]
      zarr_group_mem <- zarr_group_mem[sapply(zarr_group_mem, function(z) inherits(zarr_group$get_item(z), "ZarrArray"))]
      if(length(zarr_group_mem) > 0){
        zarr_group_mem <- na.omit(as.numeric(zarr_group_mem))

        # read dim names
        zarrdimnames <- c()
        if(length(zarr_group_mem) > 0 && all(is_integer(zarr_group_mem))){
          for(i in 1:length(zarr_group_mem)){
            zarr_array <- pizzarr::zarr_open_array(paste0(filepath, "/", name, "/", zarr_group_mem[i]))
            zarrdimnames <- c(zarrdimnames, 
                              zarr_array$get_ndim())
          }
          return(all(zarrdimnames == 1))
        } else {
          return(FALSE)
        }
        # there are no datasets under group  
      } else {
        return(FALSE)
      }
      # scales are not stored in a group 
    } else {
      return(FALSE)
    }
    # if group doesn't exist, return NULL
  } else{
    return(FALSE)
  }
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### zarrgetdimscales() / zarrsetdimscales()
###

### Retrieve the names of the existing HDF5 datasets (1 per dimension in
### dataset 'name') currently attached along the dimensions of dataset 'name'
### for Dimension Scale 'scalename'.
zarrgetdimscales <- function(filepath, name, scalename=NA)
{
  
  # check scalename
  stopifnot(isSingleStringOrNA(scalename))
  scalename <- as.character(scalename)
  scale_path <- paste0("/.", gsub("^/", "", name), "_", scalename)
  
  # only if scalename is a scale
  if(zarrisdimscale(filepath = filepath, 
                    name = scale_path)){
    
    # open zarr
    zarr <- pizzarr::zarr_open(store = filepath, mode = "r")
    zarr_array_ndim <- zarr$get_item(name)$get_ndim()
    
    # get dimnames
    zarr_group <- zarr_open_group(paste0(filepath, scale_path))
    zarr_group_mem <- zarr_group$get_store()$listdir()
    zarr_group_mem <- zarr_group_mem[!grepl("^\\.", zarr_group_mem)]
    zarr_group_mem <- zarr_group_mem[sapply(zarr_group_mem, function(z) inherits(zarr_group$get_item(z), "ZarrArray"))]
    if(length(zarr_group_mem) > 0){
      zarr_group_mem <- na.omit(as.numeric(zarr_group_mem))
      zarr_group_mem <- zarr_group_mem[zarr_group_mem < (zarr_array_ndim + 1)]
      
      # read dim names
      zarrdimnames <- list()
      if(length(zarr_group_mem) > 0){
        for(i in 1:length(zarr_group_mem)){
          zarr_array <- pizzarr::zarr_open_array(paste0(filepath, scale_path, "/", zarr_group_mem[i]))
          zarrdimnames[[as.character(zarr_group_mem[i])]] <- 
            zarr_array$get_item("...")$data
        }
        return(zarrdimnames)
        # no scales are saved as integers (index of dimensions, e.g. 1,2,3)    
      } else {
        return(NA)
      }
      # there are no datasets under group  
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
  
  # # open zarr
  # zarr <- pizzarr::zarr_open(store = filepath, mode = "r")
  # zarr_store <- zarr$get_store()
  # zarr_array_ndim <- zarr$get_item(name)$get_ndim()
  # 
  # # check group 
  # if(zarr$contains_item(paste0(name, "_", scalename))){    
  #   
  #   # get dimnames
  #   # zarr_group <- zarr$get_item(paste0(name, "_", scalename))  
  #   zarr_group <- zarr_open_group(paste0(filepath, "/", name, "_", scalename))
  #   if(inherits(zarr_group, "ZarrGroup")){
  #     
  #     # check ZarrArray names
  #     zarr_group_mem <- zarr_group$get_store()$listdir()
  #     zarr_group_mem <- zarr_group_mem[!grepl("^\\.", zarr_group_mem)]
  #     zarr_group_mem <- zarr_group_mem[sapply(zarr_group_mem, function(z) inherits(zarr_group$get_item(z), "ZarrArray"))]
  #     if(length(zarr_group_mem) > 0){
  #       zarr_group_mem <- na.omit(as.numeric(zarr_group_mem))
  #       zarr_group_mem <- zarr_group_mem[zarr_group_mem < (zarr_array_ndim + 1)]
  #       
  #       # read dim names
  #       zarrdimnames <- list()
  #       if(length(zarr_group_mem) > 0){
  #         for(i in 1:length(zarr_group_mem)){
  #           zarr_array <- pizzarr::zarr_open_array(paste0(filepath, "/", name, "_", scalename, "/", zarr_group_mem[i]))
  #           zarrdimnames[[as.character(zarr_group_mem[i])]] <- 
  #             zarr_array$get_item("...")$data
  #         }
  #         return(zarrdimnames)
  #       # no scales are saved as integers (index of dimensions, e.g. 1,2,3)    
  #       } else {
  #         return(NULL)
  #       }
  #     # there are no datasets under group  
  #     } else {
  #       return(NULL)
  #     }
  #   # scales are not stored in a group 
  #   } else {
  #     return(NULL)
  #   }
  # # if group doesn't exist, return NULL
  # } else{
  #   return(NULL)
  # }
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


# ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ### Get/set the "dimension labels" of an HDF5 dataset
# ###
# ### The "dimension labels" the HDF5 equivalent of the names on 'dimnames(a)'
# ### in R.
# ###
# 
# zarrgetdimlabels <- function(filepath, name)
# {
#   TODO: zarrgetdimlabels method for zarr arrays
# }
# 
# zarrsetdimlabels <- function(filepath, name, dimlabels)
# {
#   stopifnot(is.character(dimlabels))
#   TODO: zarrsetdimlabels method for zarr arrays
# }