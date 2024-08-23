# ZarrArray

Zarr backend for DelayedArray objects

## Installation


``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("BIMSBbioinfo/ZarrArray")
```

## Usage


``` r
mat_test <- matrix(runif(364, min=-1), nrow=26, 
                   dimnames=list(letters, LETTERS[1:14]))
mat_test <- writeZarrArray(mat_test, filepath = "mat_test.zarr", name = "assay")
```