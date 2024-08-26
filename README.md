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
mat <- array(data=1:1000, dim=c(20, 50))
mat <- writeZarrArray(mat, filepath = "data/mat.zarr", name = "assay")
mat[1:5,1:5]
```

```
<5 x 5> DelayedMatrix object of type "double":
     [,1] [,2] [,3] [,4] [,5]
[1,]    1   21   41   61   81
[2,]    2   22   42   62   82
[3,]    3   23   43   63   83
[4,]    4   24   44   64   84
[5,]    5   25   45   65   85
```