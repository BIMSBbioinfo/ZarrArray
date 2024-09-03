# ZarrArray

Zarr backend for DelayedArray objects

## Installation

``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("BIMSBbioinfo/ZarrArray")
```

## Usage

The main usage of the ZarrArray is to deliver DelayedArray operations for zarr arrays.

``` r
mat <- array(data=1:1000, dim=c(20, 50))
mat <- writeZarrArray(mat, filepath = "data/mat.zarr", name = "assay")
mat
```

```
<20 x 50> ZarrMatrix object of type "double":
       [,1]  [,2]  [,3] ... [,49] [,50]
 [1,]     1    21    41   .   961   981
 [2,]     2    22    42   .   962   982
 [3,]     3    23    43   .   963   983
 [4,]     4    24    44   .   964   984
 [5,]     5    25    45   .   965   985
  ...     .     .     .   .     .     .
[16,]    16    36    56   .   976   996
[17,]    17    37    57   .   977   997
[18,]    18    38    58   .   978   998
[19,]    19    39    59   .   979   999
[20,]    20    40    60   .   980  1000
```

However, Zarr arrays could be constructed for in-memory arrays as well.

``` r
mat <- array(data=1:1000, dim=c(20, 50))
mat <- ZarrArray(mat)
mat
```

```
<20 x 50> ZarrMatrix object of type "double":
       [,1]  [,2]  [,3] ... [,49] [,50]
 [1,]     1    21    41   .   961   981
 [2,]     2    22    42   .   962   982
 [3,]     3    23    43   .   963   983
 [4,]     4    24    44   .   964   984
 [5,]     5    25    45   .   965   985
  ...     .     .     .   .     .     .
[16,]    16    36    56   .   976   996
[17,]    17    37    57   .   977   997
[18,]    18    38    58   .   978   998
[19,]    19    39    59   .   979   999
[20,]    20    40    60   .   980  1000
```