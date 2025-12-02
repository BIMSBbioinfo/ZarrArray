library(Rarr)

# zarr file
zarr_dir <- system.file("extdata", "example2.zarr.zip", package = "ZarrArray")
td <- tempdir(check = TRUE)
unzip(zarr_dir, exdir = td)
store <- file.path(td, "example2.zarr")

test_that("read sparse", {
  
  # read sparse matrix
  name <- "layers/csc_counts"
  ZarrSparseMatrix(store, name)
  
  expect_equal(1,1)
})