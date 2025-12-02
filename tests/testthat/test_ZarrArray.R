library(Rarr)

test_that("create metadata", {
  
  # example zarr data
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(20, 50))
  mat <- writeZarrArray(mat, filepath = store, name = "assay")
  mat[c(1,6,7),c(2,8,9)]
  unlink(store, recursive = TRUE)
  
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(2, 50, 10))
  mat <- writeZarrArray(mat, filepath = store, name = "assay")
  mat[1,c(2,8,9),1:5]
  unlink(store, recursive = TRUE)
  
  expect_equal(1,1)
})

test_that("dimnames", {
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(20, 50), dimnames = list(1:20, 1:50))
  mat <- writeZarrArray(mat, filepath = store, name = "assay")
  mat[c(1,6,7),c(2,8,9)]
  unlink(store, recursive = TRUE)
  
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(20, 50), dimnames = list(paste("row", 1:20), paste("col",1:50)))
  mat <- writeZarrArray(mat, filepath = store, name = "assay")
  mat[c(1,6,7),c(2,8,9)]
  unlink(store, recursive = TRUE)
  
  expect_equal(1,1)
})

test_that("in memory ZarrArray", {
  mat <- array(data=1:1000, dim=c(20, 50))
  mat <- ZarrArray(mat)
  mat[c(1,6,7),c(2,8,9)]
})

test_that("write twice", {
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(20, 50))
  mat <- writeZarrArray(mat, filepath = store, name = "assay1")
  mat[c(1,6,7),c(2,8,9)]
  mat <- array(data=1:1000, dim=c(20, 50))
  mat <- writeZarrArray(mat, filepath = store, name = "assay2")
  mat[c(1,6,7),c(2,8,9)]
  unlink(store, recursive = TRUE)
  
  expect_equal(1,1)
})

test_that("as.array", {
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(10, 50, 2))
  mat <- writeZarrArray(mat, filepath = store, name = "assay")
  mat2 <- as.array(mat)
  unlink(store, recursive = TRUE)
  
  expect_equal(1,1)
})

test_that("aperm", {
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=1:1000, dim=c(20, 50))
  mat <- writeZarrArray(mat, filepath = store, name = "assay")
  aperm(mat, perm = c(2,1))
  unlink(store, recursive = TRUE)
  
  expect_equal(1,1)
})

test_that("test string IO", {
  store <- tempfile(fileext = ".zarr")
  mat <- array(data=rep("a", 10), dim=10)
  mat <- writeZarrArray(mat, filepath = store, name = "string_assay")
  mat[c(2,8,9)]  
  unlink(store, recursive = TRUE)
  
  expect_equal(1,1)
})