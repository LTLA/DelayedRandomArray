# This tests the recycling utilities.
# library(testthat); library(DelayedRandomArray); source("test-recycle.R")

REFCYCLE <- function(vec, dim, index) {
    mat <- array(seq_len(prod(dim)), dim)
    keep <- extract_array(mat, index)
    vec[(as.integer(keep)-1) %% length(vec) + 1]
}

test_that("recycling works as expected (2D)", {
    for (i in c(10, 20, 5, 7)) {
        X <- runif(i)
        out <- DelayedRandomArray:::recycle_vector(X, c(10, 5), list(NULL, NULL))
        ref <- REFCYCLE(X, c(10, 5), list(NULL, NULL))
        expect_identical(out, ref)

        out <- DelayedRandomArray:::recycle_vector(X, c(10, 5), list(2:5, c(1,3,5)))
        ref <- REFCYCLE(X, c(10, 5), list(2:5, c(1,3,5)))
        expect_identical(out, ref)
    }
})

test_that("recycling works as expected (3D)", {
    for (i in c(10, 20, 50, 5, 7)) {
        X <- runif(i)
        out <- DelayedRandomArray:::recycle_vector(X, c(10, 5, 2), list(NULL, NULL, NULL))
        ref <- REFCYCLE(X, c(10, 5, 2), list(NULL, NULL, NULL))
        expect_identical(out, ref)

        out <- DelayedRandomArray:::recycle_vector(X, c(10, 5, 2), list(2:5, c(1,3,5), 1))
        ref <- REFCYCLE(X, c(10, 5, 2), list(2:5, c(1,3,5), 1))
        expect_identical(out, ref)

        out <- DelayedRandomArray:::recycle_vector(X, c(10, 5, 2), list(2:5, c(1,3,5), 1:2))
        ref <- REFCYCLE(X, c(10, 5, 2), list(2:5, c(1,3,5), 1:2))
        expect_identical(out, ref)
    }
})

test_that("recycling works as expected (1D)", {
    for (i in c(10, 20, 5, 7)) {
        X <- runif(i)
        out <- DelayedRandomArray:::recycle_vector(X, 100, list(NULL, NULL))
        ref <- REFCYCLE(X, 100, list(NULL))
        expect_identical(out, ref)

        out <- DelayedRandomArray:::recycle_vector(X, c(100), list(1:10*10))
        ref <- REFCYCLE(X, c(100), list(1:10*10))
        expect_identical(out, ref)
    }
})


