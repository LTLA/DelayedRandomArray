# This checks that the chunked sampling is self-consistent.
# library(DelayedRandomArray); library(testthat); source("test-chunking.R")

test_that("recovery of a single chunk makes sense", {
    seeds <- list(c(12345L, 12345L))
    arr <- DelayedRandomArray:::sample_standard_uniform(c(10, 10), c(10, 10), seeds, list(1:10, 1:10))

    # Looks pretty random to me:
    expect_identical(dim(arr), c(10L, 10L))
    expect_false(any(arr==0 | arr==1))
    expect_true(is.unsorted(as.vector(arr)))
    expect_identical(anyDuplicated(arr), 0L)

    # Reproducible:
    arr2 <- DelayedRandomArray:::sample_standard_uniform(c(10, 10), c(10, 10), seeds, list(1:10, 1:10))
    expect_identical(arr2, arr)

    # Works with different chunk sizes:
    arr3 <- DelayedRandomArray:::sample_standard_uniform(c(10, 5), c(10, 5), seeds, list(1:10, 1:5))
    expect_identical(arr[,1:5], arr3)

    # Works with arrays.
    arr4 <- DelayedRandomArray:::sample_standard_uniform(c(10, 5, 2), c(10, 5, 2), seeds, list(1:10, 1:5, 1:2))
    expect_identical(dim(arr4), c(10L, 5L, 2L))
    expect_identical(arr4[,,1], arr3)

    subarr <- arr4[,,2]
    expect_false(any(subarr==0 | subarr==1))
    expect_true(is.unsorted(as.vector(subarr)))
    expect_identical(anyDuplicated(subarr), 0L)
})

FULL_COMPARE <- function(dim, chunkdim) {
    nchunks <- ceiling(dim/chunkdim)
    seeds <- dqrng::generateSeedVectors(prod(nchunks))
    full <- DelayedRandomArray:::sample_standard_uniform(dim, chunkdim, seeds, lapply(dim, seq_len))
    expect_identical(dim(full), as.integer(dim))

    identifier <- c(1, cumprod(head(nchunks, -1)))
    for (i in seq_along(seeds)) {
        chunk.dex <- integer(length(dim))
        curid <- (i-1)
        for (j in rev(seq_along(dim))) {
            chunk.dex[j] <- as.integer(curid / identifier[j])
            curid <- curid %% identifier[j]
        }

        everything <- subsets <- list()
        for (j in seq_along(dim)) {
            idx <- seq_len(chunkdim[j])
            full.idx <- idx + chunk.dex[j] * chunkdim[j]
            keep <- full.idx <= dim[j]
            subsets[[j]] <- full.idx[keep]
            everything[[j]] <- idx[keep]
        }

        current.chunk <- DelayedRandomArray:::sample_standard_uniform(chunkdim, chunkdim, seeds[i], everything, stream_start = i-1)
        extracted <- do.call("[", c(list(full), subsets, list(drop=FALSE)))
        expect_identical(extracted, current.chunk)

        full <- do.call("[<-", c(list(full), subsets, list(value=0)))
    }

    expect_true(all(full==0))
}

test_that("full extraction works with multiple chunks in a grid", {
    # 2D cases.
    FULL_COMPARE(c(100, 50), c(20, 10))
    FULL_COMPARE(c(100, 50), c(20, 25))
    FULL_COMPARE(c(50, 100), c(20, 25))
    FULL_COMPARE(c(100, 50), c(9, 8))

    # 3D cases.
    FULL_COMPARE(c(100, 50, 20), c(20, 10, 5))
    FULL_COMPARE(c(100, 50, 20), c(20, 25, 2))
    FULL_COMPARE(c(100, 50, 20), c(9, 8, 7))
    FULL_COMPARE(c(50, 100, 20), c(9, 8, 7))
})
