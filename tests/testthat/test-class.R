# This tests the base class utilities.
# library(DelayedRandomArray); library(testthat); source("test-class.R")

test_that("class construction works as expected", {
    arr <- RandomUnifArray(c(100, 50, 20))
    expect_identical(dim(arr), c(100L, 50L, 20L))
    expect_type(chunkdim(arr), "integer")
    expect_false(is_sparse(arr))

    expect_error(RandomUnifArray(c(100, 50, 20), chunkdim=c(-1,0,0)), "non-negative")
    expect_error(RandomUnifArray(c(100, 50, 20), chunkdim=c(0,0,0)), "positive")
    expect_error(RandomUnifArray(c(100, 50, 20), chunkdim=c(0,0)), "same length")
    expect_error(RandomUnifArray(c(100, 50, 20), chunkdim=c(100,100,100)), "larger")

    expect_output(show(arr), "RandomUnifArray")

    mat <- RandomUnifArray(c(100, 50))
    expect_output(show(mat), "RandomUnifMatrix")
})

test_that("subsetting works as expected", {
    full <- RandomUnifArray(c(100, 50, 20))
    ref <- as.array(full)

    expect_identical(ref[1:10,1:10,], as.array(full[1:10,1:10,]))

    # Non-contiguous slices.
    expect_identical(ref[1:10*10,1:10*5,], as.array(full[1:10*10,1:10*5,]))

    # Unordered slices. 
    chosen1 <- sample(100, 10)
    chosen2 <- sample(50, 20)
    expect_identical(ref[chosen1,chosen2,], as.array(full[chosen1,chosen2,]))

    chosen1 <- sort(chosen1, decreasing=TRUE)
    expect_identical(ref[chosen1,chosen2,], as.array(full[chosen1,chosen2,]))

    # Duplicates.
    chosen1 <- sample(10, 100, replace=TRUE)
    chosen2 <- sample(50, 100, replace=TRUE)
    expect_identical(ref[chosen1,chosen2,], as.array(full[chosen1,chosen2,]))

    # Works for the third dimension.
    expect_identical(ref[,,10:1], as.array(full[,,10:1]))
})

test_that("chunking behaves predictably", {
    arr <- RandomUnifArray(c(1000, 1000))
    expect_identical(chunkdim(arr), c(100L, 100L))

    # Scales with large chunks.
    arr <- RandomUnifArray(c(40000, 10000))
    expect_identical(chunkdim(arr), c(200L, 100L))

    # Handles small chunks.
    arr <- RandomUnifArray(c(50, 10))
    expect_identical(chunkdim(arr), c(50L, 10L))

    # Handles custom chunks.
    arr <- RandomUnifArray(c(100, 100), chunkdim=c(100, 1L))
    expect_identical(chunkdim(arr), c(100L, 1L))
})

test_that("parameter handling works as expected", {
    # Handles scalars.
    arr <- RandomUnifArray(c(100, 50), min=1, max=5)
    expect_identical(sampleDistrParam(seed(arr)), c("min", "max"))
    expect_true(all(arr > 1))
    expect_true(all(arr < 5))

    # Handles vectors.
    set.seed(100)
    arr <- RandomUnifArray(c(100, 50))

    set.seed(100)
    arr2 <- RandomUnifArray(c(100, 50), min=1:100/20, max=5)
    expect_identical(as.vector(arr2), qunif(as.vector(arr), 1:100/20, 5))

    set.seed(100)
    arr2 <- RandomUnifArray(c(100, 50), min=1:20/5, max=5)
    expect_identical(as.vector(arr2), qunif(as.vector(arr), 1:20/5, 5))

    set.seed(100)
    expect_warning(arr2 <- RandomUnifArray(c(100, 50), min=1:30/6, max=5), "multiple")
    expect_identical(as.vector(arr2), qunif(as.vector(arr), 1:30/6, 5))

    set.seed(100)
    MIN <- matrix(1:2000/400, 100, 50)
    arr2 <- RandomUnifArray(c(100, 50), min=MIN, max=5)
    expect_identical(as.vector(arr2), qunif(as.vector(arr), as.vector(MIN), 5))

    set.seed(100)
    MIN <- matrix(1:2000/400, 100, 50)
    arr2 <- RandomUnifArray(c(100, 50), min=MIN, max=5)
    expect_identical(as.vector(arr2), qunif(as.vector(arr), as.vector(MIN), 5))

    expect_error(RandomUnifArray(c(100, 50), min=MIN[,1:10], max=5), "must be the same")
})



