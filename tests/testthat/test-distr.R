# This tests the various specific distributions.
# library(testthat); library(DelayedRandomArray); source("test-distr.R")

FUN <- function(class, distr, defaults, ranges, has.sparse=FALSE) {
    set.seed(999)
    d <- c(100L, 50L)
    ref <- RandomUnifArray(d)
    constructor <- get(class)

    args0 <- as.list(defaults)

    for (p in names(ranges)) {
        r <- ranges[[p]]
        args <- args0

        # Scalar:
        set.seed(999)
        args[[p]] <- r[1]
        out <- do.call(constructor, c(list(d), args))
        expect_identical(dim(out), d)
        expect_identical(as.vector(out), do.call(distr, c(list(as.vector(ref)), args)))
        expect_false(is_sparse(out))

        if (has.sparse) {
            set.seed(999)
            out2 <- do.call(constructor, c(list(d), args, list(sparse=TRUE)))
            expect_true(is_sparse(out2))
            expect_identical(dim(out2), d)
            expect_identical(as.vector(out2), as.vector(out))
        }

        # Vector:
        args[[p]] <- runif(d[1], r[1], r[2])
        if (is.integer(r)) {
            args[[p]] <- round(args[[p]])
        }

        set.seed(999)
        out <- do.call(constructor, c(list(d), args))
        expect_identical(dim(out), d)
        expect_identical(as.vector(out), do.call(distr, c(list(as.vector(ref)), args)))

        # Array.
        args[[p]] <- RandomUnifArray(d, min=r[1], max=r[2])
        if (is.integer(r)) {
            args[[p]] <- round(args[[p]])
        }

        set.seed(999)
        out <- do.call(constructor, c(list(d), args))
        expect_identical(dim(out), d)
        expect_identical(as.vector(out), do.call(distr, c(list(as.vector(ref)), lapply(args, as.vector))))
    }
}

test_that("Beta works as expected", {
    FUN("RandomBetaArray", qbeta, list(shape1=1, shape2=1), ranges=list(shape1=c(1, 5), shape2=c(1, 10), ncp=c(0, 2)))
})

test_that("Binom works as expected", {
    FUN("RandomBinomArray", qbinom, list(size=5, prob=0.5), ranges=list(size=c(1L, 5L), prob=c(0, 1)), has.sparse=TRUE)
})

test_that("Cauchy works as expected", {
    FUN("RandomCauchyArray", qcauchy, list(), ranges=list(location=c(-1, 1), scale=c(0.5, 5)))
})

test_that("Exp works as expected", {
    FUN("RandomExpArray", qexp, list(), ranges=list(rate=c(0.5, 10)))
})

test_that("F works as expected", {
    FUN("RandomFArray", qf, list(df1=1, df2=10), ranges=list(df1=c(1, 10), df2=c(5, 20), ncp=c(1, 5)))
})

test_that("Gamma works as expected", {
    FUN("RandomGammaArray", qgamma, list(shape=5), ranges=list(shape=c(1, 10), rate=c(1, 10), scale=c(1, 10)))
})

test_that("Geom works as expected", {
    FUN("RandomGeomArray", qgeom, list(), ranges=list(prob=c(0.01, 0.99)), has.sparse=TRUE)
})

test_that("Hyper works as expected", {
    FUN("RandomHyperArray", qhyper, list(m=10, n=7, k=8), ranges=list(m=c(5L, 20L), n=c(5L, 20L), k=c(5L, 10L)), has.sparse=TRUE)
})

test_that("Lnorm works as expected", {
    FUN("RandomLnormArray", qlnorm, list(), ranges=list(meanlog=c(0, 5), sdlog=c(0.1, 10)))
})

test_that("Logis works as expected", {
    FUN("RandomLogisArray", qlogis, list(), ranges=list(location=c(0, 5), scale=c(0.1, 10)))
})

test_that("Nbinom works as expected", {
    FUN("RandomNbinomArray", qnbinom, list(size=10, prob=0.5), ranges=list(size=c(0.1, 100), prob=c(0.1, 0.9)))

    FUN("RandomNbinomArray", qnbinom, list(size=10), ranges=list(mu=c(0.1, 100)), has.sparse=TRUE)
})

test_that("Norm works as expected", {
    FUN("RandomNormArray", qnorm, list(), ranges=list(mean=c(-5, 5), sd=c(0.1, 10)))
})

test_that("Pois works as expected", {
    FUN("RandomPoisArray", qpois, list(), ranges=list(lambda=c(1, 100)), has.sparse=TRUE)
})

test_that("T works as expected", {
    FUN("RandomTArray", qt, list(df=10), ranges=list(df=c(5, 50), ncp=c(0, 100)))
})

test_that("Unif works as expected", {
    FUN("RandomUnifArray", qunif, list(), ranges=list(min=c(-2, -1), max=c(3, 5)))
})

test_that("Weibull works as expected", {
    FUN("RandomWeibullArray", qweibull, list(shape=10), ranges=list(shape=c(5, 50), scale=c(0.5, 10)))
})

test_that("Wilcox works as expected", {
    FUN("RandomWilcoxArray", qwilcox, list(m=10, n=5), ranges=list(m=c(5L, 50L), n=c(2L, 10L)))
})
