#' DelayedArray of random negative binomial values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of negative binomial-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomNbinomArraySeed object.
#' @param prob,size,mu Numeric vector used as the argument of the same name in \code{\link{qnbinom}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#'
#' Exactly one of \code{prob} or \code{mu} should be supplied.
#' @param sparse Logical scalar indicating whether the sampled matrix should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomNbinomArray object,
#' containing random draws from a negative binomial distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomNbinomArray-class
#' RandomNbinomArraySeed-class
#' RandomNbinomMatrix-class
#' sampleDistrParam,RandomNbinomArraySeed-method
#' sampleDistrFun,RandomNbinomArraySeed-method
#' matrixClass,RandomNbinomArraySeed-method
#' extract_array,RandomNbinomArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomNbinomArraySeed(c(1e5, 1e5), size=10, mu=20)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomNbinomArraySeed(c(1e5, 1e5), size=10, mu=runif(1e5), sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' lambda <- rsparsematrix(1e5, 1e5, density=0.00001)
#' lambda <- abs(DelayedArray(lambda)) + 0.1
#' X3 <- RandomNbinomArraySeed(c(1e5, 1e5), size=1, mu=lambda)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomNbinomArray-class
NULL

#' @export
#' @rdname RandomNbinomArray-class
RandomNbinomArraySeed <- function(dim, prob=prob, size=size, mu=mu, chunkdim=NULL, sparse=FALSE) {
    if (missing(prob)) {
        prob <- NULL
    }
    if (missing(mu)) {
        mu <- NULL
    }
    new("RandomNbinomArraySeed", dim=dim, prob=prob, size=size, mu=mu, chunkdim=chunkdim, sparse=sparse)
}

setValidity2("RandomNbinomArraySeed", function(object) {
    if (is.null(object@prob) == is.null(object@mu)) {
        return("exactly one of 'mu' or 'prob' should be supplied")
    }

    if (!is.null(object@prob)) {
        msg <- .is_valid_param(object@dim, object@prob, "prob")
        if (!is.null(msg)) {
            return(msg)
        }
    }

    if (!is.null(object@mu)) {
        msg <- .is_valid_param(object@dim, object@mu, "mu")
        if (!is.null(msg)) {
            return(msg)
        }
    }
    TRUE
})

#' @export
setMethod("sampleDistrParam", "RandomNbinomArraySeed", function(x) "size")

#' @export
setMethod("sampleDistrFun", "RandomNbinomArraySeed", function(x) stats::qnbinom)

#' @export
setMethod("extract_array", "RandomNbinomArraySeed", function(x, index) {
    reindex <- .obtain_unique_sorted_index(index)
    arr <- sample_standard_uniform(dim(x), x@chunkdim, x@seeds, reindex$index)

    params <- lapply(sampleDistrParam(x), function(i) .extract_parameter(slot(x, i), reindex$index, dim(x)))
    if (!is.null(x@prob)) {
        params$prob <- .extract_parameter(x@prob, reindex$index, dim(x))
    }
    if (!is.null(x@mu)) {
        params$mu <- .extract_parameter(x@mu, reindex$index, dim(x))
    }

    arr <- .sample_distribution(arr, sampleDistrFun(x), params)
    .remap_to_original_index(arr, index, reindex)
})

#' @export
setMethod("matrixClass", "RandomNbinomArray", function(x) "RandomNbinomMatrix")

#' @export
#' @rdname RandomNbinomArray-class
setMethod("DelayedArray", "RandomNbinomArraySeed", function(seed) new_DelayedArray(seed, Class="RandomNbinomArray"))

#' @export
#' @rdname RandomNbinomArray-class
RandomNbinomArray <- function(dim, prob, size, mu, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomNbinomArraySeed(dim, prob=prob, size=size, mu=mu, chunkdim=chunkdim, sparse=sparse))
}
