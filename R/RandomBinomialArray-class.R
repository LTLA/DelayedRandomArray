#' DelayedArray of random binomial values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of binomial-distributed values.
#'
#' @inheritParams RandomUniformArray
#' @param seed A RandomBinomialArraySeed object.
#' @param lambda Numeric vector used as \code{lambda} in \code{\link{qpois}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled matrix should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomBinomialArray object,
#' containing random draws from a binomial distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomBinomialArray-class
#' RandomBinomialArraySeed-class
#' sampleDistrParam,RandomBinomialArraySeed-method
#' sampleDistrFun,RandomBinomialArraySeed-method
#' matrixClass,RandomBinomialArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomBinomialArraySeed(c(1e5, 1e5), size=10, prob=0.5)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomBinomialArraySeed(c(1e5, 1e5), size=10, 
#'      prob=runif(1e5, 0, 0.1), sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' size <- rsparsematrix(1e5, 1e5, density=0.00001)
#' size <- round(abs(DelayedArray(size)) * 10)
#' X3 <- RandomBinomialArraySeed(c(1e5, 1e5), size=size, prob=0.5)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomBinomialArray-class
NULL

#' @export
#' @rdname RandomBinomialArray-class
RandomBinomialArraySeed <- function(dim, size, prob, chunkdim=NULL, sparse=FALSE) {
    new("RandomBinomialArraySeed", dim=dim, size=size, prob=prob, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomBinomialArraySeed", function(x) c("size", "prob"))

#' @export
setMethod("sampleDistrFun", "RandomBinomialArraySeed", function(x) stats::qbinom)

#' @export
setMethod("matrixClass", "RandomBinomialArray", function(x) "RandomBinomialMatrix")

#' @export
#' @rdname RandomBinomialArray-class
setMethod("DelayedArray", "RandomBinomialArraySeed", function(seed) new_DelayedArray(seed, Class="RandomBinomialArray"))

#' @export
#' @rdname RandomBinomialArray-class
RandomBinomialArray <- function(dim, size, prob, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomBinomialArraySeed(dim, size, prob, chunkdim=chunkdim, sparse=sparse))
}
