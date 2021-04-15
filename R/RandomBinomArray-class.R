#' DelayedArray of random binomial values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of binomial-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomBinomArraySeed object.
#' @param lambda Numeric vector used as \code{lambda} in \code{\link{qpois}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled matrix should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomBinomArray object,
#' containing random draws from a binomial distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomBinomArray-class
#' RandomBinomArraySeed-class
#' RandomBinomMatrix-class
#' sampleDistrParam,RandomBinomArraySeed-method
#' sampleDistrFun,RandomBinomArraySeed-method
#' matrixClass,RandomBinomArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomBinomArraySeed(c(1e5, 1e5), size=10, prob=0.5)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomBinomArraySeed(c(1e5, 1e5), size=10, 
#'      prob=runif(1e5, 0, 0.1), sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' size <- rsparsematrix(1e5, 1e5, density=0.00001)
#' size <- round(abs(DelayedArray(size)) * 10)
#' X3 <- RandomBinomArraySeed(c(1e5, 1e5), size=size, prob=0.5)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomBinomArray-class
NULL

#' @export
#' @rdname RandomBinomArray-class
RandomBinomArraySeed <- function(dim, size, prob, chunkdim=NULL, sparse=FALSE) {
    new("RandomBinomArraySeed", dim=dim, size=size, prob=prob, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomBinomArraySeed", function(x) c("size", "prob"))

#' @export
setMethod("sampleDistrFun", "RandomBinomArraySeed", function(x) stats::qbinom)

#' @export
setMethod("matrixClass", "RandomBinomArray", function(x) "RandomBinomMatrix")

#' @export
#' @rdname RandomBinomArray-class
setMethod("DelayedArray", "RandomBinomArraySeed", function(seed) new_DelayedArray(seed, Class="RandomBinomArray"))

#' @export
#' @rdname RandomBinomArray-class
RandomBinomArray <- function(dim, size, prob, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomBinomArraySeed(dim, size, prob, chunkdim=chunkdim, sparse=sparse))
}
