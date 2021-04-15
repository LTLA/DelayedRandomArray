#' DelayedArray of random Poisson values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of Poisson-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomPoisArraySeed object.
#' @param lambda Numeric vector used as \code{lambda} in \code{\link{qpois}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled matrix should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomPoisArray object,
#' containing random draws from a Poisson distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomPoisArray-class
#' RandomPoisArraySeed-class
#' sampleDistrParam,RandomPoisArraySeed-method
#' sampleDistrFun,RandomPoisArraySeed-method
#' matrixClass,RandomPoisArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomPoisArraySeed(c(1e5, 1e5), lambda=2)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomPoisArraySeed(c(1e5, 1e5), lambda=runif(1e5), sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' lambda <- rsparsematrix(1e5, 1e5, density=0.00001)
#' lambda <- abs(DelayedArray(lambda)) + 0.1
#' X3 <- RandomPoisArraySeed(c(1e5, 1e5), lambda=lambda)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomPoisArray-class
NULL

#' @export
#' @rdname RandomPoisArray-class
RandomPoisArraySeed <- function(dim, lambda, chunkdim=NULL, sparse=FALSE) {
    new("RandomPoisArraySeed", dim=dim, lambda=lambda, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomPoisArraySeed", function(x) c("lambda"))

#' @export
setMethod("sampleDistrFun", "RandomPoisArraySeed", function(x) stats::qpois)

#' @export
setMethod("matrixClass", "RandomPoisArray", function(x) "RandomPoissonMatrix")

#' @export
#' @rdname RandomPoisArray-class
setMethod("DelayedArray", "RandomPoisArraySeed", function(seed) new_DelayedArray(seed, Class="RandomPoissonArray"))

#' @export
#' @rdname RandomPoisArray-class
RandomPoisArray <- function(dim, lambda, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomPoisArraySeed(dim, lambda, chunkdim=chunkdim, sparse=sparse))
}
