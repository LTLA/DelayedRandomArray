#' DelayedArray of random Poisson values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of Poisson distributed values.
#'
#' @inheritParams RandomUniformArray
#' @param seed A RandomPoissonArraySeed object.
#' @param lambda Numeric vector used as \code{lambda} in \code{\link{qpois}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled matrix should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a ChunkPoissonArray object,
#' containing random draws from a Poisson distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomPoissonArray-class
#' RandomPoissonArraySeed-class
#' sampleDistrParam,RandomPoissonArraySeed-method
#' sampleDistrFun,RandomPoissonArraySeed-method
#' matrixClass,RandomPoissonArraySeed-method
#' is_sparse,RandomPoissonArraySeed-method
#' extract_sparse_array,RandomPoissonArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomRandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomPoissonArraySeed(c(1e5, 1e5), lambda=2)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomPoissonArraySeed(c(1e5, 1e5), lambda=runif(1e5), sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' lambda <- rsparsematrix(1e5, 1e5, density=0.00001)
#' lambda <- DelayedArray(lambda) + 0.1
#' X3 <- RandomPoissonArraySeed(c(1e5, 1e5), lambda=lambda)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomPoissonArray-class
NULL

#' @export
#' @rdname RandomPoissonArray-class
RandomPoissonArraySeed <- function(dim, lambda, chunkdim=NULL, sparse=FALSE) {
    new("RandomPoissonArraySeed", dim=dim, lambda=lambda, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomPoissonArraySeed", function(x) c("lambda"))

#' @export
setMethod("sampleDistrFun", "RandomPoissonArraySeed", function(x) stats::qpois)

#' @export
setMethod("matrixClass", "RandomPoissonArray", function(x) "RandomPoissonMatrix")

#' @export
#' @rdname RandomPoissonArray-class
setMethod("DelayedArray", "RandomPoissonArraySeed", function(seed) new_DelayedArray(seed, Class="RandomPoissonArray"))

#' @export
#' @rdname RandomPoissonArray-class
RandomPoissonArray <- function(dim, lambda, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomPoissonArraySeed(dim, lambda, chunkdim=chunkdim, sparse=sparse))
}
