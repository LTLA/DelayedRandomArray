#' DelayedArray of random geometric-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of geometric-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomGeomArraySeed object.
#' @param prob Numeric vector used as \code{prob} in \code{\link{qgeom}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled matrix should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomGeomArray object,
#' containing random draws from a geometric distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomGeomArray-class
#' RandomGeomArraySeed-class
#' RandomGeomMatrix-class
#' sampleDistrParam,RandomGeomArraySeed-method
#' sampleDistrFun,RandomGeomArraySeed-method
#' matrixClass,RandomGeomArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomGeomArraySeed(c(1e5, 1e5), prob=0.5)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomGeomArraySeed(c(1e5, 1e5), prob=runif(1e5, 0, 0.1), sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' prob <- RandomUnifArray(c(1e5, 1e5))
#' X3 <- RandomGeomArraySeed(c(1e5, 1e5), prob=prob)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomGeomArray-class
NULL

#' @export
#' @rdname RandomGeomArray-class
RandomGeomArraySeed <- function(dim, prob, chunkdim=NULL, sparse=FALSE) {
    new("RandomGeomArraySeed", dim=dim, prob=prob, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomGeomArraySeed", function(x) "prob")

#' @export
setMethod("sampleDistrFun", "RandomGeomArraySeed", function(x) stats::qgeom)

#' @export
setMethod("matrixClass", "RandomGeomArray", function(x) "RandomGeomMatrix")

#' @export
#' @rdname RandomGeomArray-class
setMethod("DelayedArray", "RandomGeomArraySeed", function(seed) new_DelayedArray(seed, Class="RandomGeomArray"))

#' @export
#' @rdname RandomGeomArray-class
RandomGeomArray <- function(dim, prob, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomGeomArraySeed(dim, prob=prob, chunkdim=chunkdim, sparse=sparse))
}
