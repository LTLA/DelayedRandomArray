#' DelayedArray of random uniform values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of uniformly distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param min,max Numeric vector used as \code{min} and \code{max}, respectively, in \code{\link{qunif}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomUnifArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomUnifArray object,
#' containing random draws from a uniform distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomUnifArray-class
#' RandomUnifArraySeed-class
#' RandomUnifMatrix-class
#' sampleDistrParam,RandomUnifArraySeed-method
#' sampleDistrFun,RandomUnifArraySeed-method
#' matrixClass,RandomUnifArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomUnifArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomUnifArraySeed(c(1e5, 1e5), min=1:1e5, max=1:1e5*2)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' min <- rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- RandomUnifArraySeed(c(1e5, 1e5), min=min, max=DelayedArray(min)+1)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomUnifArray-class
NULL

#' @export
#' @rdname RandomUnifArray-class
RandomUnifArraySeed <- function(dim, min=0, max=1, chunkdim=NULL) {
    new("RandomUnifArraySeed", dim=dim, min=min, max=max, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomUnifArraySeed", function(x) c("min", "max"))

#' @export
setMethod("sampleDistrFun", "RandomUnifArraySeed", function(x) stats::qunif)

#' @export
setMethod("matrixClass", "RandomUnifArray", function(x) "RandomUnifMatrix")

#' @export
#' @rdname RandomUnifArray-class
setMethod("DelayedArray", "RandomUnifArraySeed", function(seed) new_DelayedArray(seed, Class="RandomUnifArray"))

#' @export
#' @rdname RandomUnifArray-class
RandomUnifArray <- function(dim, min=0, max=1, chunkdim=NULL) {
    DelayedArray(RandomUnifArraySeed(dim, min, max, chunkdim=chunkdim))
}
