#' DelayedArray of random uniform values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of uniformly distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param min,max Numeric vector used as \code{min} and \code{max}, respectively, in \code{\link{qunif}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomUniformArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomUniformArray object,
#' containing random draws from a uniform distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomUniformArray-class
#' RandomUniformArraySeed-class
#' sampleDistrParam,RandomUniformArraySeed-method
#' sampleDistrFun,RandomUniformArraySeed-method
#' matrixClass,RandomUniformArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomUniformArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomUniformArraySeed(c(1e5, 1e5), min=1:1e5, max=1:1e5*2)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' min <- rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- RandomUniformArraySeed(c(1e5, 1e5), min=min, max=DelayedArray(min)+1)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomUniformArray-class
NULL

#' @export
#' @rdname RandomUniformArray-class
RandomUniformArraySeed <- function(dim, min=0, max=1, chunkdim=NULL) {
    new("RandomUniformArraySeed", dim=dim, min=min, max=max, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomUniformArraySeed", function(x) c("min", "max"))

#' @export
setMethod("sampleDistrFun", "RandomUniformArraySeed", function(x) stats::qunif)

#' @export
setMethod("matrixClass", "RandomUniformArray", function(x) "RandomUniformMatrix")

#' @export
#' @rdname RandomUniformArray-class
setMethod("DelayedArray", "RandomUniformArraySeed", function(seed) new_DelayedArray(seed, Class="RandomUniformArray"))

#' @export
#' @rdname RandomUniformArray-class
RandomUniformArray <- function(dim, min=0, max=1, chunkdim=NULL) {
    DelayedArray(RandomUniformArraySeed(dim, min, max, chunkdim=chunkdim))
}
