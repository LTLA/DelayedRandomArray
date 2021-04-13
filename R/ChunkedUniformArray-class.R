#' DelayedArray of chunked uniform values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of uniformly distributed values.
#' Sampling is done within chunks for rapid access to any subarray.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param min,max Numeric vector used as \code{min} and \code{max}, respectively, in \code{\link{qunif}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A ChunkedUniformArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a ChunkUniformArray object,
#' containing random draws from a uniform distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' ChunkedUniformArray-class
#' ChunkedUniformArraySeed-class
#' sampleDistrParam,ChunkedUniformArraySeed-method
#' sampleDistrFun,ChunkedUniformArraySeed-method
#' matrixClass,ChunkedUniformArraySeed-method
#'
#' @seealso
#' The \linkS4class{ChunkedRandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- ChunkedUniformArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- ChunkedUniformArraySeed(c(1e5, 1e5), min=1:1e5, max=1:1e5*2)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' min <- rsparsematrix(1e5, 1e5, density=0.00001)
#' max <- min + rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- ChunkedUniformArraySeed(c(1e5, 1e5), min=min, max=max)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name ChunkedUniformArray-class
NULL

#' @export
#' @rdname ChunkedUniformArray-class
ChunkedUniformArraySeed <- function(dim, min=0, max=1, chunkdim=NULL) {
    new("ChunkedUniformArraySeed", dim=dim, min=min, max=max, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "ChunkedUniformArraySeed", function(x) c("min", "max"))

#' @export
#' @importFrom stats qunif
setMethod("sampleDistrFun", "ChunkedUniformArraySeed", function(x) qunif)

#' @export
setMethod("matrixClass", "ChunkedUniformArray", function(x) "ChunkedUniformMatrix")

#' @export
#' @rdname ChunkedUniformArray-class
setMethod("DelayedArray", "ChunkedUniformArraySeed", function(seed) new_DelayedArray(seed, Class="ChunkedUniformArray"))

#' @export
#' @rdname ChunkedUniformArray-class
ChunkedUniformArray <- function(dim, min=0, max=1, chunkdim=NULL) {
    DelayedArray(ChunkedUniformArraySeed(dim, min, max, chunkdim=chunkdim))
}
