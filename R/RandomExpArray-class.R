#' DelayedArray of chunked exponential values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of exponentially distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param rate Numeric vector used as \code{rate} in \code{\link{qexp}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomExpArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomExpArray object,
#' containing random draws from a exponential distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomExpArray-class
#' RandomExpArraySeed-class
#' sampleDistrParam,RandomExpArraySeed-method
#' sampleDistrFun,RandomExpArraySeed-method
#' matrixClass,RandomExpArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomExpArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomExpArraySeed(c(1e5, 1e5), rate=runif(1e5))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' rate <- rsparsematrix(1e5, 1e5, density=0.00001)
#' rate <- abs(DelayedArray(rate)) + 1
#' X3 <- RandomExpArraySeed(c(1e5, 1e5), rate=rate)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomExpArray-class
NULL

#' @export
#' @rdname RandomExpArray-class
RandomExpArraySeed <- function(dim, rate=1, chunkdim=NULL) {
    new("RandomExpArraySeed", dim=dim, rate=rate, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomExpArraySeed", function(x) "rate")

#' @export
setMethod("sampleDistrFun", "RandomExpArraySeed", function(x) stats::qexp)

#' @export
setMethod("matrixClass", "RandomExpArray", function(x) "RandomExponentialMatrix")

#' @export
#' @rdname RandomExpArray-class
setMethod("DelayedArray", "RandomExpArraySeed", function(seed) new_DelayedArray(seed, Class="RandomExponentialArray"))

#' @export
#' @rdname RandomExpArray-class
RandomExpArray <- function(dim, rate=1, chunkdim=NULL) {
    DelayedArray(RandomExpArraySeed(dim, rate, chunkdim=chunkdim))
}
