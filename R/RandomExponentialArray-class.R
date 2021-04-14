#' DelayedArray of chunked exponential values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of exponentially distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param rate Numeric vector used as \code{rate} in \code{\link{qexp}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomExponentialArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomExponentialArray object,
#' containing random draws from a exponential distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomExponentialArray-class
#' RandomExponentialArraySeed-class
#' sampleDistrParam,RandomExponentialArraySeed-method
#' sampleDistrFun,RandomExponentialArraySeed-method
#' matrixClass,RandomExponentialArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomRandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomExponentialArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomExponentialArraySeed(c(1e5, 1e5), rate=runif(1e5))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' rate <- rsparsematrix(1e5, 1e5, density=0.00001)
#' rate <- abs(DelayedArray(rate)) + 1
#' X3 <- RandomExponentialArraySeed(c(1e5, 1e5), rate=rate)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomExponentialArray-class
NULL

#' @export
#' @rdname RandomExponentialArray-class
RandomExponentialArraySeed <- function(dim, rate=1, chunkdim=NULL) {
    new("RandomExponentialArraySeed", dim=dim, rate=rate, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomExponentialArraySeed", function(x) "rate")

#' @export
setMethod("sampleDistrFun", "RandomExponentialArraySeed", function(x) stats::qexp)

#' @export
setMethod("matrixClass", "RandomExponentialArray", function(x) "RandomExponentialMatrix")

#' @export
#' @rdname RandomExponentialArray-class
setMethod("DelayedArray", "RandomExponentialArraySeed", function(seed) new_DelayedArray(seed, Class="RandomExponentialArray"))

#' @export
#' @rdname RandomExponentialArray-class
RandomExponentialArray <- function(dim, rate=1, chunkdim=NULL) {
    DelayedArray(RandomExponentialArraySeed(dim, rate, chunkdim=chunkdim))
}
