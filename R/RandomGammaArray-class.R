#' DelayedArray of random gamma-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of gamma-distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param shape,rate,scale Numeric vector used as the argument of the same name in \code{\link{qgamma}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#'
#' If \code{scale} is explicitly supplied, \code{rate} is ignored.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomGammaArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomGammaArray object,
#' containing random draws from a gamma distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomGammaArray-class
#' RandomGammaArraySeed-class
#' RandomGammaMatrix-class
#' sampleDistrParam,RandomGammaArraySeed-method
#' sampleDistrFun,RandomGammaArraySeed-method
#' matrixClass,RandomGammaArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomGammaArraySeed(c(1e5, 1e5), shape=1, rate=10)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomGammaArraySeed(c(1e5, 1e5), shape=runif(1e5), rate=2)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' s1 <- rsparsematrix(1e5, 1e5, density=0.00001)
#' s1 <- abs(DelayedArray(s1)) + 1
#' X3 <- RandomGammaArraySeed(c(1e5, 1e5), shape=s1, rate=s1+1)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomGammaArray-class
NULL

#' @export
#' @rdname RandomGammaArray-class
RandomGammaArraySeed <- function(dim, shape, rate=1, scale=1/rate, chunkdim=NULL) {
    # Don't bother checking if rate and scale are consistent,
    # as this takes too much effort if one or the other is a matrix.
    new("RandomGammaArraySeed", dim=dim, shape=shape, scale=scale, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomGammaArraySeed", function(x) c("shape", "scale"))

#' @export
setMethod("sampleDistrFun", "RandomGammaArraySeed", function(x) stats::qgamma)

#' @export
setMethod("matrixClass", "RandomGammaArray", function(x) "RandomGammaMatrix")

#' @export
#' @rdname RandomGammaArray-class
setMethod("DelayedArray", "RandomGammaArraySeed", function(seed) new_DelayedArray(seed, Class="RandomGammaArray"))

#' @export
#' @rdname RandomGammaArray-class
RandomGammaArray <- function(dim, shape, rate=1, scale=1/rate, chunkdim=NULL) {
    DelayedArray(RandomGammaArraySeed(dim, shape=shape, rate=rate, scale=scale, chunkdim=chunkdim))
}
