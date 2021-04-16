#' DelayedArray of random log-normal values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of log-normally distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param location,scale Numeric vector used as the argument of the same name in \code{\link{qlogis}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomLogisArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomLogisArray object,
#' containing random draws from a log-normal distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomLogisArray-class
#' RandomLogisArraySeed-class
#' RandomLogisMatrix-class
#' sampleDistrParam,RandomLogisArraySeed-method
#' sampleDistrFun,RandomLogisArraySeed-method
#' matrixClass,RandomLogisArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomLogisArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomLogisArraySeed(c(1e5, 1e5), location=runif(1e5), scale=runif(1e5))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' location <- rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- RandomLogisArraySeed(c(1e5, 1e5), location=location)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomLogisArray-class
NULL

#' @export
#' @rdname RandomLogisArray-class
RandomLogisArraySeed <- function(dim, location=0, scale=1, chunkdim=NULL) {
    new("RandomLogisArraySeed", dim=dim, location=location, scale=scale, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomLogisArraySeed", function(x) c("location", "scale"))

#' @export
setMethod("sampleDistrFun", "RandomLogisArraySeed", function(x) stats::qlogis)

#' @export
setMethod("matrixClass", "RandomLogisArray", function(x) "RandomLogisMatrix")

#' @export
#' @rdname RandomLogisArray-class
setMethod("DelayedArray", "RandomLogisArraySeed", function(seed) new_DelayedArray(seed, Class="RandomLogisArray"))

#' @export
#' @rdname RandomLogisArray-class
RandomLogisArray <- function(dim, location=0, scale=1, chunkdim=NULL) {
    DelayedArray(RandomLogisArraySeed(dim, location, scale, chunkdim=chunkdim))
}
