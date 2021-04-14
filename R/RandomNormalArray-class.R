#' DelayedArray of chunked normal values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of normally distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param mean,sd Numeric vector used as \code{mean} and \code{sd}, respectively, in \code{\link{qnorm}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomNormalArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomNormalArray object,
#' containing random draws from a normal distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomNormalArray-class
#' RandomNormalArraySeed-class
#' sampleDistrParam,RandomNormalArraySeed-method
#' sampleDistrFun,RandomNormalArraySeed-method
#' matrixClass,RandomNormalArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomNormalArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomNormalArraySeed(c(1e5, 1e5), mean=runif(1e5), sd=runif(1e5))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' mean <- rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- RandomNormalArraySeed(c(1e5, 1e5), mean=mean)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomNormalArray-class
NULL

#' @export
#' @rdname RandomNormalArray-class
RandomNormalArraySeed <- function(dim, mean=0, sd=1, chunkdim=NULL) {
    new("RandomNormalArraySeed", dim=dim, mean=mean, sd=sd, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomNormalArraySeed", function(x) c("mean", "sd"))

#' @export
setMethod("sampleDistrFun", "RandomNormalArraySeed", function(x) stats::qnorm)

#' @export
setMethod("matrixClass", "RandomNormalArray", function(x) "RandomNormalMatrix")

#' @export
#' @rdname RandomNormalArray-class
setMethod("DelayedArray", "RandomNormalArraySeed", function(seed) new_DelayedArray(seed, Class="RandomNormalArray"))

#' @export
#' @rdname RandomNormalArray-class
RandomNormalArray <- function(dim, mean=0, sd=1, chunkdim=NULL) {
    DelayedArray(RandomNormalArraySeed(dim, mean, sd, chunkdim=chunkdim))
}
