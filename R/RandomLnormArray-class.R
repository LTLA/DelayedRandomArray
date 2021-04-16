#' DelayedArray of random log-normal values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of log-normally distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param meanlog,sdlog Numeric vector used as the argument of the same name in \code{\link{qlnorm}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomLnormArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomLnormArray object,
#' containing random draws from a log-normal distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomLnormArray-class
#' RandomLnormArraySeed-class
#' RandomLnormMatrix-class
#' sampleDistrParam,RandomLnormArraySeed-method
#' sampleDistrFun,RandomLnormArraySeed-method
#' matrixClass,RandomLnormArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomLnormArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomLnormArraySeed(c(1e5, 1e5), meanlog=runif(1e5), sdlog=runif(1e5))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' meanlog <- rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- RandomLnormArraySeed(c(1e5, 1e5), meanlog=meanlog)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomLnormArray-class
NULL

#' @export
#' @rdname RandomLnormArray-class
RandomLnormArraySeed <- function(dim, meanlog=0, sdlog=1, chunkdim=NULL) {
    new("RandomLnormArraySeed", dim=dim, meanlog=meanlog, sdlog=sdlog, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomLnormArraySeed", function(x) c("meanlog", "sdlog"))

#' @export
setMethod("sampleDistrFun", "RandomLnormArraySeed", function(x) stats::qlnorm)

#' @export
setMethod("matrixClass", "RandomLnormArray", function(x) "RandomLnormMatrix")

#' @export
#' @rdname RandomLnormArray-class
setMethod("DelayedArray", "RandomLnormArraySeed", function(seed) new_DelayedArray(seed, Class="RandomLnormArray"))

#' @export
#' @rdname RandomLnormArray-class
RandomLnormArray <- function(dim, meanlog=0, sdlog=1, chunkdim=NULL) {
    DelayedArray(RandomLnormArraySeed(dim, meanlog, sdlog, chunkdim=chunkdim))
}
