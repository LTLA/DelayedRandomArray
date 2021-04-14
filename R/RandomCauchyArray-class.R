#' DelayedArray of chunked Cauchy-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of Cauchy-distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param location,scale Numeric vector used as the argument of the same name in \code{\link{qcauchy}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomCauchyArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomCauchyArray object,
#' containing random draws from a Cauchy distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomCauchyArray-class
#' RandomCauchyArraySeed-class
#' sampleDistrParam,RandomCauchyArraySeed-method
#' sampleDistrFun,RandomCauchyArraySeed-method
#' matrixClass,RandomCauchyArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomCauchyArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomCauchyArraySeed(c(1e5, 1e5), location=runif(1e5))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' loc <- rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- RandomCauchyArraySeed(c(1e5, 1e5), location=loc)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomCauchyArray-class
NULL

#' @export
#' @rdname RandomCauchyArray-class
RandomCauchyArraySeed <- function(dim, location=0, scale=1, chunkdim=NULL) {
    new("RandomCauchyArraySeed", dim=dim, location=location, scale=scale, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomCauchyArraySeed", function(x) c("location", "scale"))

#' @export
setMethod("sampleDistrFun", "RandomCauchyArraySeed", function(x) stats::qcauchy)

#' @export
setMethod("matrixClass", "RandomCauchyArray", function(x) "RandomCauchyMatrix")

#' @export
#' @rdname RandomCauchyArray-class
setMethod("DelayedArray", "RandomCauchyArraySeed", function(seed) new_DelayedArray(seed, Class="RandomCauchyArray"))

#' @export
#' @rdname RandomCauchyArray-class
RandomCauchyArray <- function(dim, location=0, scale=1, chunkdim=NULL) {
    DelayedArray(RandomCauchyArraySeed(dim, location=location, scale=scale, chunkdim=chunkdim))
}
