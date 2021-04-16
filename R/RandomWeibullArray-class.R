#' DelayedArray of random Weibull-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of Weibull-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomWeibullArraySeed object.
#' @param shape,scale Numeric vector used as the argument of the same name in \code{\link{qweibull}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' 
#' @return 
#' All constructors return an instance of a RandomWeibullArray object,
#' containing random draws from a Weibull distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomWeibullArray-class
#' RandomWeibullArraySeed-class
#' RandomWeibullMatrix-class
#' sampleDistrParam,RandomWeibullArraySeed-method
#' sampleDistrFun,RandomWeibullArraySeed-method
#' matrixClass,RandomWeibullArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomWeibullArraySeed(c(1e5, 1e5), shape=10)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomWeibullArraySeed(c(1e5, 1e5), shape=round(runif(1e5, 10, 20)))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' shape <- round(RandomUnifArray(c(1e5, 1e5), 10, 20))
#' X3 <- RandomWeibullArraySeed(c(1e5, 1e5), shape=shape)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomWeibullArray-class
NULL

#' @export
#' @rdname RandomWeibullArray-class
RandomWeibullArraySeed <- function(dim, shape, scale=1, chunkdim=NULL) {
    new("RandomWeibullArraySeed", dim=dim, shape=shape, scale=scale, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomWeibullArraySeed", function(x) c("shape", "scale"))

#' @export
setMethod("sampleDistrFun", "RandomWeibullArraySeed", function(x) stats::qweibull)

#' @export
setMethod("matrixClass", "RandomWeibullArray", function(x) "RandomWeibullMatrix")

#' @export
#' @rdname RandomWeibullArray-class
setMethod("DelayedArray", "RandomWeibullArraySeed", function(seed) new_DelayedArray(seed, Class="RandomWeibullArray"))

#' @export
#' @rdname RandomWeibullArray-class
RandomWeibullArray <- function(dim, shape, scale=1, chunkdim=NULL) {
    DelayedArray(RandomWeibullArraySeed(dim, shape=shape, scale=scale, chunkdim=chunkdim))
}
