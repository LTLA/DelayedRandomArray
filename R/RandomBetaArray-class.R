#' DelayedArray of random beta-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of beta-distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param shape1,shape2,ncp Numeric vector used as the argument of the same name in \code{\link{qbeta}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomBetaArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomBetaArray object,
#' containing random draws from a beta distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomBetaArray-class
#' RandomBetaArraySeed-class
#' RandomBetaMatrix-class
#' sampleDistrParam,RandomBetaArraySeed-method
#' sampleDistrFun,RandomBetaArraySeed-method
#' extract_array,RandomBetaArraySeed-method
#' matrixClass,RandomBetaArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomBetaArraySeed(c(1e5, 1e5), shape1=1, shape2=10)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomBetaArraySeed(c(1e5, 1e5), shape1=runif(1e5), shape2=2)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' s1 <- rsparsematrix(1e5, 1e5, density=0.00001)
#' s1 <- abs(DelayedArray(s1)) + 1
#' X3 <- RandomBetaArraySeed(c(1e5, 1e5), shape1=s1, shape2=s1+1)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomBetaArray-class
NULL

#' @export
#' @rdname RandomBetaArray-class
RandomBetaArraySeed <- function(dim, shape1, shape2, ncp=0, chunkdim=NULL) {
    # Ensure that the corresponding internal routine in qbeta gets called if ncp is missing.
    if (missing(ncp)) {
        ncp <- NULL
    }
    new("RandomBetaArraySeed", dim=dim, shape1=shape1, shape2=shape2, ncp=ncp, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomBetaArraySeed", function(x) c("shape1", "shape2"))

#' @export
setMethod("sampleDistrFun", "RandomBetaArraySeed", function(x) stats::qbeta)

#' @export
setMethod("extract_array", "RandomBetaArraySeed", .ncp_extract_array)

#' @export
setMethod("matrixClass", "RandomBetaArray", function(x) "RandomBetaMatrix")

#' @export
#' @rdname RandomBetaArray-class
setMethod("DelayedArray", "RandomBetaArraySeed", function(seed) new_DelayedArray(seed, Class="RandomBetaArray"))

#' @export
#' @rdname RandomBetaArray-class
RandomBetaArray <- function(dim, shape1, shape2, ncp=0, chunkdim=NULL) {
    if (missing(ncp)) {
        # Preserve missingness.
        seed <- RandomBetaArraySeed(dim, shape1, shape2, chunkdim=chunkdim)
    } else {
        seed <- RandomBetaArraySeed(dim, shape1, shape2, ncp=ncp, chunkdim=chunkdim)
    }
    DelayedArray(seed)
}
