#' DelayedArray of random F-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of F-distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param df,ncp Numeric vector used as the argument of the same name in \code{\link{qf}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#'
#' If \code{ncp} is missing, a central T distribution is assumed.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomTArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomTArray object,
#' containing random draws from a exponential distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomTArray-class
#' RandomTArraySeed-class
#' RandomTMatrix-class
#' sampleDistrParam,RandomTArraySeed-method
#' sampleDistrFun,RandomTArraySeed-method
#' matrixClass,RandomTArray-method
#' extract_array,RandomTArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomTArraySeed(c(1e5, 1e5), df=10)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomTArraySeed(c(1e5, 1e5), df=sample(20, 1e5, replace=TRUE))
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' ncp <- rsparsematrix(1e5, 1e5, density=0.00001)
#' ncp <- abs(DelayedArray(ncp)) + 1
#' X3 <- RandomTArraySeed(c(1e5, 1e5), df=10, ncp=ncp)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomTArray-class
NULL

#' @export
#' @rdname RandomTArray-class
RandomTArraySeed <- function(dim, df, ncp, chunkdim=NULL) {
    if (missing(ncp)) {
        ncp <- NULL
    }
    new("RandomTArraySeed", dim=dim, df=df, ncp=ncp, chunkdim=chunkdim)
}

setValidity2("RandomTArraySeed", function(object) {
    if (!is.null(object@ncp)) {
        msg <- .is_valid_param(object@dim, object@ncp, "ncp")
        if (!is.null(msg)) {
            return(msg)
        }
    }
    TRUE
})

#' @export
setMethod("sampleDistrParam", "RandomTArraySeed", function(x) "df")

#' @export
setMethod("sampleDistrFun", "RandomTArraySeed", function(x) stats::qt)

#' @export
setMethod("extract_array", "RandomTArraySeed", .ncp_extract_array)

#' @export
setMethod("matrixClass", "RandomTArray", function(x) "RandomTMatrix")

#' @export
#' @rdname RandomTArray-class
setMethod("DelayedArray", "RandomTArraySeed", function(seed) new_DelayedArray(seed, Class="RandomTArray"))

#' @export
#' @rdname RandomTArray-class
RandomTArray <- function(dim, df, ncp, chunkdim=NULL) {
    DelayedArray(RandomTArraySeed(dim, df=df, ncp=ncp, chunkdim=chunkdim))
}
