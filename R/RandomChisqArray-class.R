#' DelayedArray of random chi-squared-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of chi-squared-distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param df,ncp Numeric vector used as the argument of the same name in \code{\link{qchisq}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomChisqArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomChisqArray object,
#' containing random draws from a chi-squared distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomChisqArray-class
#' RandomChisqArraySeed-class
#' RandomChisqMatrix-class
#' sampleDistrParam,RandomChisqArraySeed-method
#' sampleDistrFun,RandomChisqArraySeed-method
#' matrixClass,RandomChisqArray-method
#' extract_array,RandomChisqArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomChisqArraySeed(c(1e5, 1e5), df=5)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomChisqArraySeed(c(1e5, 1e5), df=runif(1e5)*20)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' df <- rsparsematrix(1e5, 1e5, density=0.00001)
#' df <- abs(DelayedArray(df) + 1) * 10
#' X3 <- RandomChisqArraySeed(c(1e5, 1e5), df=df)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomChisqArray-class
NULL

#' @export
#' @rdname RandomChisqArray-class
RandomChisqArraySeed <- function(dim, df, ncp=0, chunkdim=NULL) {
    if (missing(ncp)) {
        ncp <- NULL
    }
    new("RandomChisqArraySeed", dim=dim, df=df, ncp=ncp, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomChisqArraySeed", function(x) "df")

#' @export
setMethod("sampleDistrFun", "RandomChisqArraySeed", function(x) stats::qchisq)

#' @export
setMethod("extract_array", "RandomChisqArraySeed", .ncp_extract_array)

#' @export
setMethod("matrixClass", "RandomChisqArray", function(x) "RandomChisqMatrix")

#' @export
#' @rdname RandomChisqArray-class
setMethod("DelayedArray", "RandomChisqArraySeed", function(seed) new_DelayedArray(seed, Class="RandomChisqArray"))

#' @export
#' @rdname RandomChisqArray-class
RandomChisqArray <- function(dim, df, ncp=0, chunkdim=NULL) {
    if (missing(ncp)) {
        # Preserve missingness.
        DelayedArray(RandomChisqArraySeed(dim, df=df, chunkdim=chunkdim))
    } else {
        DelayedArray(RandomChisqArraySeed(dim, df=df, ncp=ncp, chunkdim=chunkdim))
    }
}
