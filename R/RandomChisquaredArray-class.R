#' DelayedArray of chunked chi-squared-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of chi-squared-distributed values.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param df,ncp Numeric vector used as the argument of the same name in \code{\link{qchisq}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A RandomChisquaredArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a RandomChisquaredArray object,
#' containing random draws from a chi-squared distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomChisquaredArray-class
#' RandomChisquaredArraySeed-class
#' sampleDistrParam,RandomChisquaredArraySeed-method
#' sampleDistrFun,RandomChisquaredArraySeed-method
#' matrixClass,RandomChisquaredArraySeed-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomChisquaredArraySeed(c(1e5, 1e5), df=5)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomChisquaredArraySeed(c(1e5, 1e5), df=runif(1e5)*20)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' df <- rsparsematrix(1e5, 1e5, density=0.00001)
#' df <- abs(DelayedArray(df) + 1) * 10
#' X3 <- RandomChisquaredArraySeed(c(1e5, 1e5), df=df)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomChisquaredArray-class
NULL

#' @export
#' @rdname RandomChisquaredArray-class
RandomChisquaredArraySeed <- function(dim, df, ncp=0, chunkdim=NULL) {
    new("RandomChisquaredArraySeed", dim=dim, df=df, ncp=ncp, chunkdim=chunkdim)
}

#' @export
setMethod("sampleDistrParam", "RandomChisquaredArraySeed", function(x) c("df", "ncp"))

#' @export
setMethod("sampleDistrFun", "RandomChisquaredArraySeed", function(x) stats::qchisq)

#' @export
setMethod("matrixClass", "RandomChisquaredArray", function(x) "RandomChisquaredMatrix")

#' @export
#' @rdname RandomChisquaredArray-class
setMethod("DelayedArray", "RandomChisquaredArraySeed", function(seed) new_DelayedArray(seed, Class="RandomChisquaredArray"))

#' @export
#' @rdname RandomChisquaredArray-class
RandomChisquaredArray <- function(dim, df, ncp=0, chunkdim=NULL) {
    DelayedArray(RandomChisquaredArraySeed(dim, df=df, ncp=ncp, chunkdim=chunkdim))
}
