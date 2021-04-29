#' DelayedArray of random hypergeometric-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of hypergeometric-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomHyperArraySeed object.
#' @param m,n,k Numeric vector used as the argument of the same name in \code{\link{qhyper}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled array should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomHyperArray object,
#' containing random draws from a hypergeometric distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomHyperArray-class
#' RandomHyperArraySeed-class
#' RandomHyperMatrix-class
#' sampleDistrParam,RandomHyperArraySeed-method
#' sampleDistrFun,RandomHyperArraySeed-method
#' matrixClass,RandomHyperArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomHyperArraySeed(c(1e5, 1e5), m=10, n=20, k=15)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomHyperArraySeed(c(1e5, 1e5), m=round(runif(1e5, 10, 20)), 
#'     n=20, k=15, sparse=TRUE)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' m <- round(RandomUnifArray(c(1e5, 1e5), 10, 20))
#' X3 <- RandomHyperArraySeed(c(1e5, 1e5), m=m, n=50, k=20)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomHyperArray-class
NULL

#' @export
#' @rdname RandomHyperArray-class
RandomHyperArraySeed <- function(dim, m, n, k, chunkdim=NULL, sparse=FALSE) {
    new("RandomHyperArraySeed", dim=dim, m=m, n=n, k=k, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomHyperArraySeed", function(x) c("m", "n", "k"))

#' @export
setMethod("sampleDistrFun", "RandomHyperArraySeed", function(x) stats::qhyper)

#' @export
setMethod("matrixClass", "RandomHyperArray", function(x) "RandomHyperMatrix")

#' @export
#' @rdname RandomHyperArray-class
setMethod("DelayedArray", "RandomHyperArraySeed", function(seed) new_DelayedArray(seed, Class="RandomHyperArray"))

#' @export
#' @rdname RandomHyperArray-class
RandomHyperArray <- function(dim, m, n, k, chunkdim=NULL, sparse=FALSE) {
    DelayedArray(RandomHyperArraySeed(dim, m=m, n=n, k=k, chunkdim=chunkdim, sparse=sparse))
}
