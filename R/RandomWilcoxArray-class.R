#' DelayedArray of random Wilcoxon-distributed values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of Wilcoxon-distributed values.
#'
#' @inheritParams RandomUnifArray
#' @param seed A RandomWilcoxArraySeed object.
#' @param m,n Numeric vector used as the argument of the same name in \code{\link{qwilcox}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param sparse Logical scalar indicating whether the sampled array should be treated as sparse. 
#' 
#' @return 
#' All constructors return an instance of a RandomWilcoxArray object,
#' containing random draws from a Wilcox distribution with the specified parameters.
#'
#' @author Aaron Lun
#' 
#' @aliases
#' RandomWilcoxArray-class
#' RandomWilcoxArraySeed-class
#' RandomWilcoxMatrix-class
#' sampleDistrParam,RandomWilcoxArraySeed-method
#' sampleDistrFun,RandomWilcoxArraySeed-method
#' matrixClass,RandomWilcoxArray-method
#'
#' @seealso
#' The \linkS4class{RandomArraySeed} class, for details on chunking and the distributional parameters.
#' 
#' @examples
#' X <- RandomWilcoxArraySeed(c(1e5, 1e5), m=10, n=20)
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- RandomWilcoxArraySeed(c(1e5, 1e5), m=round(runif(1e5, 10, 20)), n=20)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' m <- round(RandomUnifArray(c(1e5, 1e5), 10, 20))
#' X3 <- RandomWilcoxArraySeed(c(1e5, 1e5), m=m, n=50)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name RandomWilcoxArray-class
NULL

#' @export
#' @rdname RandomWilcoxArray-class
RandomWilcoxArraySeed <- function(dim, m, n, chunkdim=NULL, sparse=FALSE) {
    new("RandomWilcoxArraySeed", dim=dim, m=m, n=n, chunkdim=chunkdim, sparse=sparse)
}

#' @export
setMethod("sampleDistrParam", "RandomWilcoxArraySeed", function(x) c("m", "n"))

#' @export
setMethod("sampleDistrFun", "RandomWilcoxArraySeed", function(x) stats::qwilcox)

#' @export
setMethod("matrixClass", "RandomWilcoxArray", function(x) "RandomWilcoxMatrix")

#' @export
#' @rdname RandomWilcoxArray-class
setMethod("DelayedArray", "RandomWilcoxArraySeed", function(seed) new_DelayedArray(seed, Class="RandomWilcoxArray"))

#' @export
#' @rdname RandomWilcoxArray-class
RandomWilcoxArray <- function(dim, m, n, chunkdim=NULL) {
    DelayedArray(RandomWilcoxArraySeed(dim, m=m, n=n, chunkdim=chunkdim))
}
