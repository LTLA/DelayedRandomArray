#' DelayedArray of chunked uniform values
#'
#' A \linkS4class{DelayedArray} subclass that performs on-the-fly sampling of uniformly distributed values.
#' Sampling is done within chunks for rapid access to any subarray.
#'
#' @param dim Integer vector of positive length, specifying the dimensions of the array.
#' @param min,max Numeric vector used as \code{min} and \code{max}, respectively, in \code{\link{qunif}}.
#' Alternatively, a numeric array-like object with the same dimensions as \code{dim}.
#' @param chunkdim Integer vector of length equal to \code{dim}, containing the dimensions of each chunk.
#' @param seed A ChunkedUniformArraySeed object.
#' 
#' @return 
#' All constructors return an instance of a ChunkUniformArray object,
#' containing random draws from a uniform distribution with the specified parameters.
#'
#' @section Distributional parameters:
#' Distributional parameters are passed to the relevant quantile function to obtain a random value from the desired distribution.
#' Each parameter can be:
#' \itemize{
#' \item A numeric scalar, which is used throughout the array.
#' \item A numeric vector, which is recycled along the length of the array.
#' This traverses the array along the first dimension, then the second, then the third, and so on;
#' for matrices, this is equivalent to column-major ordering.
#' \item A numeric array-like object of the same dimensions as \code{dim},
#' where each entry contains the parameter value for the corresponding entry of the output array.
#' This can be another \linkS4class{DelayedArray} object.
#' }
#'
#' @author Aaron Lun
#' 
#' @aliases
#' ChunkedUniformArray-class
#' ChunkedUniformArraySeed-class
#' extract_array,ChunkedUniformArraySeed-method
#' matrixClass,ChunkedUniformArraySeed-method
#' chunkdim,ChunkedUniformArraySeed-method
#' 
#' @examples
#' X <- ChunkedUniformArraySeed(c(1e5, 1e5))
#' Y <- DelayedArray(X)
#' Y
#'
#' # Fiddling with the distribution parameters:
#' X2 <- ChunkedUniformArraySeed(c(1e5, 1e5), min=1:1e5, max=1:1e5*2)
#' Y2 <- DelayedArray(X2)
#' Y2
#'
#' # Using another array as input:
#' library(Matrix)
#' min <- rsparsematrix(1e5, 1e5, density=0.00001)
#' max <- min + rsparsematrix(1e5, 1e5, density=0.00001)
#' X3 <- ChunkedUniformArraySeed(c(1e5, 1e5), min=min, max=max)
#' Y3 <- DelayedArray(X3)
#' Y3
#' 
#' @docType class
#' @name ChunkedUniformArray-class
NULL

#' @export
#' @import DelayedArray
setClass("ChunkedUniformArraySeed", slots=c(dim="integer", min="ANY", max="ANY", chunkdim="integer", seeds="list"))

#' @export
#' @importFrom dqrng generateSeedVectors
#' @rdname ChunkedUniformArray-class
ChunkedUniformArraySeed <- function(dim, min=0, max=1, chunkdim=NULL) {
    dim <- as.integer(dim)

    if (is.null(chunkdim)) {
        # Figure out a better default choice here.
        chunkdim <- rep(100L, length(dim))
    }
    chunkdim <- pmin(as.integer(chunkdim), dim)

    nchunks <- prod(ceiling(dim/chunkdim))
    seeds <- generateSeedVectors(nchunks)
    
    new("ChunkedUniformArraySeed", dim=dim, min=min, max=max, chunkdim=chunkdim, seeds=seeds)
}

setValidity2("ChunkedUniformArraySeed", function(object) {
    dims <- object@dim
    if (any(dims < 0)) {
        return("'dim' must contain non-negative integers")
    }

    msg <- .is_valid_param(dims, object@min, "min")
    if (!is.null(msg)) {
        return(msg)
    }

    msg <- .is_valid_param(dims, object@max, "max")
    if (!is.null(msg)) {
        return(msg)
    }

    msg <- .is_valid_chunkdim(dims, object@chunkdim)
    if (!is.null(msg)) {
        return(msg)
    }

    TRUE
})

#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib DelayedRandomArray
setMethod("extract_array", "ChunkedUniformArraySeed", function(x, index) {
    reindex <- .obtain_unique_sorted_index(index)
    arr <- sample_standard_uniform(dim(x), x@chunkdim, x@seeds, reindex$index)

    arr <- .sample_distribution(arr, qunif, 
        list(
            min=.extract_parameter(x@min, reindex$index, dim(x)),
            max=.extract_parameter(x@max, reindex$index, dim(x))
        )
    )

    .remap_to_original_index(arr, index, reindex)
})

#' @export
setMethod("chunkdim", "ChunkedUniformArraySeed", function(x) x@chunkdim)

#' @export
setClass("ChunkedUniformArray",
    contains="DelayedArray",
    representation(seed="ChunkedUniformArraySeed")
)

#' @export
setClass("ChunkedUniformMatrix",
    contains="DelayedMatrix",
    representation(seed="ChunkedUniformArraySeed")
)

#' @export
setMethod("matrixClass", "ChunkedUniformArray", function(x) "ChunkedUniformMatrix")

#' @export
#' @rdname ChunkedUniformArray-class
setMethod("DelayedArray", "ChunkedUniformArraySeed", function(seed) new_DelayedArray(seed, Class="ChunkedUniformArray"))

#' @export
#' @rdname ChunkedUniformArray-class
ChunkedUniformArray <- function(dim, min=0, max=1, chunkdim=NULL) {
    DelayedArray(ChunkedUniformArraySeed(dim, min, max, chunkdim=chunkdim))
}
