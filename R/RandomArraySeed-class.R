#' A DelayedArray seed supplying chunked random values
#'
#' The RandomArraySeed is a \linkS4class{DelayedArray} seed that performs reproducible, on-demand sampling of randomly distributed values.
#' Note that this is a virtual class; the intention is to define concrete subclasses corresponding to specific parameterized distributions.
#'
#' @section Chunking dimensions:
#' The array is conceptually partitioned into contiguous chunks of the same shape.
#' The random values in each chunk are initialized with a different seed and stream via the PCG32 pseudo-random number generator (see the \pkg{dqrng} package for details).
#' This design allows us to rapidly access any given subarray without having to do jump-aheads from the start of the stream.
#'
#' The default chunking dimensions are set to the square root of the array dimensions - or 100, whichever is larger.
#' This scheme provides decent though suboptimal performance along any dimension.
#' If the access pattern is known beforehand, a better chunking scheme can often be chosen and passed to the \code{chunkdim} argument.
#'
#' Note that changing the chunking dimensions will change the ordering of array values, even if the seeds are unchanged.
#' This may be unexpected, given that chunking in real datasets will never change the data, only the performance of access operations.
#' However, it is largely unavoidable in this context as the random number stream is rearranged within the array.
#'
#' The \code{\link{chunkdim}(x)} method will return the chunk dimensions of a RandomArraySeed instance \code{x}.
#' This will be used by the \pkg{DelayedArray} machinery to optimize block processing by extracting whole chunks where possible.
#'
#' @section Implementing subclasses:
#' To sample from a specific distribution, we can implement a concrete subclass of the RandomArraySeed.
#' This is done by implementing methods for \code{sampleDistrFun} and \code{sampleDistrParam}.
#'
#' In the code chunks below, \code{x} is an instance of a RandomArraySeed subclass:
#' \itemize{
#' \item \code{sampleDistrFun(x)} returns a quantile function that accepts a vector of cumulative probabilities \code{p} and returns a numeric vector of quantiles.
#' A typical example is \code{\link{qnorm}}, though similar functions from the \pkg{stats} package can also be used.
#' The output vector should be the same length as \code{p}; any other distributional parameters should be recycled to the length of \code{p}.
#' \item \code{sampleDistrParam(x)} returns a character vector specifying the names of the distributional parameters as slots of \code{x}.
#' For example, for a subclass that samples from a normal distribution, this might be \code{"mean"} and \code{"sd"}.
#' Each distributional parameter is expected to be numeric. 
#' }
#'
#' The \code{extract_array} method for the RandomArraySeed will automatically use both of the above methods to sample from the specified distribution.
#' This is achieved by randomly sampling from a standard uniform distribution, treating the values as probabilities and converting them into quantiles.
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
#' @section Representing sparsity:
#' For certain distributions, we may expect a large number of zeroes in the random output.
#' We provide the \emph{option} to treat the sampled values as being sparse, by setting \code{sparse=TRUE} in the constructors of the relevant subclasses.
#' This is optional as most distributions will not yield sparse arrays for most of their possible parameter space.
#'
#' When \code{sparse=TRUE}, the block processing machinery in \pkg{DelayedArray} will return a sparse array. 
#' This gives downstream applications the opportunity to use more efficient sparse algorithms when relevant.
#' However, this option does not affect the sampling itself; the result is always the same as a dense array, just that the output is coerced into a \linkS4class{SparseArraySeed}.
#'
#' We can determine whether a RandomArraySeed \code{x} has a sparse interpretation with \code{is_sparse(x)}.
#' 
#' @seealso
#' The \linkS4class{RandomUnifArraySeed} class, which implements sampling from a uniform distribution.
#'
#' The \linkS4class{RandomPoisArraySeed} class, which implements sampling from a Poisson distribution.
#'
#' @author Aaron Lun
#' @aliases
#' RandomArraySeed
#' initialize,RandomArraySeed-method
#' chunkdim,RandomArraySeed-method
#' show,RandomArraySeed-method
#' extract_array,RandomArraySeed-method
#' is_sparse,RandomArraySeed-method
#' OLD_extract_sparse_array,RandomArraySeed-method
#' sampleDistrFun
#' sampleDistrParam
#'
#' @docType class
#' @name RandomArraySeed-class
NULL

#' @export
#' @importFrom dqrng generateSeedVectors
setMethod("initialize", "RandomArraySeed", function(.Object, dim, chunkdim, sparse=FALSE, ...) {
    dim <- as.integer(dim)

    if (is.null(chunkdim)) {
        chunkdim <- pmax(100L, as.integer(ceiling(sqrt(dim))))
        chunkdim <- pmin(chunkdim, dim)
    } else {
        chunkdim <- as.integer(chunkdim)
        msg <- .is_valid_chunkdim(dim, chunkdim)
        if (!is.null(msg)) {
            stop(msg)
        }
    }

    nchunks <- .compute_nchunks(dim, chunkdim)
    seeds <- generateSeedVectors(nchunks)

    callNextMethod(.Object, dim=dim, chunkdim=chunkdim, seeds=seeds, sparse=sparse, ...)
})

#' @export
setMethod("chunkdim", "RandomArraySeed", function(x) x@chunkdim)

setValidity2("RandomArraySeed", function(object) {
    dims <- object@dim
    if (any(dims < 0)) {
        return("'dim' must contain non-negative integers")
    }

    params <- sampleDistrParam(object)
    for (i in params) {
        msg <- .is_valid_param(dims, slot(object, i), i)
        if (!is.null(msg)) {
            return(msg)
        }
    }

    msg <- .is_valid_chunkdim(dims, object@chunkdim)
    if (!is.null(msg)) {
        return(msg)
    }

    if (!isTRUE(object@sparse) && !isFALSE(object@sparse)) {
        return("'sparse' must be either TRUE or FALSE")
    }

    TRUE
})

#' @export
setMethod("show", "RandomArraySeed", function(object) {
    cat(paste(dim(object), collapse=" x "), class(object), "object\n")
})

#' @export
setMethod("extract_array", "RandomArraySeed", function(x, index) {
    reindex <- .obtain_unique_sorted_index(index)
    arr <- sample_standard_uniform(dim(x), x@chunkdim, x@seeds, reindex$index)

    param.names <- sampleDistrParam(x)
    params <- lapply(param.names, function(i) .extract_parameter(slot(x, i), reindex$index, dim(x)))
    names(params) <- param.names

    arr <- .sample_distribution(arr, sampleDistrFun(x), params)
    .remap_to_original_index(arr, index, reindex)
})

#' @export
setMethod("is_sparse", "RandomArraySeed", function(x) x@sparse)

#' @export
setMethod("OLD_extract_sparse_array", "RandomArraySeed", function(x, index) {
    out <- extract_array(x, index)
    as(out, "SparseArraySeed")
})

.ncp_extract_array <- function(x, index) {
    reindex <- .obtain_unique_sorted_index(index)
    arr <- sample_standard_uniform(dim(x), x@chunkdim, x@seeds, reindex$index)

    param.names <- sampleDistrParam(x)
    params <- lapply(param.names, function(i) .extract_parameter(slot(x, i), reindex$index, dim(x)))
    names(params) <- param.names
    if (!is.null(x@ncp)) {
        params$ncp <- .extract_parameter(x@ncp, reindex$index, dim(x))
    }

    arr <- .sample_distribution(arr, sampleDistrFun(x), params)
    .remap_to_original_index(arr, index, reindex)
}
