#' A DelayedArray seed supplying chunked random values
#'
#' The ChunkedDelayedRandomSeed is a \linkS4class{DelayedArray} seed that performs reproducible, on-demand sampling of randomly distributed values.
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
#' The \code{\link{chunkdim}(x)} method will return the chunk dimensions of a ChunkedDelayedRandomSeed instance \code{x}.
#' This will be used by the \pkg{DelayedArray} machinery to optimize block processing by extracting whole chunks where possible.
#'
#' @section Implementing subclasses:
#' To sample from a specific distribution, we can implement a concrete subclass of the ChunkedDelayedRandomSeed.
#' This is done by implementing methods for \code{sampleDistrFun} and \code{sampleDistrParam}.
#'
#' In the code chunks below, \code{x} is an instance of a ChunkedDelayedRandomSeed subclass:
#' \itemize{
#' \item \code{sampleDistrFun(x)} returns a quantile function that accepts a vector of cumulative probabilities \code{p} and returns a numeric vector of quantiles.
#' A typical example is \code{\link{qnorm}}, though similar functions from the \pkg{stats} package can also be used.
#' The output vector should be the same length as \code{p}; any other distributional parameters should be recycled to the length of \code{p}.
#' \item \code{sampleDistrParam(x)} returns a character vector specifying the names of the distributional parameters as slots of \code{x}.
#' For example, for a subclass that samples from a normal distribution, this might be \code{"mean"} and \code{"sd"}.
#' Each distributional parameter is expected to be numeric. 
#' }
#'
#' The \code{extract_array} method for the ChunkedDelayedRandomSeed will automatically use both of the above methods to sample from the specified distribution.
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
#' @seealso
#' The \linkS4class{ChunkedUniformArraySeed} class, which implements sampling from a uniform distribution.
#'
#' @author Aaron Lun
#' @aliases
#' ChunkedRandomArraySeed
#' initialize,ChunkedRandomArraySeed-method
#' chunkdim,ChunkedRandomArraySeed-method
#' show,ChunkedRandomArraySeed-method
#' extract_array,ChunkedRandomArraySeed-method
#'
#' @docType class
#' @name ChunkedRandomArraySeed-class
NULL

#' @export
#' @importFrom dqrng generateSeedVectors
setMethod("initialize", "ChunkedRandomArraySeed", function(.Object, dim, chunkdim, ...) {
    dim <- as.integer(dim)

    if (is.null(chunkdim)) {
        chunkdim <- pmax(100L, as.integer(ceiling(sqrt(dim))))
    } 
    msg <- .is_valid_chunkdim(dim, chunkdim)
    if (!is.null(msg)) {
        stop(msg)
    }
    chunkdim <- pmin(as.integer(chunkdim), dim)

    nchunks <- .compute_nchunks(dim, chunkdim)
    seeds <- generateSeedVectors(nchunks)

    callNextMethod(.Object, dim=dim, chunkdim=chunkdim, seeds=seeds, ...)
})

#' @export
setMethod("chunkdim", "ChunkedRandomArraySeed", function(x) x@chunkdim)

setValidity2("ChunkedRandomArraySeed", function(object) {
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

    TRUE
})

#' @export
setMethod("show", "ChunkedRandomArraySeed", function(object) {
    cat(paste(dim(object), collapse=" x "), class(object), "object\n")
})

#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib DelayedRandomArray
setMethod("extract_array", "ChunkedRandomArraySeed", function(x, index) {
    reindex <- .obtain_unique_sorted_index(index)
    arr <- sample_standard_uniform(dim(x), x@chunkdim, x@seeds, reindex$index)
    params <- lapply(sampleDistrParam(x), function(i) .extract_parameter(slot(x, i), reindex$index, dim(x)))
    arr <- .sample_distribution(arr, sampleDistrFun(x), params)
    .remap_to_original_index(arr, index, reindex)
})
