#' @export
#' @import DelayedArray
setClass("ChunkedUniformArraySeed", slots=c(dim="integer", min="ANY", max="ANY", chunkdim="integer", seeds="list"))

#' @export
#' @importFrom dqrng generateSeedVectors
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
setMethod("DelayedArray", "ChunkedUniformArraySeed", function(seed) new_DelayedArray(seed, Class="ChunkedUniformArray"))

#' @export
ChunkedUniformArray <- function(dim, min=0, max=1) {
    DelayedArray(ChunkedUniformArraySeed(dim, min, max))
}
