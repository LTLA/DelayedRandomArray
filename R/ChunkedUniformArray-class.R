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
    index2 <- reindex$index

    arr <- sample_standard_uniform(x@dim, x@chunkdim, x@seeds, index)
    
    if (any(reindex$coerced)) {
        mapping <- vector("list", length(index))
        for (i in seq_along(mapping)) {
            if (reindex$coerced[i]) {
                mapping[[i]] <- match(index[[i]], index2[[i]])
            } else {
                mapping[[i]] <- substitute()
            }
        }
        arr <- do.call("[", c(list(arr), mapping, list(drop=FALSE)))
    }

    arr
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
