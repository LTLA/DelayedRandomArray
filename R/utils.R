.is_valid_param <- function(dim, param, name) {
    if (!is.null(dim(param))) {
        if (!identical(dim, dim(param))) {
            return(sprintf("'dim' and 'dim(%s)' must be the same", name))
        }
    } else if (prod(dim) %% length(param) !=0) {
        warning(sprintf("number of entries in the array is not a multiple of 'length(%s)'", name))
    }
    NULL
}

.is_valid_chunkdim <- function(dim, chunkdim) {
    if (any(chunkdim < 0)) {
        return("'chunkdim' should contain non-negative integers")
    }
    if (length(dim)!=length(chunkdim)) {
        return("'dim' and 'chunkdim' should be of the same length")
    }
    if (any(chunkdim==0 & dim > 0)) {
        return("'chunkdim' should contain positive integers for non-zero-length 'dim'")
    }
    if (any(chunkdim > dim)) {
        return("'chunkdim' should not contain values larger than 'dim'")
    }
    NULL
}

.compute_nchunks <- function(dim, chunkdim) {
    if (any(dim <= 0)) {
        0L
    } else {
        as.integer(prod(ceiling(dim/chunkdim)))
    }
}

.obtain_unique_sorted_index <- function(index) {
    coerced <- logical(length(index))
    for (i in seq_along(index)) {
        current <- index[[i]]
        if (!is.null(current) && (is.unsorted(current) || anyDuplicated(current))) {
            index[[i]] <- unique(sort(current))
            coerced[i] <- TRUE
        }
    }
    list(index=index, coerced=coerced)
}

.remap_to_original_index <- function(arr, index, reindex) {
    if (any(reindex$coerced)) {
        mapping <- vector("list", length(index))
        for (i in seq_along(mapping)) {
            if (reindex$coerced[i]) {
                mapping[[i]] <- match(index[[i]], reindex$index[[i]])
            } else {
                mapping[[i]] <- substitute()
            }
        }
        arr <- do.call("[", c(list(arr), mapping, list(drop=FALSE)))
    }
    arr
}

.extract_parameter <- function(param, index, dims) {
    if (!is.null(dim(param))) {
        extract_array(param, index)
    } else if (length(param)==1L) {
        param
    } else {
        recycle_vector(param, dims, index)
    }
}

.sample_distribution <- function(probs, FUN, params) {
    probs[] <- do.call(FUN, c(list(p=as.numeric(probs)), lapply(params, as.numeric)))
    probs
}
