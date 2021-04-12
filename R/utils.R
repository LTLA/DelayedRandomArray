.is_valid_param <- function(dim, param, name) {
    if (!is.null(dim(param))) {
        if (!identical(dim, dim(param))) {
            return(sprintf("'dim' and 'dim(%s)' must be the same", name))
        }
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
