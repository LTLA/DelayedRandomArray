#' @export
#' @import DelayedArray
setClass("ChunkedRandomArraySeed", contains="VIRTUAL", slots=c(dim="integer", chunkdim="integer", seeds="list"))

#' @export
setClass("ChunkedUniformArraySeed", contains="ChunkedRandomArraySeed", slots=c(min="ANY", max="ANY"))

#' @export
setClass("ChunkedUniformArray", contains="DelayedArray", representation(seed="ChunkedUniformArraySeed"))

#' @export
setClass("ChunkedUniformMatrix", contains="DelayedMatrix", representation(seed="ChunkedUniformArraySeed"))


