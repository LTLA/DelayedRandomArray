#' @export
#' @import DelayedArray
setClass("RandomArraySeed", contains="VIRTUAL", slots=c(dim="integer", chunkdim="integer", seeds="list"))

#' @export
setClass("SparseRandomArraySeed", contains=c("VIRTUAL", "RandomArraySeed"), slots=c(sparse="logical"))


#' @export
setClass("RandomUniformArraySeed", contains="RandomArraySeed", slots=c(min="ANY", max="ANY"))

#' @export
setClass("RandomUniformArray", contains="DelayedArray", representation(seed="RandomUniformArraySeed"))

#' @export
setClass("RandomUniformMatrix", contains="DelayedMatrix", representation(seed="RandomUniformArraySeed"))


#' @export
setClass("RandomNormalArraySeed", contains="RandomArraySeed", slots=c(mean="ANY", sd="ANY"))

#' @export
setClass("RandomNormalArray", contains="DelayedArray", representation(seed="RandomNormalArraySeed"))

#' @export
setClass("RandomNormalMatrix", contains="DelayedMatrix", representation(seed="RandomNormalArraySeed"))


#' @export
setClass("RandomPoissonArraySeed", contains="SparseRandomArraySeed", slots=c(lambda="ANY"))

#' @export
setClass("RandomPoissonArray", contains="DelayedArray", representation(seed="RandomPoissonArraySeed"))

#' @export
setClass("RandomPoissonMatrix", contains="DelayedMatrix", representation(seed="RandomPoissonArraySeed"))


