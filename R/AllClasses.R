#' @export
#' @import DelayedArray
setClass("RandomArraySeed", contains="VIRTUAL", slots=c(dim="integer", chunkdim="integer", seeds="list", sparse="logical"))


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
setClass("RandomPoissonArraySeed", contains="RandomArraySeed", slots=c(lambda="ANY"))

#' @export
setClass("RandomPoissonArray", contains="DelayedArray", representation(seed="RandomPoissonArraySeed"))

#' @export
setClass("RandomPoissonMatrix", contains="DelayedMatrix", representation(seed="RandomPoissonArraySeed"))


#' @export
setClass("RandomExponentialArraySeed", contains="RandomArraySeed", slots=c(rate="ANY"))

#' @export
setClass("RandomExponentialArray", contains="DelayedArray", representation(seed="RandomExponentialArraySeed"))

#' @export
setClass("RandomExponentialMatrix", contains="DelayedMatrix", representation(seed="RandomExponentialArraySeed"))


#' @export
setClass("RandomBetaArraySeed", contains="RandomArraySeed", slots=c(shape1="ANY", shape2="ANY", ncp="ANY"))

#' @export
setClass("RandomBetaArray", contains="DelayedArray", representation(seed="RandomBetaArraySeed"))

#' @export
setClass("RandomBetaMatrix", contains="DelayedMatrix", representation(seed="RandomBetaArraySeed"))


#' @export
setClass("RandomBinomialArraySeed", contains="RandomArraySeed", slots=c(size="ANY", prob="ANY"))

#' @export
setClass("RandomBinomialArray", contains="DelayedArray", representation(seed="RandomBinomialArraySeed"))

#' @export
setClass("RandomBinomialMatrix", contains="DelayedMatrix", representation(seed="RandomBinomialArraySeed"))


#' @export
setClass("RandomCauchyArraySeed", contains="RandomArraySeed", slots=c(location="ANY", scale="ANY"))

#' @export
setClass("RandomCauchyArray", contains="DelayedArray", representation(seed="RandomCauchyArraySeed"))

#' @export
setClass("RandomCauchyMatrix", contains="DelayedMatrix", representation(seed="RandomCauchyArraySeed"))


#' @export
setClass("RandomChisquaredArraySeed", contains="RandomArraySeed", slots=c(df="ANY", ncp="ANY"))

#' @export
setClass("RandomChisquaredArray", contains="DelayedArray", representation(seed="RandomChisquaredArraySeed"))

#' @export
setClass("RandomChisquaredMatrix", contains="DelayedMatrix", representation(seed="RandomChisquaredArraySeed"))


#' @export
setClass("RandomFArraySeed", contains="RandomArraySeed", slots=c(df1="ANY", df2="ANY", ncp="ANY"))

#' @export
setClass("RandomFArray", contains="DelayedArray", representation(seed="RandomFArraySeed"))

#' @export
setClass("RandomFMatrix", contains="DelayedMatrix", representation(seed="RandomFArraySeed"))


#' @export
setClass("RandomGammaArraySeed", contains="RandomArraySeed", slots=c(shape="ANY", scale="ANY"))

#' @export
setClass("RandomGammaArray", contains="DelayedArray", representation(seed="RandomGammaArraySeed"))

#' @export
setClass("RandomGammaMatrix", contains="DelayedMatrix", representation(seed="RandomGammaArraySeed"))


#' @export
setClass("RandomNbinomArraySeed", contains="RandomArraySeed", slots=c(prob="ANY", size="ANY", mu="ANY"))

#' @export
setClass("RandomNbinomArray", contains="DelayedArray", representation(seed="RandomNbinomArraySeed"))

#' @export
setClass("RandomNbinomMatrix", contains="DelayedMatrix", representation(seed="RandomNbinomArraySeed"))

