#' @export
#' @import DelayedArray
setClass("RandomArraySeed", contains="VIRTUAL", slots=c(dim="integer", chunkdim="integer", seeds="list", sparse="logical"))


#' @export
setClass("RandomUnifArraySeed", contains="RandomArraySeed", slots=c(min="ANY", max="ANY"))

#' @export
setClass("RandomUnifArray", contains="DelayedArray", representation(seed="RandomUnifArraySeed"))

#' @export
setClass("RandomUnifMatrix", contains="DelayedMatrix", representation(seed="RandomUnifArraySeed"))


#' @export
setClass("RandomNormArraySeed", contains="RandomArraySeed", slots=c(mean="ANY", sd="ANY"))

#' @export
setClass("RandomNormArray", contains="DelayedArray", representation(seed="RandomNormArraySeed"))

#' @export
setClass("RandomNormMatrix", contains="DelayedMatrix", representation(seed="RandomNormArraySeed"))


#' @export
setClass("RandomPoisArraySeed", contains="RandomArraySeed", slots=c(lambda="ANY"))

#' @export
setClass("RandomPoisArray", contains="DelayedArray", representation(seed="RandomPoisArraySeed"))

#' @export
setClass("RandomPoisMatrix", contains="DelayedMatrix", representation(seed="RandomPoisArraySeed"))


#' @export
setClass("RandomExpArraySeed", contains="RandomArraySeed", slots=c(rate="ANY"))

#' @export
setClass("RandomExpArray", contains="DelayedArray", representation(seed="RandomExpArraySeed"))

#' @export
setClass("RandomExpMatrix", contains="DelayedMatrix", representation(seed="RandomExpArraySeed"))


#' @export
setClass("RandomBetaArraySeed", contains="RandomArraySeed", slots=c(shape1="ANY", shape2="ANY", ncp="ANY"))

#' @export
setClass("RandomBetaArray", contains="DelayedArray", representation(seed="RandomBetaArraySeed"))

#' @export
setClass("RandomBetaMatrix", contains="DelayedMatrix", representation(seed="RandomBetaArraySeed"))


#' @export
setClass("RandomBinomArraySeed", contains="RandomArraySeed", slots=c(size="ANY", prob="ANY"))

#' @export
setClass("RandomBinomArray", contains="DelayedArray", representation(seed="RandomBinomArraySeed"))

#' @export
setClass("RandomBinomMatrix", contains="DelayedMatrix", representation(seed="RandomBinomArraySeed"))


#' @export
setClass("RandomCauchyArraySeed", contains="RandomArraySeed", slots=c(location="ANY", scale="ANY"))

#' @export
setClass("RandomCauchyArray", contains="DelayedArray", representation(seed="RandomCauchyArraySeed"))

#' @export
setClass("RandomCauchyMatrix", contains="DelayedMatrix", representation(seed="RandomCauchyArraySeed"))


#' @export
setClass("RandomChisqArraySeed", contains="RandomArraySeed", slots=c(df="ANY", ncp="ANY"))

#' @export
setClass("RandomChisqArray", contains="DelayedArray", representation(seed="RandomChisqArraySeed"))

#' @export
setClass("RandomChisqMatrix", contains="DelayedMatrix", representation(seed="RandomChisqArraySeed"))


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


#' @export
setClass("RandomGeomArraySeed", contains="RandomArraySeed", slots=c(prob="ANY"))

#' @export
setClass("RandomGeomArray", contains="DelayedArray", representation(seed="RandomGeomArraySeed"))

#' @export
setClass("RandomGeomMatrix", contains="DelayedMatrix", representation(seed="RandomGeomArraySeed"))


#' @export
setClass("RandomHyperArraySeed", contains="RandomArraySeed", slots=c(m="ANY", n="ANY", k="ANY"))

#' @export
setClass("RandomHyperArray", contains="DelayedArray", representation(seed="RandomHyperArraySeed"))

#' @export
setClass("RandomHyperMatrix", contains="DelayedMatrix", representation(seed="RandomHyperArraySeed"))


#' @export
setClass("RandomLnormArraySeed", contains="RandomArraySeed", slots=c(meanlog="ANY", sdlog="ANY"))

#' @export
setClass("RandomLnormArray", contains="DelayedArray", representation(seed="RandomLnormArraySeed"))

#' @export
setClass("RandomLnormMatrix", contains="DelayedMatrix", representation(seed="RandomLnormArraySeed"))


#' @export
setClass("RandomLogisArraySeed", contains="RandomArraySeed", slots=c(location="ANY", scale="ANY"))

#' @export
setClass("RandomLogisArray", contains="DelayedArray", representation(seed="RandomLogisArraySeed"))

#' @export
setClass("RandomLogisMatrix", contains="DelayedMatrix", representation(seed="RandomLogisArraySeed"))


#' @export
setClass("RandomTArraySeed", contains="RandomArraySeed", slots=c(df="ANY", ncp="ANY"))

#' @export
setClass("RandomTArray", contains="DelayedArray", representation(seed="RandomTArraySeed"))

#' @export
setClass("RandomTMatrix", contains="DelayedMatrix", representation(seed="RandomTArraySeed"))


#' @export
setClass("RandomWilcoxArraySeed", contains="RandomArraySeed", slots=c(m="ANY", n="ANY"))

#' @export
setClass("RandomWilcoxArray", contains="DelayedArray", representation(seed="RandomWilcoxArraySeed"))

#' @export
setClass("RandomWilcoxMatrix", contains="DelayedMatrix", representation(seed="RandomWilcoxArraySeed"))


#' @export
setClass("RandomWeibullArraySeed", contains="RandomArraySeed", slots=c(shape="ANY", scale="ANY"))

#' @export
setClass("RandomWeibullArray", contains="DelayedArray", representation(seed="RandomWeibullArraySeed"))

#' @export
setClass("RandomWeibullMatrix", contains="DelayedMatrix", representation(seed="RandomWeibullArraySeed"))


