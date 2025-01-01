# DelayedArray of random values

|Build|Status|
|-----|----|
| [Bioc-release](https://bioconductor.org/packages/release/bioc/html/DelayedRandomArray.html) | [![](https://bioconductor.org/shields/build/release/bioc/DelayedRandomArray.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/DelayedRandomArray) |
| [Bioc-devel](https://bioconductor.org/packages/devel/bioc/html/DelayedRandomArray.html) | [![](https://bioconductor.org/shields/build/devel/bioc/DelayedRandomArray.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/DelayedRandomArray) | 

This package implements a `DelayedArray` of random values where the realization of the sampled values is delayed until they are needed.
Reproducible sampling within any subarray is achieved by chunking where each chunk is initialized with a different random seed and stream.
Check out the user's guide on the [Bioconductor landing page](https://bioconductor.org/packages/devel/bioc/html/DelayedRandomArray.html) for more details.
