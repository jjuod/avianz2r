# Reading and processing of AviaNZ annotations in R

This package contains functions for reading AviaNZ-format annotations into R, doing some simple processing, and preparing them for inference models, in particular acoustic spatial capture-recapture.

## Installation

Install the package by:

```
install.packages("remotes")
remotes::install_github("jjuod/avianz2r", build_vignettes=T)
```

Note that the `build_vignettes` argument is needed to include the package's vignette, which provides a summary of the functions included.
If building it throws errors (or takes too long), set it to `FALSE`, and view the raw vignette [here](vignettes/avianz2r.Rmd).

## Usage

See the vignette with `vignette("avianz2r")` or browse the help files. Most likely you will need the function `readAnnots`, which imports AviaNZ annotations.

