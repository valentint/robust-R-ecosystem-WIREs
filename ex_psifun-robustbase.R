##  ex-psifun-robustbase.R
##
## Plot examples of the Huber and bisquare robust loss functions. For details see
##  the vignette of the package 'robustbase': Definitions of Psi-Functions Available in Robustbase.
##

##  Figure XX: Robust ρ- ψ-functions: Huber family of functions using tuning parameter 
##      k = 1.345 in the left-hand panel and Bisquare family functions using tuning 
##      parameter k = 4.685 in the right-hand panel. This figure was created using 
##      the code provided in the package robustbase.

library(conflicted) # avoid masking of functions

here::i_am("ex_psifun-robustbase.R")
library(here)

source(system.file("xtraR/plot-psiFun.R", package = "robustbase", mustWork=TRUE))
x. <- seq(-5, 10, length=1501)

library(RColorBrewer)
library(scales)
(mycol <- brewer.pal(8, "Set1"))
mycol <- mycol[-c(3,6)]
show_col(mycol)

cairo_pdf(filename=here::here("Output", "psifun-robustbase.pdf"), width=15, height=7.5)

opar <- par(mfrow=c(1,2))
plot(huberPsi, x., ylim=c(-1.4, 5), leg.loc="topright", main="", col=mycol, lwd=1.5)
p.psiFun(x., "biweight", par = 4.685, main="", col=mycol, lwd=1.5)
par(opar)

dev.off()

