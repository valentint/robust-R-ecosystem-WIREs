library(conflicted) # avoid masking of functions
library(rospca)
library(robustbase)
library(rrcov)

here::i_am("ex_glass-1.R")
library(here)


##  Figure 6: Glass data set: Robust principal component analysis (PCA) of a subset 
##      of the data. Two groups of outliers are identified 

##  Figure 7:  Glass data: chi-square QQ-plots of squared Mahalanobis distances of estimates.

##  In this example we consider a data set of EPXMA spectra over p = 750 wavelengths collected on 180 different
##  archeological glass vessels (Hubert et al., 2005; Maronna and Yohai, 2017). The data are available in the package
##  rospca as the data set Glass.


data(Glass)
dim(Glass)

##  Let us now consider only the frequencies 350 to 385 of this data set, which yield p = 36
##  The reason for choosing this interval is that this is a region of the spectra where 
##  the variability is highest maronna:2017

x <- Glass[,350:385]
dim(x)
a <- apply(x, 2, mad)
max(a)/min(a)
## [1] 38.92019

col2 <- col1 <- rep("#377eb8", nrow(x))
pch1 <- rep(21, nrow(x))
##  In order to gain some insight on the data structure, again a principal 
##  components analysis (PCA) was performed. Since we already expect 
##  the presence of outliers, robust PCA is used (ROBPCA)
pc <- PcaHubert(x, k=10, scale=mad)
scorePlot(pc, id.n=0, col=col1, pch=pch1)

##  The bulk of the data lies in the zone with z1 <= 10. 
##  There is a group of 10 clear outliers with z1 >= 20.
iout1 <- which(pc$scores[,1] >= 20)
col1[iout1] <- "#e41a1c"
pch1[iout1] <- 19
scorePlot(pc, id.n=0, col=col1, pch=pch1)

##  Another intermediate group of 12 outliers lies between 10 and 20, i.e.  10 >= z1 < 20.
iout2 <- which(pc$scores[,1] >= 10 & pc$scores[,1] < 20)
col1[iout2] <- "#e41a1c"
pch1[iout2] <- 21
scorePlot(pc, id.n=0, col=col1, pch=pch1)

savePlot(file=here::here("Output", "glass-ex1-scoreplot.pdf"), type="pdf")

##  For each of several estimators we will show the QQ-plots that
##  compare the ordered squared Mahalanobis distances of the estimators
##  to the quantiles of the chi2 distribution with p degrees of freedom.

##  The plots corresponding to MCD, MM, and Rocke  show a sharp turn 
##  that indicates the presence of outliers.
##  The turn of SD is less pronounced; and S-bw and Cov are much less drastic.

sortColor <- function(obj, col) {
    rd <- getDistance(obj)
    x <- sort(rd, index.return=TRUE)
    ird <- x$ix
    col[ird]
}

set.seed(33334)
mcd <- CovMcd(x)
mm <- CovMMest(x)
rocke <- CovSest(x, method="rocke")
se <- CovSest(x)
sde <- CovSde(x, nsamp=5000)
cov <- CovClassic(x)

opar <- par(mfrow=c(2,3))
plot(mcd, which="qqchi2", main="MCD", id.n=0, col=sortColor(mcd, col1), pch=sortColor(mcd, pch1))
plot(mm, which="qqchi2", main="MM", xlab="", id.n=0, col=sortColor(mm, col1), pch=sortColor(mm, pch1))
plot(rocke, which="qqchi2", main="S-rocke", xlab="", id.n=0, col=sortColor(rocke, col1), pch=sortColor(rocke, pch1))
plot(se, which="qqchi2", main="S-bw", xlab="", id.n=0, col=sortColor(se, col1), pch=sortColor(se, pch1))
plot(sde, which="qqchi2", main="SD", xlab="", id.n=0, col=sortColor(sde, col1), pch=sortColor(sde, pch1))
plot(cov, which="qqchi2", main="Cov", xlab="", id.n=0, col=sortColor(cov, col1), pch=sortColor(cov, pch1))
par(opar)

savePlot(file=here::here("Output", "glass-ex1-QQ.pdf"), type="pdf")

##  For each estimator we compute the number of observations corresponding to
##  the largest 22 distances such that PC1 >= 20 and PC1 >= 10.
##  It is seen that MCD, MM, and S-Rocke correctly include the largest 10 outliers,
##  and 22, 16 or 18 of the total 22 outliers while the other estimators
##  show less satisfying performance

library(xtable)
iout1 <- which(pc$scores[,1] >= 20)
iout2 <- which(pc$scores[,1] >= 10 & pc$scores[,1] < 20)
iout3 <- c(iout1, iout2)

aout <- matrix(0, nrow=2, ncol=7)
rownames(aout) <- c("PC1 > 20", "PC1 > 10")
colnames(aout) <- c("Number", "MCD", "MM", "S-rocke", "S-bw", "SDE", "COV")
aout[1, 1] <- 10
aout[2, 1] <- 22
aout[1, 2] <- length(which(iout1 %in% which(!getFlag(mcd))))     # 10
aout[2, 2] <- length(which(iout3 %in% which(!getFlag(mcd))))     # 20
aout[1, 3] <- length(which(iout1 %in% which(!getFlag(mm))))      # 10
aout[2, 3] <- length(which(iout3 %in% which(!getFlag(mm))))      # 16
aout[1, 4] <- length(which(iout1 %in% which(!getFlag(rocke))))   # 10
aout[2, 4] <- length(which(iout3 %in% which(!getFlag(rocke))))   # 19
aout[1, 5] <- length(which(iout1 %in% which(!getFlag(se))))      #  8
aout[2, 5] <- length(which(iout3 %in% which(!getFlag(se))))      # 11
aout[1, 6] <- length(which(iout1 %in% which(!getFlag(sde))))     #  7
aout[2, 6] <- length(which(iout3 %in% which(!getFlag(sde))))     # 10
aout[1, 7] <- length(which(iout1 %in% which(!getFlag(cov))))     #  6
aout[2, 7] <- length(which(iout3 %in% which(!getFlag(cov))))     #  9

aout

#         MCD MM S-rocke S-bw SDE COV
#PC1 > 20  10 10      10    8   8   6
#PC1 > 10  22 16      18   11  11   9

xtable(aout, digits=0)

