library(conflicted) # avoid masking of functions
library(rospca)
library(robustbase)
library(rrcov)

here::i_am("ex_glass-1.R")
library(here)

##  The data are available in the package 'rospca' as the data set 'Glass'.
##  It is a data frame with 180 observations and 750 variables. For this
##  example we will use only the first 500 wavelengths (variables).
    
data(Glass)
dim(Glass)

##  Instead of working with the raw data, we rst robustly center the
##  spectra by subtracting the univariate MCD location estimator
##  from each wavelength. Doing so allow us to observe more of the
##  variability that is present in the data

##  We define a function to calculate the univariate MCD using the
##  internal rrcov function unimcd() and apply it to the data set
##  using the robustbase (undocumented) function doScale(). We set
##  the scale parameter to NULL - in order to do no scaling note
##  that in base::scale() we have scale=FALSE for this purpose.

umcd_center <- function(y)
    rrcov:::unimcd(y, quan=h.alpha.n(0.5, length(y), 1))$tmcd

X <- doScale(Glass, center=umcd_center, scale=NULL)$x
X <- X[, 1:500]
(n <- nrow(X))
(p <- ncol(X))


##  All observations, robustly centered
matplot(t(X), type="l", lty=1)

##  Classical PCA. Three components yield 99% explained variance.
##  The outlier map shows only mild orthogonal outliers and good leverage points.

pc <- PcaClassic(X, k=3, scale=FALSE)
plot(pc, id.n.sd=0, id.n.od=0)

##  The score plot shows the observations falling into several groups, but almost all are
##  encompassed by the 0.975 tolerance ellipse.
scorePlot(pc, id.n=0)

##  The first three loadings vectors of classical PCA. 
##  The second and third peaks are mixed up.
matplot(pc$loadings, type="l", lty=1, lwd=2, ylab="",
    col=c(2,3,4), main="Classical PCA")
text(x=c(140, 300, 380), y=c(0.25, 0.2, 0.25), label=c("1", "2", "3"))

##  The first three loadings vectors of the robust PCA. 
##  ROBPCA keeps the peaks more separate.
rpc <- PcaHubert(X, k=3, scale=FALSE, alpha=0.7)
matplot(rpc$loadings, type="l", lty=1, lwd=2, ylab="", col=c(2,3,4), main="ROBPCA")
text(x=c(140, 300, 380), y=c(0.25, 0.2, 0.25), label=c(3,1,2))

##  What has caused the outliers in the glass data?
##  - The window of the detector system was cleaned before the last __38__
##      spectra were measured and as a result less radiation was absorbed, 
##      hence more was detected: 143--179.
##  - Observations 57--63 and 74--76 are samples with a large concentration of calcium.
##  - Observations 22, 23 and 30 are borderline cases (with a larger concentration of phosphor).
##  - Observation 180 is also a borderline case.

##  Outlier map from ROBPCA. The bad leverage points are easily seen.
plot(rpc, id.n.sd=0, id.n.od=0)
text(x=7.5, y=1100, label="143-179")
text(x=5.1, y=1350, label="57-63,")
text(x=5.1, y=1300, label="74-76")
text(x=1.8, y=1350, label="30")
text(x=1, y=1150, label="22,23")
text(x=5, y=1050, label="180")

##  Regular observations.
##  These clearly have lower measurements at channels 160-175 than the samples 143-179.
xa <- c(57:63, 74:76)               # bad leverage points 57-63 and 74-76; grp==4
xb <- 143:179                       # bad leverage points
xc <- c(22, 23, 30)                 # orthogonal outliers 22, 23, 30
xd <- 180
xgood <- 1:nrow(X)
xgood <- xgood[which(!(xgood %in% xa))]
xgood <- xgood[which(!(xgood %in% xb))]
xgood <- xgood[which(!(xgood %in% xc))]
xgood <- xgood[which(!(xgood %in% xd))]
matplot(t(X[xgood,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="")

##  Bad leverage points 143--179.
matplot(t(X[xb,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="")

##  Bad leverage points 57--63 and 74--76.
matplot(t(X[xa,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="")

##  Orthogonal outliers.
matplot(t(X[xc,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="")

##  All plots
opar <- par(mfrow=c(2,2))
matplot(t(X[xgood,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="", main="Regular observations")
matplot(t(X[xb,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="", main="Bad leverage points 143--179")
matplot(t(X[xa,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="", main="Bad leverage points 57--63 and 74--76")
matplot(t(X[xc,]), type="l", lty=1, ylim=c(min(X), max(X)), ylab="", main="Orthogonal outliers")
par(opar)
