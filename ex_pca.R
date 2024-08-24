library(conflicted) # avoid masking of functions
library(rrcov)
library(MASS)

here::i_am("ex_pca.R")
library(here)

##  Figure 11: Plot of the principal components of the generated data: the upper 
##  two panels show scatter plots of the clean (left-hand panel) and the altered 
##  (right-hand panel) data with the first principal component. The lower two 
##  panels show plots of the scores obtained by classical (left-hand panel) and 
##  robust (right-hand panel) PCA together with the corresponding 97.5% 
##  tolerance ellipses.

##myrng(8954)
##myrng(6452)

##  Generate data
set.seed(6452)
n <- 60     # number of observations
k <- 4      # number of outliers
rho <- 0.8  # correlation between x and y
x <- mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1, rho, rho, 1), ncol=2))
x <-x[order(x[,1]),] 

## Correlation of the clean data
cor(x)

##            [,1]      [,2]
##  [1,] 1.0000000 0.7546488
##  [2,] 0.7546488 1.0000000

## Add contamination
xcont <- x
tmp <- xcont[1:k,1]; 
xcont[1:k, 1] <- xcont[(n-k+1):n, 1]
xcont[(n-k+1):n, 1] <- tmp 
iout <- c(1:k, (n-k+1):n)

## Correlation of the contaminated data
cor(xcont)

##               [,1]         [,2]
##  [1,]  1.000000000 -0.002258378
##  [2,] -0.002258378  1.000000000

##  Set the colorscheme
col0 <- col1 <- rep("#377eb8", nrow(x))
col1[iout] <- "#e41a1c"

pch1 <- rep(1, nrow(x))
pch1[iout] <- 16

## PCA on the clea data
pc1 <- PcaClassic(x)
pc2 <- PcaCov(x)

## PCA on the contaminated data
pc1c <- PcaClassic(xcont)
pc2c <- PcaCov(xcont, k=2)

##  Do the 4 plots ............................................................
cairo_pdf(filename=here::here("Output", "ex-pca-1.pdf"), width=7.5, height=7.5)

opar <- par(mfrow=c(2,2))

## Clean data
plot(x, xlab="x", ylab="y", col=col0, main="Clean data")
abline(a=0, b=pc1$loadings[1,1]/pc1$loadings[2,1], col="red")
#abline(a=0, b=pc1$loadings[1,2]/pc1$loadings[2,2])
text(1.6, 1.1, "PC1", col="red")

## Contaminated data
plot(xcont, xlab="x", ylab="y", col=col1, pch=pch1, main="Data with 15% outliers")
abline(a=0, b=pc1c$loadings[1,1]/pc1c$loadings[2,1], col="red")
#abline(a=0, b=pc1c$loadings[1,2]/pc1c$loadings[2,2])
text(0.4, -1.8, "PC1", col="red")
xrange <- par("usr")
off.x <- xrange[2] - xrange[1]
off.x <- off.x/20               # 0.2227334
off.y <- xrange[4] - xrange[3]
off.y <- off.y/30               #  0.1383388
xx <- xcont[iout,]
xx[,1] <- xx[,1] + off.x
xx[6,2] <- xx[6,2] - off.y
text(xx[,1], xx[,2], iout, cex=0.8)

scorePlot(pc1c, col=col1, pch=pch1, id.n=0)
xx <- pc1c$scores[iout,]
xx[,1] <- xx[,1] + off.x
xx[6,2] <- xx[6,2] - off.y
text(xx[,1], xx[,2], iout, cex=0.8)

scorePlot(pc2c, col=col1, pch=pch1, id.n=0)
xx <- pc2c$scores[iout,]
xx[,1] <- xx[,1] + off.x
xx[6,2] <- xx[6,2] - off.y
xx[7,2] <- xx[7,2] + off.y
text(xx[,1], xx[,2], iout, cex=0.8)

par(opar)

dev.off()

