library(conflicted) # avoid masking of functions
library(rospca)
library(robustbase)
library(rrcov)

here::i_am("ex_glass-2-MRCD.R")
library(here)

data(Glass)
dim(Glass)

##  Figure 10: 
##  Glass data: Distance-distance plot obtained with the MRCD estimator in the 
##      left-hand panel and distance-distance plot obtained with robust PCA (ROBPCA) 
##      in the right-hand panel. The regular observations are shown in green,
##      the 38 outliers 143-179 in red, the outliers 57-63 and 74-76 - in orange 
##      and the border cases 22, 23, 30 and 180 - in blue.
##

##  Now we turn again to the complete data set (first 500 frequences).
##  Let's remove the variables with 0 MAD (there 13 such variables)
##  And then compute the regularized MCD (MRCD) estimator

X <- Glass[,1:500]
X <- X[,-which(apply(X, 2, mad) <= 1e-5)]
mrcd <- CovMrcd(X)
rpc <- PcaHubert(X, k=3, scale=FALSE, alpha=0.7)

##  This could crash R !!!
## ogk <- CovOgk(X)

##  Set the color code to the observations
library(RColorBrewer)
(outcol <- brewer.pal(3, "Set1")[1:2])
show_col(outcol)

outred <- outcol[1]         #  "#E41A1C"
outblu <- outcol[2]         #  "#377EB8"

col1 <- rep(outblu, 180)        # regular
col1[143:179] <- outred         # The window of the detector system was cleaned 
                                # before the last 38 spectra were measured and 
                                # as a result less radiation was absorbed, hence 
                                # more was detected: 143-179.
col1[57:63] <- outred           # 57-63: are samples with a large concentration of calcium
col1[74:76] <- outred           # 74:76: "---"   
col1[c(22, 23, 30)] <- outred   # 22, 23 and 30: border cases
col1[180] <- outred             # 180: border case

pch1 <- rep(19, 180)
pch1[143:179] <- 19
pch1[c(57:63, 74:76)] <- 15
pch1[c(22, 23, 30, 180)] <- 17

cairo_pdf(filename=here::here("Output", "glass-dd.pdf"), width=15, height=7.5)
opar <- par(mfrow=c(1,2))
plot(mrcd, id.n=0, col=col1, pch=pch1, main="MRCD: Distance-distance plot", cex=1.8)
plot(rpc, id.n.od=0, id.n.sd=0, col=col1, pch=pch1, main="ROBPCA: Robust PCA", cex=1.8)
par(opar)
dev.off()

if(FALSE) {
    ##  Create other plots (not included in the paper)
    plot(mrcd, which="qqchi2", id.n=0)
    savePlot(type="pdf", file=here::here("Output", "glass-mrcd-QQ.pdf"))
    
    plot(mrcd, which="xyqqchi2", id.n=0)
    savePlot(type="pdf", file=here::here("Output", "glass-mrcd-xyQQ.pdf"))
    
    plot(mrcd, which="xydistance", id.n=0)
    savePlot(type="pdf", file=here::here("Output", "ws2-glass-III-mrcd-xydist.pdf"))

}
