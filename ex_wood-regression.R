##
##  Regression on the Modified wood gravity data
##

library(conflicted) # avoid masking of functions
library(robustbase)

here::i_am("ex_wood-regression.R")
library(here)

##  Figure 16: Modified data on wood specific gravity: OLS regression diagnostic 
##      plot in the left-hand panel and the robust version obtained with the 
##      LTS in the right-hand panel. 

##  Table 5: Modified data on wood specific gravity: Standard regression output 
##      of LTS estimation.


##  The data set wood from the package robustbase contains the well
##  known modified wood specific gravity data set from 
##  Rousseeuw and Leroy (1987, Table 8, page 243). The raw data are from 
##  Draper and Smith (1966, p. 227) and were used to determine the influence of 
##  anatomical factors of wood specific gravity with five explanatory variables 
##  and an intercept. A contaminated version of the data set was created by 
##  replacing a few (four) observations by outliers: 4, 6, 8 and 19

data(wood, package="robustbase")

## OLS regression and the OLS regression diagnostic plots -
##  no indication of the outliers 4, 6, 8 and 19
lm <- lm(y~., data=wood)

usr <- par(mfrow=c(2,2))
plot(lm, which=1)
plot(lm, which=2)
plot(lm, which=3)
plot(lm, which=5)
par(usr)

## OLS regression, computed through the ltsReg() function - to be used for the 
##  diagnostic plot
ltslm <- ltsReg(y~., data=wood, alpha=1)
plot(ltslm, which="rdiag", xlim=c(1.2, 3.7), main="LS regression diagnostic plot")

lts <- ltsReg(y~., data=wood)

##  Table 5: ...
summary(lts)

iout <- c(4, 6, 8, 19)

##  Set the color code to the observations
library(RColorBrewer)
(outcol <- brewer.pal(3, "Set1")[1:2])
show_col(outcol)

outred <- outcol[1]         #  "#E41A1C"
outblu <- outcol[2]         #  "#377EB8"

col1 <- rep(outblu, nrow(wood))
col1[iout] <- outred
pch1 <- 21

##==============================================================================
##  Figure 16: 
##  OLS and LTS diagnostic plots: outlier map

cairo_pdf(filename=here::here("Output", "wood-rdiag.pdf"), width=12, height=6)
opar <- par(mfrow=c(1,2))
##  OLS
plot(ltslm, which="rdiag", col=col1, bg=col1, pch=pch1, offset=0.4, xlim=c(1.1, 3.7), main="LS regression diagnostic plot", id.n=0)
quant <- max(c(sqrt(qchisq(0.975, 5)), 2.5))
abline(v = quant, h = c(-2.5, 2.5), col=outred, lwd=1.5)

##  LTS
plot(lts, which="rdiag", col=col1, bg=col1, pch=pch1, offset=0.4, xlim=c(1,16), , main="LTS regression diagnostic plot")
quant <- max(c(sqrt(qchisq(0.975, 5)), 2.5))
abline(v = quant, h = c(-2.5, 2.5), col=outred, lwd=1.5)

dev.off()

