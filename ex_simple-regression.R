library(conflicted)         # avoid masking of functions
library(robustbase)

here::i_am("ex_simple-regression.R")
library(here)

##  Figure 15: Simple linear regression: scatter plot with outliers (in red), 
##      classical least squares line (red) and robust (LTS) line (blue) in 
##      the left-hand panel; standardized robust residuals of y versus robust 
##      distances of X in the right-hand panel.


##  Create artificial data based on starsCYG and containing outliers of all types.
data(starsCYG, package="robustbase")
head(starsCYG)
dim(starsCYG)
x <- starsCYG
colnames(x) <- c("x", "y")

ibad <- c(7, 11, 20, 30, 34)
igood <- 14
ivert <- 9

# x <- x[-c(ivert, igood, ibad), ]
x[20, 2] <- -0.02        # bad outlier with negative residual
x[7, 1] <- 4.6           # regular obs.
x[11, 1] <- 4.8          # good leverage point
x[9, 2] <- 6.8           # vertical outlier
x[1, 2] <- 6.7           # vertical outlier
x[2, 2] <- -0.02         # vertical outlier with negative residual

ibad <- c(20, 30, 34)
igood <- c(11, 14)
ivert <- c(1, 2, 9)
iout <- c(ibad, igood, ivert)

out <- x[iout,]
x <- x[-iout,]
x <- rbind(out, x)
iout <- 1:nrow(out)

##  Set the color code to the observations
library(RColorBrewer)
library(scales)                     # for show_col()
(outcol <- brewer.pal(3, "Set1")[1:2])
show_col(outcol)

outred <- outcol[1]         #  "#E41A1C"
outblu <- outcol[2]         #  "#377EB8"

col1 <- rep(outblu, nrow(x))
col1[iout] <- outred
pch1 <- 21

cairo_pdf(filename=here::here("output", "simple-reg.pdf"), width=12, height=6)

opar <- par(mfrow=c(1,2))

##  Scatterplot with lines
plot(x$x, x$y, xlim=c(3.4, 4.9), pch=pch1, col=col1, bg=col1, xlab="x", ylab="y") 
text(x[iout,1], x[iout,2], iout, pos=4) 
abline(lm(y~x, data=x), col=outred, lwd=2)
abline(ltsReg(y~x, data=x), col=outblu, lwd=2)
text(3.45, 2, "LTS", col=outblu)
text(3.45, 4.3, "OLS", col=outred)

##  Diagnostic plot
plot(lts <- ltsReg(y~x, data=x), which="rdiag", pch=pch1, col=col1, bg=col1, offset=0.4) 
quant <- max(c(sqrt(qchisq(0.975, 1)), 2.5))
abline(v = quant, h = c(-2.5, 2.5), col=outred, lwd=1.5)
text(4, 5, "bad leverage points")
text(4, -5, "bad leverage points")
text(5.2, 0, "good leverage points")
text(1, 5.8, "vertical outliers")
text(1, -5.8, "vertical outliers")
text(1.2, 2, "regular observations")

par(opar)

dev.off()
