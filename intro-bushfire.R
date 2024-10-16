library(conflicted) # avoid masking of functions
library(rrcov)
library(reshape)
library(ggplot2)
library(GGally)
library(viridis)        # for Colorblind-Friendly Color Maps for R
library(RColorBrewer)

here::i_am("intro-bushfire.R")
library(here)

##  Figure 4: Box plots (left) and pairwise scatter plots (right) for the 
##      bushfire data set. The four known outliers are marked in red in the 
##      right-hand panel.

##  Figure 5: Classical and robust correlations and tolerance ellipses (left) and 
##      classical and robust Mahalanobis distances (right) for the modified wood gravity data.

##  We illustrate the effect of outliers on classical location and covariance estimates 
##  and the performance of the corresponding robust estimates with a small multivariate 
##  example with 38 observations in 5 variables. The data set bushfire from the package 
##  'robustbase' contains the well known data set used by Campbell (1984) to locate bushfire 
##  scars. The data set contains satellite measurements on five frequency bands, 
##  corresponding to each of 38 pixels. It was introduced in the robustness 
##  literature by Maronna and Yohai (1995) while studying the properties of the
##  Stahel-Donoho robust estimator. They identified 14 outliers: cases 8 and 9 
##  are outstanding and are followed by a cluster of outliers 32-38. Next come 
##  7, 10 and 11 and finally 31 and 12. The 14 outliers do not show up in
##  the box plots in the left-hand panel of Figure 4 but they are clearly seen in 
##  several of the scatter plots shown in the right-hand panel which shows that 
##  they are not univariate outliers and cannot be identified by investigating
##  any of the coordinates separately.

data(bushfire, package="robustbase")
x <- bushfire
colnames(x) <- paste0("X", 1:5)

##  Boxplot and pairwise scatterplot matrix
boxplot(x)
pairs(x)

##  Now the same with ggplot
library(viridis)

##  Boxplots
data <- reshape::melt(x)
p1 <- ggplot(data, aes(x=variable, y=value, fill=variable)) +
    stat_boxplot(geom="errorbar", width=0.25) +     
    geom_boxplot(outlier.shape = NA) +
##    scale_fill_viridis(discrete=TRUE, alpha=0.6) +
    scale_fill_brewer(palette = "RdBu") +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_light() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12, face="bold"),
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold")
    ) +
    ggtitle("A boxplot with jitter") +
    xlab("")
 
p1   

##  Pairwise scatter plot matrix
iout <- rep(2, nrow(x))
iout[c(7:12, 31:38)] <- 1
iout <- factor(iout)

p2 <- ggpairs(x, columns = 1:5, aes(color=iout, alpha = 0.5), 
        diag=list(continuous="blankDiag"), 
        upper=list(continuous="points")) +
        theme_light() +
    theme(
      axis.text.y=element_text(size=10),
      axis.text.x=element_text(size=10, angle=90, vjust = 0.5, hjust=1),
      strip.text=element_text(color="black", face="bold", size=11)
    ) 

for(i in 1:5) {
  for(j in 1:5){
    p2[i,j] <- p2[i,j] +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
}

p2

## Plot the left- and right-hand panels as separate files
cairo_pdf(filename=here::here("Output", "intro-bushfire-boxplot.pdf"), width=6, height=6)
p1
dev.off()

cairo_pdf(filename=here::here("Output", "intro-bushfire-pairs.pdf"), width=6, height=6)
p2
dev.off()

##==============================================================================
##  Now the MCD plots
set.seed(12345)
mcd <- CovMcd(x, alpha=0.57)
which(!getFlag(mcd))
plot(mcd, pch=21, xlim=c(1.2,3.8))

##  Set the color code to the observations
library(RColorBrewer)
(outcol <- brewer.pal(3, "Set1")[1:2])
show_col(outcol)

outred <- outcol[1]         #  "#E41A1C"
outblu <- outcol[2]         #  "#377EB8"

col1 <- ifelse(iout==2, outblu, outred)

plot(mcd, pch=21, xlim=c(1.2,3.8), id.n=0)
oo <- which(!getFlag(mcd))
cooo <- ooo <- oo[1:9]
rd <- sqrt(mcd$mah[ooo])
md <- sqrt(getDistance(CovClassic(x))[ooo])

cooo[9] <- "33-38"
rd[9] <- rd[9] - 1
xrange <- par("usr")
xrange <- xrange[2] - xrange[1]
text(md+xrange/40, rd, labels=cooo)

## Plot the left- and right-hand panels as separate files
cairo_pdf(filename=here::here("Output", "intro-bushfire-mcd-dd.pdf"), width=6, height=6)
plot(mcd, col=col1, bg=col1, pch=21, xlim=c(1.2,3.75), id.n=0)
text(md+xrange/40, rd, labels=cooo, cex=0.8)
abline(h=sqrt(qchisq(0.975, 5)), col="red")
abline(v=sqrt(qchisq(0.975, 5)), col="red")
dev.off()


cairo_pdf(filename=here::here("Output", "intro-bushfire-mcd-pairs.pdf"), width=6, height=6)
plot(mcd, col=col1, bg=col1, pch=21, which="pairs")
dev.off()
