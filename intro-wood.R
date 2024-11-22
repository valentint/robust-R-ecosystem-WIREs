library(conflicted) # avoid masking of functions
library(rrcov)
library(reshape)
library(ggplot2)
library(GGally)
library(viridis)        # for Colorblind-Friendly Color Maps for R
library(RColorBrewer)
library(scales)         # for show-col()

here::i_am("intro-wood.R")
library(here)

##  Figure 4: Box plots (left) and pairwise scatter plots (right) for the modified 
##      wood gravity data set. The four known outliers are marked in red in the 
##      right-hand panel.

##  Figure 5: Classical and robust correlations and tolerance ellipses (left) and 
##      classical and robust Mahalanobis distances (right) for the modified wood gravity data.

##  The data set wood from the package robustbase contains the well
##  known modified wood specific gravity data set from 
##  Rousseeuw and Leroy (1987, Table 8, page 243). The raw data are from 
##  Draper and Smith (1966, p. 227) and were used to determine the influence of 
##  anatomical factors of wood specific gravity with five explanatory variables 
##  and an intercept. A contaminated version of the data set was created by 
##  replacing a few (four) observations by outliers. We consider only the X part 
##  consisting of the explanatory variables. The four outliers do not show up in 
##  the box plots but they are clearly seen in several of the scatter plots  
##  of the pairwise scatterplot matrix, i.e. they are not univariate outliers and
##  cannot be identified by investigating any of the coordinates separately.

data(wood, package="robustbase")
x <- wood[, 1:5]
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
    ##  scale_fill_viridis(discrete=TRUE, alpha=0.6) +
    scale_fill_brewer(palette = "RdBu") +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_light() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("A boxplot with jitter") +
    xlab("")
 
p1   

##  Pairwise scatter plot matrix
iout <- rep(2, nrow(x))
iout[c(4, 6, 8, 19)] <- 1
iout <- factor(iout)
p2 <- ggpairs(x, columns = 1:5, aes(color=iout), diag=list(continuous="blankDiag"), upper=list(continuous="points")) +
        theme_light()

for(i in 1:5) {
  for(j in 1:5){
    p2[i,j] <- p2[i,j] +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
}

p2

## Plot the left- and right-hand panels as separate files
cairo_pdf(filename=here::here("Output", "intro-wood-boxplot.pdf"), width=6, height=6)
p1
dev.off()

cairo_pdf(filename=here::here("Output", "intro-wood-pairs.pdf"), width=6, height=6)
p2
dev.off()

##==============================================================================
##  Now the MCD plots
mcd <- CovMcd(x, alpha=0.75)
which(!getFlag(mcd))

##  Set the color code to the observations
library(RColorBrewer)
(outcol <- brewer.pal(3, "Set1")[1:2])
show_col(outcol)

outred <- outcol[1]         #  "#E41A1C"
outblu <- outcol[2]         #  "#377EB8"

col1 <- ifelse(iout==2, outblu, outred)


## Plot the left- and right-hand panels as separate files
cairo_pdf(filename=here::here("Output", "intro-wood-mcd-dd.pdf"), width=6, height=6)
plot(mcd, col=col1, bg=col1, pch=21, xlim=c(1.2,3.7))
abline(h=sqrt(qchisq(0.975, 5)), col="red")
abline(v=sqrt(qchisq(0.975, 5)), col="red")
dev.off()


cairo_pdf(filename=here::here("Output", "intro-wood-mcd-pairs.pdf"), width=6, height=6)
plot(mcd, col=col1, bg=col1, pch=21, which="pairs")
dev.off()
