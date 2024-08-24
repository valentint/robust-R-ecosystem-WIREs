library(conflicted) # avoid masking of functions
library(pracma)
library(rrcov)
library(robustX)
library(RobStatTM)
library(xtable)

library(reshape)
library(ggplot2)
library(GGally)     # for ggpairs()

here::i_am("ex_bwheel.R")
library(here)

##  Figure 8: Scatter plot matrices of a sample from the barrow wheel distribution, p = 4

##  Figure 9: Simulation results for 100 repetitions with n = 25, p = 5, ε = 0.2 of 
##      the “barrow wheel” distribution. The oracle function is the sample covariance 
##      for the clean data.

##  Table 3: Robust estimators available in R which are compared in the simulation

##  Figure 8: ...
n <- 500; p <- 4
r <- rbwheel(n, p)
n1 <- attr(r, "n1")
out <- 1+((1:n) > n1)
bg1 <- hcl.colors(2, "Temps")[out]
col1 <- hcl.colors(2, "Temps")[out]
pairs(r, bg=bg1, col=col1, pch=22)

##  cairo_pdf(filename="bench_bwheel_example-1.pdf", width=6, height=6)
##  pairs(r, col=1+((1:n) > n1))
##  dev.off()

## Now the same with ggpairs() ...
library(GGally)

n <- 500; p <- 4
r <- rbwheel(n, p)
n1 <- attr(r, "n1")
out <- 2-((1:n) > n1)
colnames(r) <- paste0("V", 1:p)
r <- cbind.data.frame(r, out=factor(out))
gg <- ggpairs(r, columns = 1:p, aes(color = out), upper=list(continuous="points")) + theme_light()

for(i in 1:4) {
  for(j in 1:4){
    gg[i,j] <- gg[i,j] +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
}

gg

cairo_pdf(filename=here::here("Output", "bench_bwheel_example-2.pdf"), width=6, height=6)
gg
dev.off()

##  Table 3: ...
meth <- c("CovOracle", "CovClassic", "CovMcd", "CovMrcd", "CovSde", "CovMve",
          "CovSest_fast", "CovSest_rocke", "CovSest_bs", "CovMMest",
          "CovOgk", "covRob_S", "covRob_MM", "covRob_Kurt",
          "covBACON", "covNNC")

meth_desc <- c("",
"Classic (CovClassic in rrcov)",
"Fast MCD (Minimum Covariance Determinant)",
"Minimum Regularized Covariance Determinant",
"Stahel-Donoho",
"Minimum Volume estimator",
"Fast S-est. Salibian-Barrera and Yohai(2006)",
"S-est with Rocke rho function",
"S-est. with bisquare rho function and HBDP start (MVE)",
"MM estimate",
"Orthogonalized Gnanadesikan-Kettenring (OGK)",
"S-est. with Rocke function; Maronna et al (2019)",
"MM estimate with SFR rho function; Maronna et al (2019)",
"Kurtosis plus specific directions (Pena and Prieto, 2007)",
"Blocked Adaptive Computationally-Efficient Outlier Nominator",
"Nearest-Neighbor Cleaning; Wang and Raftery (2002)")

meth_R <- c(" ", "rrcov", "rrcov", "rrcov", "rrcov", "rrcov", "rrcov", "rrcov",
        "rrcov", "rrcov", "rrcov", "RobStatTM", "RobStatTM", "RobStatTM", "robustX", "robustX")

dfmeth <- data.frame(`R package`=meth_R, Function=meth, Description=meth_desc)

print(xtable(dfmeth), include.rownames=FALSE)

##  Perform the simulations ...
do_bwheel_simulation <- function(nsim=100, meth) {
    nmeth <- length(meth)
    nsim <- 100
    eps <- 0.2
    n <- 25
    p <- 5
    
    ares <- matrix(NA, nrow=nsim, ncol=nmeth)
    atime <- matrix(NA, nrow=nsim, ncol=nmeth)
    colnames(ares) <- colnames(atime) <- meth
    tic()
    for(isim in 1:nsim) {
        cat("\n", isim)
        x <- rbwheel(n, p, frac=eps)
        n1 <- attr(x, "n1")              # number of good observations
        x0 <- x[1:n1,]                  # the 'good' observations
        for(imeth in 1:nmeth) {
    
            ##  cat("\n", isim, meth[imeth])
    
            cputime <- system.time(
            res <- tryCatch(expr={
            if(meth[imeth] == "CovOracle")                  cond(cov(x0))
                   else if(meth[imeth] == "CovClassic")     cond(CovClassic(x)$cov)
                   else if(meth[imeth] == "CovMcd")         cond(CovMcd(x)$cov)
                   else if(meth[imeth] == "CovMrcd")        cond(CovMrcd(x)$cov)
                   else if(meth[imeth] == "CovSde")         cond(CovSde(x)$cov)
                   else if(meth[imeth] == "CovMve")         cond(CovMve(x)$cov)
                   else if(meth[imeth] == "CovSest_fast")   cond(CovSest(x)$cov)
                   else if(meth[imeth] == "CovSest_rocke")  cond(CovSest(x, method="rocke")$cov)
                   else if(meth[imeth] == "CovSest_bs")     cond(CovSest(x, method="bisquare")$cov)
                   else if(meth[imeth] == "CovMMest")       cond(CovMMest(x)$cov)
                   else if(meth[imeth] == "CovOgk")         cond(CovOgk(x)$cov)
                   else if(meth[imeth] == "covRob_S")       cond(RobStatTM::covRob(x, type="Rocke")$cov)
                   else if(meth[imeth] == "covRob_MM")      cond(RobStatTM::covRob(x, type="MM")$cov)
                   else if(meth[imeth] == "covRob_Kurt")    cond(RobStatTM::initPP(x)$cova)
                   else if(meth[imeth] == "covBACON")       cond(robustX::mvBACON(x, verbose=FALSE)$cov)
                   else if(meth[imeth] == "covNNC")         cond(robustX::covNNC(x)$cov)
                   else
                    stop(paste("Unknown method:", meth[imeth]))
                   },
                   error=function(msg) {print(paste("ERROR:", meth[imeth])); NA}))
            ##  cat("  ", res)
    
            ares[isim, imeth] <- res
            atime[isim, imeth] <- cputime[1]
        }
    }
    toc()
    
    save(ares, atime, file=here::here("Data", "bench_bwheel.rda"))
    
    boxplot(ares, log="x", outline=FALSE, horizontal=TRUE, las=2)
    abline(v=1000)
    
    boxplot(atime, horizontal=TRUE, las=2)
    
    list(ares=ares, atime=atime)
}
    
    
alist <- do_bwheel_simulation(nsim=100, meth=meth)
ares <- alist$ares
atime <- alist$atime

##  Figure 9: ...

## Prepare the data. The data frame 'ares' contains the results from a simulation
load(here::here("Data", "bench_bwheel.rda"))
data <- cbind.data.frame(id=1:nrow(ares), ares)

## CovSde usually has very large outliers, cap these by 25000
data$CovSde[data$CovSde > 25000] <- 25000

data1 <- reshape::melt(data, id.vars="id", variable_name="text")
data1$text <- factor(data1$text, levels=rev(colnames(ares)))

head(data1)

##  Prepare the theme
theme_mylight_base <- function(base_size=11, base_family="sans",
        my.background="white", my.text.color="black", my.text.size=8,
        my.text.axis="grey30", my.gridsize=0.01, my.gridcolor="gray50", my.gridlinetype="dashed",
        my.strip.background="#A68776", my.strip.color="white",
        ...)
{
    half_line <- base_size/2

    theme_light(base_size = base_size, base_family = base_family) %+replace%
    theme(
          panel.border = element_rect(fill = NA, size=my.gridsize, color=my.gridcolor),
          panel.grid.minor = element_line(size=my.gridsize, linetype=my.gridlinetype, color=my.gridcolor),
          panel.grid.major = element_line(size=my.gridsize, linetype=my.gridlinetype, color=my.gridcolor),
          panel.background = element_rect(size=my.gridsize, fill=my.background, color=my.background),

          plot.title=element_text(family=base_family, face="bold", size = rel(1), hjust = 0, vjust = 1, margin = margin(b = half_line)),
          # plot.title=element_blank(),                                               # no title

          axis.text=element_text(size=my.text.size, colour=my.text.axis),
          axis.ticks = element_blank(),

          axis.title = element_text(size=my.text.size, face="bold"),                # 8pt, bold

          ## axis.text.x = element_text(angle=90, vjust=0.5),
          axis.title.x = element_text(margin=margin(t=half_line/2), vjust = 1),
          ## axis.title.x.top = element_text(margin = margin(b = half_line/2), vjust = 0),
          ## axis.title.x = element_blank(),

          axis.title.y = element_text(angle=90, margin=margin(r=half_line/2), vjust = 1),
          ## axis.title.y = element_text(size=my.text.size, face="bold", angle=90, margin=margin(r=half_line/2), vjust = 1, hjust=0.97),   # adjust the y-axis label to the top
          ## axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/2), vjust = 0)

          legend.title=element_blank(),
          legend.text=element_text(size=my.text.size, color=my.text.color),
          legend.key = element_rect(fill = my.background, colour = "transparent"),
          legend.background = element_rect(fill=my.background, color=my.background),

          strip.background = element_rect(fill=my.strip.background, color=my.gridcolor, linewidth=0.1),
          strip.text = element_text(color=my.strip.color, size=my.text.size, margin=margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
          ...
        )
}

theme_mylight <- function()
    theme_mylight_base(base_family="serif",
        my.background="white", my.text.color="black", my.text.size=10,
        my.gridcolor="grey", my.gridlinetype="solid") +
        theme(
                plot.title=element_text(face="bold", size=10),
                axis.title = element_text(hjust=1),                 # justified to the top and to the right
              )

options(scipen = 999)

breaks <- c(5, 10, 50, 100, 500, 1000, 5000, 25000)
clr <- "#377EB8"    # "black"  # "#3366FF"
clr_fill <- "#377EB8"   #   rgb(0.3,0.5,0.4,0.6)
p <- ggplot(data1, aes(x=value, y=text, fill=clr_fill, color=clr)) +
        stat_boxplot(aes(value, text), geom ='errorbar', color=clr, width = 0.4, linewidth=0.2) +
        geom_boxplot(width=0.4, fill=clr_fill, color=clr, alpha=0.3, linewidth=0.2) +
        scale_x_continuous(trans='log10', n.breaks=length(breaks), breaks=breaks, labels=breaks) +
        geom_vline(xintercept=800, linetype="solid", color = "blue", linewidth=0.5)
##        + ggtitle("n=25, p=5, nsim=50")

p <- p +
        xlab("cond(cov)") +
        ylab("") +
        theme_mylight() +
        theme(
            legend.position="none")

p

cairo_pdf(filename=here::here("Output", "bench_bwheel.pdf"), width=6, height=3.425)
print(p)
dev.off()
