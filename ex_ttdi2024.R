##  WEF Travel & Tourism Development Index May, 2024
##
##  See Kalina and Vidnerova, 2020
##      Kalina et al. 2019
##

library(conflicted) # avoid masking of functions
library(pracma)     # for tictoc
library(glmnet)
library(corrplot)   # for the correlation plot
library(ggplot2)
library(ggrepel)
library(latex2exp)  # for TeX() - Latex in ggplot graphs
library(readxl)
library(xtable)     
library(gridExtra)  # for grid.arrange()

here::i_am("ex_ttdi2024.R")
library(here)

##  Table 6:  List of the 17 pillars of the Travel and Development Index 2024 of 
##      the World Economic Forum. The response variable is TSI - Tourist Services 
##      and Infrastructure and the regressors are the rest 16 pillars.

##  Figure 17: Travel and tourism development index data 2024: Left-hand panel: 
##      Coefficient path for the sparse LTS, plotted versus the penalty parameter λ 
##      and right-hand panel: cross-validated estimate of the robust root mean-squared 
##      prediction error for sparse LTS together with upper and lower standard 
##      deviation curves, as a function of the penalty parameter λ (on a log scale). 
##      The value of λ which gives the minimal RTMSPE and the largest value of λ such 
##      that the error is within one standard error of the minimum are indicated 
##      by vertical dotted lines. 

##  Figure 18: Travel and tourism development index data 2024: Robust regression 
##      diagnostic plots with 7 non-zero coefficients. Left-hand panel: normal 
##      QQ-plot of the standardized residuals against the quantiles of the standard 
##      normal distribution; Right-hand panel: standardized residuals against 
##      robust Mahalanobis distances.

## Figure 19: Travel and tourism development index data 2024: Robust regression 
##      diagnostic plots with 7 non-zero coefficients. Left-hand panel: Index 
##      plot of the residuals; Right-hand panel: plot of the residuals vs. 
##      fitted values. 

if(FALSE){
    ## Read data from the original Excel file and write to a CSV file
    df <- as.data.frame(read_excel(path=here::here("Data", "WEF_TTDI_2024_edition_data.xlsx"), sheet=1, skip=1))
    dim(df)
    head(df) 
    
    ## Select columns: 
    ##  - skip the first five columns (country information), but keep country code and name
    ##  - skip the next 60 columns: 6 dimensions, each with 10 columns
    ##  - select the fifth column from each pilar
    cols <- c(1, 2, 5 + 60 + seq(5, ncol(df)-5-60, by=10))
    df <- df[, cols]
    
    ## Dimensions (skipped)
    ##  Travel & Tourism Development Index
    ##  Enabling Environment dimension
    ##  Travel and Tourism Policy and Enabling Conditions dimension
    ##  Infrastructure and Services dimension
    ##  Travel and Tourism Resources dimension
    ##  Travel and Tourism Sustainability dimension
    
    ## Pilars
    ##  BE - Business Environment pillar
    ##  SS - Safety and Security pillar
    ##  HH - Health and Hygiene pillar
    ##  HRLM - Human Resources and Labour Market pillar
    ##  ICT - ICT Readiness pillar
    ##  TT - Prioritization of Travel & Tourism pillar
    ##  IO - International Openness pillar
    ##  PC - Price competitiveness pillar
    ##  ATI -   Air Transport Infrastructure pillar
    ##  GPI - Ground and Port Infrastructure pillar
    ##  TSI - Tourist Services and Infrastructure pillar
    ##  NR - Natural Resources pillar
    ##  CR - Cultural Resources pillar
    ##  NLR - Non-Leisure Resources pillar
    ##  ES - Environmental Sustainability pillar
    ##  SI - T&T Socioeconomic Impact pillar
    ##  DS - T&T Demand Sustainability pillar									
    
    cnames <- c("ISO", "Country", "BE", "SS", "HH", "HRLM", "ICT", "TT", "IO", "PC", "ATI", "GPI", "TSI", "NR", "CR", "NLR", "ES", "SI", "DS")
    colnames(df) <- cnames
    
    ##  Remove the first two columns which contains country code and name and set it as
    ##  row names
    row.names(df) <- df[,1]
    df <- df[,-c(1:2)]
    write.csv(df, file=here::here("Data", "ttdi2024.csv"))
}

##  Do not run this!!!
if(FALSE){
    pillars <- read.csv(file=here::here("Data", "ttdi2024-pillars.csv"), header=FALSE)
    pillars$V2 <- gsub(" pillar", "", pillars$V2)
    pillars <- cbind.data.frame(ID=1:nrow(pillars), pillars)
    colnames(pillars) <- c("ID", "Abbreviation", "Pillar")
    pillars
    write.csv(pillars, file=here::here("Data", "ttdi2024-pillars.csv"), row.names=FALSE)
}

##  Table 6 ===================================================================
(pillars <- read.csv(file=here::here("Data", "ttdi2024-pillars.csv")))
print(xtable(pillars), include.rownames=FALSE)

##  Read the data 
df <- read.csv(file=here::here("Data", "ttdi2024.csv"), row.names=1)

##  Correlaion plot ===========================================================
corr <- cor(df)
corrplot(corr)
##  corrplot(corr, method="number")

cairo_pdf(filename=here::here("Output", "ttdi-corrplot.pdf"), width=7.5, height=7.5)
corrplot(corr)
dev.off()

##  Prepare the data for regression ===========================================
##  Use the function model.matrix to extract the X-matrix
##  Remove the first column which contains 1s (for the intercept)
##
df <- read.csv(here::here("Data", "ttdi2024.csv"), row.names=1)
x <- model.matrix(TSI~., data=df)[, -1]
dim(x)
head(x)
y <- df$TSI

##  Multicollinearity?
## X <- scale(x)
cond(t(x) %*% x)
##  [1] 5424.833

##=============================================================================
##  1: Perform classical ridge regression and LASSO and compare the models
##

set.seed(67890)

## Perform cross validation for Ridge regression
ridge_cv <- cv.glmnet(x, y, alpha=0)
coef(ridge_cv)

(min_lambda <- ridge_cv$lambda.min)
ridge <- glmnet(x, y, alpha=0, lambda=min_lambda)
length(which(ridge$beta != 0))
(v_ridge <- row.names(ridge$beta)[which(ridge$beta != 0)])

## Perform cross validation for classical LASSO
lasso_cv <- cv.glmnet(x, y, family="gaussian", alpha=1)
coef(lasso_cv)

(min_lambda <- lasso_cv$lambda.min)
(best_lambda <- lasso_cv$lambda.1se)
lasso_min <- glmnet(x, y, family="gaussian", alpha=1, lambda=min_lambda)
length(which(lasso_min$beta != 0))
(v_lasso_min <- row.names(lasso_min$beta)[which(lasso_min$beta != 0)])

lasso_1se <- glmnet(x, y, family="gaussian", alpha=1, lambda=best_lambda)
length(which(lasso_1se$beta != 0))
(v_lasso_1se <- row.names(lasso_1se$beta)[which(lasso_1se$beta != 0)])

## Coefficients path plot for ridge regression
ridge1 <- glmnet(x, y, alpha=0)
plot(ridge1, xvar="lambda", cex.lab=1.4)
savePlot(file=here::here("Output", "ttdi-coefficients-ridge.pdf"), type="pdf")

## Coefficients path plot for LASSO
lasso1 <- glmnet(x, y, alpha=1)
plot(lasso1, xvar="lambda", cex.lab=1.4)
savePlot(file=here::here("Output", "ttdi-coefficients-lasso.pdf"), type="pdf")

## Cross validation plot for LASSO
plot(lasso_cv)

df <- data.frame(lambda=log(lasso_cv$lambda), 
    cvm=lasso_cv$cvm, low=lasso_cv$cvlo, 
    upper=lasso_cv$cvup, nzero=lasso_cv$nzero)

gg <- ggplot(df, aes(x=lambda, y=cvm)) +
    geom_point(color="#cb4154") + 
    geom_ribbon(aes(ymin=low, ymax=upper), alpha=0.2) + 
    geom_vline(xintercept=log(c(lasso_cv$lambda.min, lasso_cv$lambda.1se)), 
        linetype="dotted", linewidth=1.0) + 
    theme_bw() +
        theme(plot.title=element_text(size=14, face="bold", hjust=0.5),
              axis.title=element_text(size=14,face="bold")) +
        ##ggtitle(TeX(r"( $\gamma^2 = \alpha^2 + \beta^2$ )") ) +
        ggtitle(TeX(r"( LASSO  )") ) +
        xlab(TeX(r"( $log(\lambda)$ )") ) +
        ylab("Mean-Squared Error")
gg

cairo_pdf(filename=here::here("Output", "ttdi-crossval-lasso.pdf"), width=7.5, height=5)
gg
dev.off()

## Cross validation plot for ridge regression
plot(ridge_cv)

df <- data.frame(lambda=log(ridge_cv$lambda), 
    cvm=ridge_cv$cvm, low=ridge_cv$cvlo, 
    upper=ridge_cv$cvup, nzero=ridge_cv$nzero)

gg <- ggplot(df, aes(x=lambda, y=cvm)) +
    geom_point(color="#cb4154") + 
    geom_ribbon(aes(ymin=low, ymax=upper), alpha=0.2) + 
    geom_vline(xintercept=log(c(ridge_cv$lambda.min, ridge_cv$lambda.1se)), 
        linetype="dotted", linewidth=1.0) + 
    theme_bw() +
        theme(plot.title=element_text(size=14, face="bold", hjust=0.5),
              axis.title=element_text(size=14,face="bold")) +
        ##ggtitle(TeX(r"( $\gamma^2 = \alpha^2 + \beta^2$ )") ) +
        ggtitle(TeX(r"( Ridge regression  )") ) +
        xlab(TeX(r"( $log(\lambda)$ )") ) +
        ylab("Mean-Squared Error")
gg

cairo_pdf(filename=here::here("Output", "ttdi-crossval-ridge.pdf"), width=7.5, height=5)
gg
dev.off()


##=============================================================================
##  2: Perform sparse LTS and compare to the classical LASSO
##  from the previous example

library(robustHD)
library(parallel)       # Create parallel cluster

detectCores(logical = FALSE)
cluster <- makeCluster(10)
clusterExport(cluster, "sparseLTS")

tic()
lambda <- seq(0.01, 0.5, length.out = 50)
fit25 <- sparseLTS(x, y, alpha=0.75, lambda=lambda, mode="fraction", crit="PE",
                 splits = foldControl(K=10, R=10), seed=20210507, cl=cluster)
toc()

## PE min, reweighted and the corresponding lambda
(pe25_min <- min(fit25$pe[,2]))
(lambda25_min <- fit25$tuning[which.min(fit25$pe[,2]), 1])

save(fit25, file=here::here("Data", "ttdi-fit25.rda"))

##==============================================================================

load(file=here::here("Data", "ttdi-fit25.rda"))

## lambda 1se (hastie), reweighted
(pe25 <- fit25$pe[fit25$best[1], 2])
(lambda25 <- fit25$tuning[fit25$best[1],])

##==============================================================================
##  Figure 17: right-hand panel
##  Plot PE for bdp=0.25 
##

df <- data.frame(lambda=log(fit25$tuning), 
    cvm=fit25$pe$reweighted, low=fit25$pe$reweighted-fit25$se$reweighted, 
    upper=fit25$pe$reweighted+fit25$se$reweighted)
(lambda.min <- fit25$tuning[which.min(fit25$pe$reweighted),1])
(lambda.1se <- fit25$tuning[fit25$best[1],1])
    
p1 <- ggplot(df, aes(x=lambda, y=cvm)) +
    geom_point(color="#cb4154") + 
    geom_ribbon(aes(ymin=low, ymax=upper), alpha=0.2) + 
    ##scale_x_reverse() +
    geom_vline(xintercept=log(c(lambda.min, lambda.1se)), linetype="dotted", linewidth=1) + 
    theme_bw(base_size=18) +
        theme(plot.title=element_text(face="bold", hjust=0.5),
              axis.title=element_text(face="bold")) +
        ##ggtitle(TeX(r"( $\gamma^2 = \alpha^2 + \beta^2$ )") ) +
        ##ggtitle(TeX(r"( Sparse LTS: $bdp=0.25$ )") ) +
        xlab(TeX(r"( $log(\lambda)$ )") ) +
        ylab("RTMSPE")
p1

##==============================================================================
##  Figure 17: left-hand panel
##  Coefficients path for Sparse LTS
##

lambda <- seq(0.01, 0.5, length.out = 50)
fit_coef <- sparseLTS(x, y, lambda = lambda, mode = "fraction")

library(RColorBrewer)
nb.cols <- 18       # Define the number of colors you want
mycolors <- colorRampPalette(brewer.pal(8, "RdBu"))(nb.cols)

p2 <- plot(fit_coef, labels=NA) + 
    scale_color_manual(values=mycolors) +
#    scale_color_viridis(discrete=TRUE, option="viridis") +
    theme_bw(base_size=18) +
    theme(plot.title=element_text(face="bold", hjust=0.5),
          axis.title=element_text(face="bold")) +
    ##ggtitle(TeX(r"( $\gamma^2 = \alpha^2 + \beta^2$ )") ) +
    ##ggtitle(TeX(r"( Sparse LTS $(bdp=0.25)$ )") ) +
    xlab(TeX(r"( $\lambda$ )") )
p2

cairo_pdf(filename=here::here("Output", "ttdi-sparseLTS.pdf"), width=15, height=5)
grid.arrange(p2, p1, nrow=1)
dev.off()

##=============================================================================
##  Fig. 18 and 19
##
##  Diagnostic plots (for bdp=25%)
##

fit <- sparseLTS(x, y, alpha=0.75, lambda=0.02887073)
length(which(coef(fit) !=0))

##  Set the random seed befor ethe call to setupDiagnosticPlot.sparseLTS(), beacuse
##  subsecuqnt calls to MCD could produce different solutions (and thus different rd-distances).
##  Use the same seed before the call to plot(..., which+"rdiag") 
set.seed(2345)

objx <- robustHD:::setupDiagnosticPlot.sparseLTS(fit)
iout <- which(objx$data$residual > sqrt(qchisq(0.975, 1)))      # vertical outliers
ird <- which(objx$data$rd > objx$q[1,1])                        # leverage points
igood <- ird[which(!(ird %in% iout))]                           # good leverage points

## vertical outliers
xout <- objx$data[iout,]
xout$names <- rownames(xout)

## good leverage points
yout <- objx$data[igood,]
yout$names <- rownames(yout)

## Choose the colors: from RColorBrewer (qualitative), Set 1
colscheme <- c("Regular observation"="#377eb8", "Potential outlier"="#e41a1c")

p3 <- plot(fit, method="diagnostic", which="rqq", id.n=0) + 
        geom_text_repel(aes(x = theoretical, y = residual, label=names), data=xout, hjust = 0, size=4.5, alpha=0.6) +
        scale_color_manual(values = colscheme) +
        theme_bw(base_size=18) +
        theme(legend.position="bottom", 
            plot.title=element_text(face="bold", hjust=0.5),
            axis.title=element_text(face="bold")) 
p3

set.seed(2345)
p4 <- plot(fit, method="diagnostic", which="rdiag", id.n=0) + 
        geom_text_repel(aes(x = rd, y = residual, label=names), data = xout, hjust = 0,
                       size=4.5, alpha = 0.6) +
        geom_text_repel(aes(x = rd, y = residual, label=names), data = yout, hjust = 0,
                       size=4.5, alpha = 0.6) +
        scale_color_manual(values = colscheme) +
        theme_bw(base_size=18) +
        theme(legend.position="bottom", 
            plot.title=element_text(face="bold", hjust=0.5),
            axis.title=element_text(face="bold")) 
p4

cairo_pdf(filename=here::here("Output", "ttdi-sparseLTS-diag-1.pdf"), width=15, height=7.5)
grid.arrange(p3, p4, nrow=1)
dev.off()

p5 <- plot(fit, method="diagnostic", which="rindex", id.n=0) + 
        geom_text_repel(aes(x = index, y = residual, label=names), data = xout, hjust = 0,
                       size=4.5, alpha = 0.6) +
        scale_color_manual(values = colscheme) +
        theme_bw(base_size=18) +
        theme(legend.position="bottom", 
        plot.title=element_text(face="bold", hjust=0.5),
        axis.title=element_text(face="bold")) 
p5

p6 <- plot(fit, method="diagnostic", which="rfit", id.n=0) + 
        geom_text_repel(aes(x = fitted, y = residual, label=names), data = xout, hjust = 0,
                       size=4.5, alpha = 0.6) +
        scale_color_manual(values = colscheme) +
        theme_bw(base_size=18) +
        theme(legend.position="bottom", 
            plot.title=element_text(face="bold", hjust=0.5),
            axis.title=element_text(face="bold")) 
p6

cairo_pdf(filename=here::here("Output", "ttdi-sparseLTS-diag-2.pdf"), width=15, height=7.5)
grid.arrange(p5, p6, nrow=1)
dev.off()
