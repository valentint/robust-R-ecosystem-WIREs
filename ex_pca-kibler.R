library(conflicted) # avoid masking of functions
library(rrcov)
library(rrcovHD)    # for the kibler data set
library(pcaPP)      # for qn
library(corrplot)   # for the correlation plot
library(pracma)
library(xtable)

library(rospca)
library(ltsspca)

here::i_am("ex_pca-kibler.R")
library(here)
source("sparse-functions.R")

##  Figure XX:  Correlation plot of the kibler data set.

##  Figure 12: Plots of the first two PCA scores for classical PCA, robust PCA based 
##      on MM-estimate of the covariance matrix, robust PCA based on projection 
##      pursuit and the ROBCA algorithm. The marked observations are the cars 
##      with diesel engines.

##  Figure 13: Diagnostic plots for classical PCA, robust PCA based on MM-estimate 
##      of the covariance matrix, robust PCA based on projection pursuit and the 
##      ROBCA algorithm. The marked observations are the cars with diesel engines.

##  Figure 14: Scaled outlier maps of standard PCA, SCOTLASS (with λ = 1.48), 
##      ROBPCA (non-sparse), ROSPCA (with λ = 1.075), SRPCA (with λ = 3.09) and 
##      LTSSPCA on the kibler data. The marked observations are the cars 
##      with diesel engines. 

##  Table 4: Number of non-zero loadings (larger than 1e-5) for the kibler data 
##      for each method per PC. The PS row shows the number variables in the PC 
##      subspace. The last row shows the number of variables that were excluded 
##      form the model (i.e. have zero loadings on all 4 PCs)


## Prepare the data ===========================================================
##  The data set 'kibler' contains also a data froma 'kibler.orig', the original 
##      data set, with the 'make' and 'fuel-type' variables. Keep only the 
##      continuous variables, remove the observations with missing values.

if(FALSE) {
    data(kibler)
    dim(kibler)
    head(kibler)
    rownames(kibler) <- NULL
    
    dim(kibler.orig)
    head(kibler.orig)
    
    df <- kibler.orig[, c(2, 3, 26, 9:13, 18:25)]
    df$symboling <- as.double(as.character(df$symboling))       # symboling was factor
    xmiss <- apply(df, 1, function(x) length(which(is.na(x))))  # which obs have missings
    length(which(xmiss > 0))                                    # 10, remove these
    
    df[xmiss > 0, ]
    df <- df[xmiss == 0, ]
    row.names(df) <- NULL
    
    all.equal(kibler, df[, 3:ncol(df)])
    
    kibler <- df
    save(kibler, kibler.orig, file=here::here("Data", "kibler.rda"))
}

##=============================================================================
##
##  Load the data
##
load(here::here("Data", "kibler.rda"))
fuel_type <- kibler[, 2]
xdat <- kibler[, 3:ncol(kibler)]

##
##  Set the colorscheme: outliers/non-outliers (diesel/gasoline)
##
col1 <- rep("#377eb8", nrow(xdat))          # darkblue for regular
col1[fuel_type == "diesel"] <- "#e41a1c"    # darkred for outlier
pch1 <- rep(1, nrow(xdat))
pch1[which(fuel_type == "diesel")] <- 16

##
##  Figure XX: Correlation plot of the kibler data set.
##  (not in the article)
##
corr <- cor(xdat, method="spearman")
corrplot(corr)

cairo_pdf(filename=here::here("Output", "kibler-corrplot.pdf"), width=7.5, height=7.5)
corrplot(corr)
dev.off()

## Calculate VAR for each variable
xsd <- apply(xdat, 2, sd)
cat("\nMin, Max of sd: ", min(xsd), max(xsd), "\n")

## Calculate Qn for each variable
xqn <- apply(xdat, 2, qn)
cat("\nMin, Max of Qn: ", min(xqn), max(xqn), "\n")

x0 <- as.matrix(sweep(xdat, 2, xsd, "/", check.margin = FALSE))
x <- as.matrix(sweep(xdat, 2, xqn, "/", check.margin = FALSE))
p <- ncol(x)

## Find optimal number of components k =========================================
pc1 <- PcaGrid(x0, method="sd", k=p, kmax=p)
pc2 <- PcaClassic(x0)

summary(pc1)
##  0.4892 0.6695 0.75883 0.82750

rpc1 <- PcaGrid(x, method="qn", k=p, kmax=p)                        # GRID
rpc2 <- PcaCov(x, k=p, kmax=p)                                      # MCD
rpc3 <- PcaCov(x, k=p, kmax=p, cov.control=CovControlMMest())       # MM
rpc4 <- PcaHubert(x, k=p, kmax=p, alpha=0.56)                       # ROBPCA with alpha=0.56

summary(rpc1)
##  0.4828 0.6492 0.7525 0.83277
summary(rpc2)
##  0.4373 0.6545 0.7807 0.86486
summary(rpc3)
##  0.5194 0.6663 0.7604 0.83570
summary(rpc4)
##  0.4482 0.6538 0.7716 0.85408

##  Figure XX: Screeplot for classical PCA, robust PCA 
##      based on MM-estimate of the covariance matrix, robust PCA based on 
##      projection pursuit and the ROBCA algorithm.

cairo_pdf(filename=here::here("Output", "kibler-scree-plot.pdf"), width=7.5, height=7.5)
opar <- par(mfrow=c(4,1))
screeplot(pc1, k=p, type="line", main="Classical PCA", sub="PC", cex.main=2)
screeplot(rpc1, k=p, type="line", main="GRID", sub="PC", cex.main=2)
screeplot(rpc2, k=p, type="line", main="MCD", sub="PC", cex.main=2)
screeplot(rpc3, k=p, type="line", main="MM", sub="PC", cex.main=2)
par(opar)
dev.off()

##  Recompute no the PCA with the selected k=4
pc1 <- PcaClassic(x0, k=4)         
rpc1 <- PcaGrid(x, k=4)            
rpc3 <- PcaCov(x, k=4, kmax=p, cov.control=CovControlMMest())
rpc4 <- PcaHubert(x, k=4, alpha=0.56) 

##==============================================================================
##  Figure 12: 
##      Plots of the first two PCA scores for classical PCA, robust PCA 
##      based on MM-estimate of the covariance matrix, robust PCA based on 
##      projection pursuit and the ROBCA algorithm. The marked observations are 
##      the cars with diesel engines.
##

cairo_pdf(filename=here::here("Output", "kibler-scoreplot.pdf"), width=7.5, height=7.5)
opar <- par(mfrow=c(2,2))
scorePlot(pc1, main="(a) Classical PCA", col=col1, pch=pch1, id.n=0)
scorePlot(rpc3, main="(b) MM", col=col1, pch=pch1, id.n=0)
scorePlot(rpc1, main="(c) PCA based on PP", col=col1, pch=pch1, id.n=0)
scorePlot(rpc4, main="(d) ROBPCA", col=col1, pch=pch1, id.n=0)
par(opar)
dev.off()

##==============================================================================
##  Figure 13: 
##      Diagnostic plots for classical PCA, robust PCA based on MM-estimate 
##      of the covariance matrix, robust PCA based on projection pursuit and 
##      the ROBCA algorithm. The marked observations are the cars with diesel engines.
##

cairo_pdf(filename=here::here("Output", "kibler-dd.pdf"), width=7.5, height=7.5)
opar <- par(mfrow=c(2,2))
plot(pc1, main="(a) Classical PCA", col=col1, pch=pch1, id.n.sd=0, id.n.od=0)
plot(rpc3, main="(b) MM", col=col1, pch=pch1, id.n.sd=0, id.n.od=0)
plot(rpc1, main="(c) PCA based on PP", col=col1, pch=pch1, id.n.sd=0, id.n.od=0)
plot(rpc4, main="(d) ROBPCA", col=col1, pch=pch1, id.n.sd=0, id.n.od=0)
par(opar)
dev.off()

##==============================================================================
##
##  Sparse PCA
##
##  We have selected the number of components:

K <- 4

## Find the degree of sparseness lambda ========================================

## 1. First find lambda for classical sparse PCA aka SCOTLASS  =================

## 1.1 Find lambda for SCOTLASS using the BIC_CFF
tic()
oBIC <- opt.BIC(x0, k.max=4, method="sd", n.lambda=101)
toc()
plot(oBIC, f.x="lambda")
oBIC$opt$pc[[4]]$lambda[1]
##  lambda_opt = 2.09777

## 1.2 Find lambda for SCOTLASS using the TPO_CFF
tic()
oTPO <- opt.TPO(x0, k.max=4, method="sd", n.lambda=101)
toc()
plot(oTPO, f.x="lambda")
oTPO$opt[[4]]$pc[[1]]$lambda[1]
##  lambda_opt =  1.47948

## 1.3 Find lambda for SCOTLASS using the ROSPCA utility
sl_SCoTLASS <- selectLambda(x, k=K, method="SCoTLASS", stan=FALSE)
selectPlot(sl_SCoTLASS)
sl_SCoTLASS$opt.lambda
##  [1] 0.54

## 1.4 Calculate the Index of sparseness (IS)
xx <- IS.BIC(x0, oBIC)
plot.IS(xx)
xx$tab$lambda[which.max(xx$tab$IS)]
##  [1] 1.47948

## SCOTLASS: lambda.opt: 
##  BIC_CFF=2.1 (PEV=28%, full sparseness), 
##  TPO_CFF=1.48 (PEV=69%, excludes 2 of the 14 variables, three of the components are with only one variable), 
##  BIC_HUB=0.54 (PEV=78%, does not exclude any of the 14 variables), 
##                                         PC1  PC2  PC3  PC4   PS EXCL 
##                                          13    9    4    2   14    0 
##  IS=1.48      (PEV=69%, excludes 2 of the 14 variables, three of the components are with only one variable) 

pc1 <- SPcaGrid(x0, k=4, lambda=1.48, method="sd")
(v1 <- dfs(pc1$loadings))

## PC1  PC2  PC3  PC4   PS EXCL 
##   9    1    1    1   12    2 

100*pev(x0, pc1, method="sd")$PEV
##  [1] 68.68595

##==============================================================================
## 2. Next we select lambda for ROSPCA

## 2.1 Find lambda for ROSPCA using the BIC_HUB
tic()
sl_rospca <- selectLambda(x, k=K, alpha=0.5, method="ROSPCA", stan=FALSE)
toc()
##  elapsed time is 6.55 seconds

selectPlot(sl_rospca)
sl_rospca$opt.lambda
##  0.36

## 2.2. Calculate the Index of sparseness (IS) for ROSPCA
tic()
xx <- IS.rospca(x, k=4, alpha=0.5, lambda.n=101)
toc()

plot.IS(xx)
xx$tab$lambda[which.max(xx$tab$IS)]
##  [1] 1.7

## ROSPCA: lambda.opt: 
##  BIC_HUB=0.36 (PEV=62%, does not exclude any of the 14 variables), 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                  14   11   10   12   14    0 
##  IS=1.7       (PEV=58%, excludes 1 of the 14 variables, two of the components are with only one variable) 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                  11    1    1    6   13    1 
##  IS=1.0757    (PEV=64%, does not exclude any 1 of the 14 variables, two of the components are with only one variable) 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                  11    1    1    3   14    0 
  
pc3 <- rospca(x, k=4, lambda=1.075, alpha=0.5, stan=FALSE)
(v3 <- dfs(pc3$loadings))

## PC1  PC2  PC3  PC4   PS EXCL 
##  11    1    1    6   13    1 
100*pev(x, pc3$loadings, pc3$eigenvalues, method="qn")$PEV
##  58.29073

##==============================================================================
## 3. Next we select lambda for SPCAGRID

## 3.1 Find lambda for SPCAGRID using the BIC_CFF
tic()
oBIC <- opt.BIC(x, k.max=4, method="qn", n.lambda=101)
toc()
##  elapsed time is 155.460000 seconds 

plot(oBIC, f.x="lambda")
oBIC$opt$pc[[4]]$lambda[1]
##  lambda_opt = 18.30745

## 3.2 Find lambda for SPCAGRID using the TPO_CFF
tic()
oTPO <- opt.TPO(x, k.max=4, method="qn", n.lambda=101)
toc()
##  elapsed time is 212.000 seconds 

plot(oTPO)
oTPO$opt[[4]]$pc[[1]]$lambda[1]
##  lambda_opt = 3.090868

## 3.3 Find lambda for SRPCA using the ROSPCA utility
##  this takes too long...
tic()
sl_srpca <- selectLambda(x, k=4, method="SRPCA", stan=FALSE)
toc()
##  elapsed time is 447.360000 seconds 

selectPlot(sl_srpca)
sl_srpca$opt.lambda
##  [1] 0.7

## 3.4 Calculate the Index of sparseness (IS)
xx <- IS.BIC(x, oBIC)
plot.IS(xx)
xx$tab$lambda[which.max(xx$tab$IS)]
##  [1] 3.090868

##  SRPCA(GRID): lambda.opt
##  BIC_CFF=18.3 (PEV=29%, excludes 10 out of 14 variables, full sparseness), 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                   1    1    1    1    4   10 
##  BIC_TPO=3.09 (PEV=103% !!!!, does not exclude any of the 14 variables), 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                  10    5    1    1   14    0 
##  BIC_HUB=0.7 (PEV=140% !!!!, no sparseness at all!!!!), 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                  14   14   14   14   14    0 
##  IS=3.09      (PEV=103% !!!!, does not exclude any of the 14 variables), 
##                                 PC1  PC2  PC3  PC4   PS EXCL 
##                                  10    5    1    1   14    0 

pc4 <- SPcaGrid(x, k=4, lambda=3.09, method="qn")
(v4 <- dfs(pc4$loadings))
## PC1  PC2  PC3  PC4   PS EXCL 
##  10    5    1    1   14    0 
100*pev(x, pc4$loadings, pc4$eigenvalues, method="qn")$PEV
##  [1] 102.8701


##==============================================================================
## 4. LTSSPCA

## Robust, non-sparse
lpca <- ltspca(x, q=4, alpha=0.5)

## Sparse, non-robust (Shen and Huang (2008))
spca <- sPCA_rSVD(x, k=4)

## Robust sparse and Reweighted robust sparse
pc5x <- ltsspca(x=x, kmax=4, alpha=0.5)
pc5 <- ltsspcaRw(x=x, obj=pc5x, k=4, alpha=0.5)

xx <- mydiagPlot(x, pc5, k=4, alpha=0.5, co.sd=0.25)     # does not return invisible
xxx.ddplot(xxx.distances(x, pc5, "LTSSPCA"), main="LTSSPCA")

(v5 <- dfs(pc5$loadings))
## PC1  PC2  PC3  PC4   PS EXCL 
##  12    8    9    5   14    0 

100*pev(x, pc5$loadings, pc5$eigenvalues, method="qn")$PEV
##  [1] 59.57026

##==============================================================================
## 5. Run ROBPCA, ROSPCA, SRPCA and LTSSPCA with the selected lambda ===========

pc0 <- PcaClassic(x0, k=4)
(v0 <- dfs(pc0$loadings))

## SCOTLASS: lambda.opt = 1.48
pc1 <- SPcaGrid(x0, k=4, lambda=1.48, method="sd")
(v1 <- dfs(pc1$loadings))

pc2a <- PcaHubert(x, k=4, alpha=0.5)
(v2a <- dfs(pc2a$loadings))

pc2b <- PcaGrid(x, k=4, method="qn")
(v2b <- dfs(pc2b$loadings))

##  ROSPCA: lambda.opt=1.075, alpha=0.5
pc3 <- rospca(x, k=4, lambda=1.075, alpha=0.5, stan=FALSE)
(v3 <- dfs(pc3$loadings))

##  SRPCA(GRID): lambda.opt=3.8
pc4 <- SPcaGrid(x, k=4, lambda=3.09, method="qn")
(v4 <- dfs(pc4$loadings))

##  LTSSPCA:
pc5x <- ltsspca(x=x, kmax=4, alpha=0.5)
pc5 <- ltsspcaRw(x=x, obj=pc5x, k=4, alpha=0.5)
(v5 <- dfs(pc5$loadings))

##  Table 4: Number of non-zero loadings (larger than 1e-5) for the kibler data 
##      for each method per PC. The PS row shows the number variables in the PC 
##      subspace. The last row shows the number of variables that were excluded 
##      form the model (i.e. have zero loadings on all 4 PCs)
##
(df_tab1 <- cbind.data.frame(ROSPCA=v3, SRPCA=v4, LTSSPCA=v5, SCOTLASS=v1, ROBPCA=v2a, GRID=v2b))
xtable(df_tab1)

## =============================================================================
##  Angle between the spaces
apca <- list(PCA=pc0, SCOTLAS=pc1, ROBPCA=pc2a, RPCAGRID=pc2b, ROSPCA=pc3, SRPCA=pc4, LTSPCA=pc5)
amat <- matrix(NA, nrow=length(apca), ncol=length(apca))
rownames(amat) <- colnames(amat) <- names(apca)

for(i in 1:length(apca))
    for(j in i:length(apca)) {
        amat[i,j] <- rospca::angle(apca[[i]]$loadings, apca[[j]]$loadings)
    }
round(t(amat),4)

##==============================================================================
##  Figure 14: 
##      Scaled outlier maps of standard PCA, SCOTLASS (with λ = 1.48), 
##      ROBPCA (non-sparse), ROSPCA (with λ = 1.075), SRPCA (with λ = 3.09) and 
##      LTSSPCA on the kibler data. The marked observations are the cars with diesel engines.
##

cairo_pdf(filename=here::here("Output", "kibler-sparse-dd-plot.pdf"), width=7.5, height=7.5)
opar <- par(mfrow=c(2,3))
xxx.ddplot(xxx.distances(x0, pc0), main="Standard PCA", col=col1, pch=pch1)
xxx.ddplot(xxx.distances(x0, pc1), main="SCOTLASS", col=col1, pch=pch1)
xxx.ddplot(xxx.distances(x, pc2a), main="ROBPCA", col=col1, pch=pch1)
xxx.ddplot(xxx.distances(x, pc3, "ROSPCA"), main="ROSPCA", col=col1, pch=pch1)
xxx.ddplot(xxx.distances(x, pc4), main="SRPCA(GRID)", col=col1, pch=pch1)
xxx.ddplot(xxx.distances(x, pc5, "LTSSPCA"), main="LTSSPCA", col=col1, pch=pch1)
par(opar)
dev.off()
