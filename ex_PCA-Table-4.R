library(xtable)

##  Table 4: ...
meth <- c("PcaClassic", "PcaCov", "PcaHubert", "PcaGrid", "PcaProj", "PcaLocantore", 
          "SPcaGrid", "rospca", "ltsspca", "PCAgrid", "PCAproj", "sPCAgrid", 
          "pcaRobS", "prcompRob", "MacroPCA")

meth_desc <- c(
"Classical PCA (equivalent to \\code{prcomp()})",
"PCA based on robust covariance matrix",
"ROBPCA (Hubert et al. 2005)",
"PCA based on projection pursuit (Croux et al., 2007)",
"PCA based on projection pursuit (Croux and Ruiz-Gazen (2005))",
"Spherical PCA (Locantore et al., 1999)",
"Sparse PCA based on PP (Croux et al., 2013)",
"ROSPCA (Hubert et al., 2016)",
"Sparse PCA based on LTS (Wang and Van Aelst, 2020)",
"PCA based on projection pursuit (Croux et al., 2007)",
"PCA based on projection pursuit (Croux and Ruiz-Gazen (2005))",
"Sparse PCA based on PP (Croux et al., 2013)",
"Robust PCA based on a robust scale (Maronna, 2005)",
"Same as \\code{pcaRobS()} but returns an object similar to \\code{prcomp}",
"ROBPCA for missing values and cellwise outliers (Hubert et al., 2019)")

meth_R <- c("rrcov", "rrcov", "rrcov", "rrcov", "rrcov", "rrcov", "rrcovHD",
        "rospca", "ltsspca", "pcaPP" , "pcaPP" , "pcaPP", 
        "RobStatTM", "RobStatTM", "cellWise")

(dfmeth <- data.frame(`R package`=meth_R, Function=meth, Description=meth_desc))

df2 <- dfmeth
df2[,1] <- paste0("\\pkg{", df2[,1], "}")
df2[,2] <- paste0("\\code{", df2[,2], "}")
colnames(df2) <- paste0("\\textbf{", colnames(df2), "}")
print(xtable(df2), include.rownames=FALSE, sanitize.text.function = identity,
    sanitize.colnames.function = identity)


##==============================================================================
library(RobStatTM)
data(bus)
X0 <- as.matrix(bus)
X1 <- X0[,-9]
ss <- apply(X1, 2, mad)
mu <- apply(X1, 2, median)
X <- scale(X1, center=mu, scale=ss)
q <- 3  #compute three components
rr <- pcaRobS(X, q, 0.99)
round(rr$eigvec, 3)

(rrx <- prcomp(X, rank.=3))
(rry <- prcompRob(X, rank.=3))


