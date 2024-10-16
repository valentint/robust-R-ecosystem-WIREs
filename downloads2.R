library(conflicted) # avoid masking of functions
library(cranlogs)
library(data.table)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(plyr)
library(packagefinder)

here::i_am("downloads2.R")
library(here)

##  Figure 2
##  Top 15 R packages for robust statistics by total downloads in millions 
##      (left-hand panel) and average monthly downloads (K/month) (right-hand panel) 
##      from October 2012 to April 2024. Source of data: http://cran-logs.rstudio.com/.

##  Figure 3:
##  Monthly downloads of the Top 5 R packages for robust statistics in the period 
##      October 2012 to April 2024. Source of data: http://cran-logs.rstudio.com/.


getDownloads <- function(p, date1, date2, n=100) {

    ## Set a range of dates and names of the packages to track
    if(missing(date1))
        date1 <- "2012-10-01"
    
    if(missing(date2)) {
        date2 <- Sys.Date()                                 # Today
        date2 <- as.Date(format(date2, "%Y-%m-01"))         # Start of this month
        date2 <- date2 - 1                                  # End of last month
    }

    X <- NULL
    while(length(p) > 0){
        ind <- 1:min(n, length(p))
        P <- p[ind]; print(length(P))
        p <- p[-ind]
    
        D <- cran_downloads(from = date1, to = date2, packages = P)
        D <- setDT(D)
        setnames(D, "package", "Package") 
        
        ## Aggregate by month  
        d <- D[, .(Count=sum(count)), by = .(Year=year(date), Month=months(date), Package)]
        
        ## Create the date variable for plotting
        x <- d[, ':='(Date = as.Date(paste0(Year, "-", Month, "-01"), format = "%Y-%B-%d"))]            
        x <- x[x$Count > 0,]        # Only the months with positive number of downloads
    
        X <- rbind(X, x)
    }
    
    downloads.total <- ddply(X, .variables=c("Package"), .fun=function(x) sum(x$Count))
    downloads.monthly <- ddply(X, .variables=c("Package"), .fun=function(x) round(mean(x$Count)))
    
    downloads.total <- downloads.total[order(downloads.total$V1, decreasing=TRUE),]
    downloads.monthly <- downloads.monthly[order(downloads.monthly$V1, decreasing=TRUE),]
    rownames(downloads.total) <- NULL
    rownames(downloads.monthly) <- NULL

    list(downloads.total=downloads.total, downloads.monthly=downloads.monthly,
        date1=date1, date2=date2)
}

## df <- findPackage(c("robust", "outlier"), limit.results=600, return.df=TRUE, display="console", results.longdesc=FALSE)
df <- findPackage(c("robust"), limit.results=600, return.df=TRUE, display="console", results.longdesc=FALSE)

df <- df[,-4]           # remove the long names
head(df)
dim(df)

p1 <- df[,2]

## This are the packages that we already know
p2 <-  c("cluster", "mvoutlier", "robustbase", "rrcov", "RobStatTM", "tclust", 
    "robust", "pcaPP", "robustHD", "rrcovHD", "rrcov3way", "robCompositions", 
    "pense", "cellWise", "classmap", "rospca", "ltsspca", "mrfDepth",
    "complmrob", "DetMCD", "DetR", "enetLTS", "GSE", "HDRFA", "ImputeRobust", "otrimle", 
    "pyinit", "riv", "RMBC", "robcor", "robeth", "RobRegression", "robustfa", 
    "robustreg", "robustsur", "Routliers", "rpls", "RSC", "ICS", "ShapleyOutlier", 
    "ICSOutlier", "robustX", "crmReg", "robustcov", "robregcc", "fsdaR", "fit.models", 
    "VIM", "robmed", "RobKF", "robStepSplitReg", "ivx", "chemometrics")

## The following package names does not appear in the search for "robust" and "outlier"
p2[which(!(p2 %in% p1))] 

p <- unique(c(p1, p2))
length(p) 
## 606 

## We will ignore the following packages:

##  WRS2
##  ivreg: Instrumental-Variables Regression by '2SLS', '2SM', or '2SMM', with Diagnostics
##  rsae: Robust Small Area Estimation

##  amap: Another Multidimensional Analysis Package
##  bst: Gradient Boosting
##  Gmedian: Geometric Median, k-Medians Clustering and Robust Median PCA
##  DRDID: Doubly Robust Difference-in-Differences Estimators
##  MixtureMissing: Robust and Flexible Model-Based Clustering for Data Sets with Missing Values at Random
##  ChemoSpec: Exploratory Chemometrics for Spectroscopy
##  sparsepca: Sparse Principal Component Analysis (SPCA)
##  mpath: Regularized Linear Models
##  mblm: Median-Based Linear Models
##  MicrobiomeStat: Statistical Methods for Microbiome Compositional Data

##  collapse: Advanced and Fast Data Transformation: "... advanced data transformation and statistical computing in R ..., robust and programmer friendly"
##  sfsmisc: "... For robustness, have a robust F test and robust range()"
##  prophet: forecasting time series data: "... Prophet is robust to missing data ..."
##  brms: Bayesian Regression Models using 'Stan'
##  rsvd: Randomized Singular Value Decomposition:  randomized robust principal component analysis (rrpca)
##  statsExpressions: Tidy Dataframes and Expressions with Statistical Details: Tidy Dataframes and Expressions with Statistical Details: "... robust, and Bayesian t-test"
##  ggspatplot: "... robust, and Bayesian versions of t-test/ANOVA"
##  golem: golem: A Framework for Robust Shiny Applications
##  varhandle: Functions for Robust Variable Handling
##  sas7bdat
##  MOCHA: Modeling for Single-Cell Open Chromatin Analysis: 22::: robustly models repeated measures in single cell data"
##  lawstat: Tools for Biostatistics, Public Policy, and Law: "... new robust tests of symmetry, ...robust QQ plot, robust trend tests for variances, etc"
##  mmrm: Mixed Models for Repeated Measures: "... nables fast and robust model fitting"
##  lares: Analytics & Machine Learning Sidekick: "...quick and robust results, without the need of repetitive coding or extensive R programming skills."
##  caretEnsemble: Ensembles of Caret Models
##  TwoWayFEWeights: Estimation of the Weights Attached to the Two-Way Fixed Effects Regressions
##  IDPmisc: Utilities of Institute of Data Analyses and Process Design (www.zhaw.ch/idp)
##  ECOSolveR: Embedded Conic Solver in R
##  trend: Non-Parametric Trend Tests and Change-Point Detection: "... two sample Robust Rank-Order Distributional Test"
##  robustlmm: Robust Linear Mixed Effects Models
##  rdrobust: Robust Data-Driven Statistical Inference in Regression-Discontinuity Designs
##  robumeta: Robust Variance Meta-Regression
##  EpiModel: Mathematical Modeling of Infectious Disease Dynamics
##  sensitivity: Global Sensitivity Analysis of Model Outputs and Importance Measures
##  ramcmc: Robust Adaptive Metropolis Algorithm
##  pammtools: Piece-Wise Exponential Additive Mixed Modeling Tools for Survival Analysis
##  mr.raps: Two Sample Mendelian Randomization using Robust Adjusted Profile Score
##  NNS: Nonlinear Nonparametric Statistics
##  GWmodel: Geographically-Weighted Models
##  simmer: Discrete-Event Simulation for R
##  cutpointr: Determine and Evaluate Optimal Cutpoints in Binary Classification Tasks
##  datanugget: Create, and Refine Data Nuggets
##  rnmamod: Bayesian Network Meta-Analysis with Missing Participants
##  reproducible: Enhance Reproducibility of R Code
##  rminer: Data Mining Classification and Regression Methods
##  isotree: Isolation-Based Outlier Detection
##  ExPanDaR: Explore Your Data Interactively
##  metaplus: Robust Meta-Analysis and Meta-Regression: "...Robust methods are based on alternative distributions for the random effect, either the t-distribution..."
##  blockCV: Spatial and Environmental Blocking for K-Fold and LOO Cross-Validation
##  soma: General-Purpose Optimisation with the Self-Organising Migrating Algorithm
##  NetSwan: Network Strengths and Weaknesses Analysis
##  carat: Covariate-Adaptive Randomization for Clinical Trials
##  PreciseSums: Accurate Floating Point Sums and Products
##  wrProteo: Proteomics Data Analysis Functions
##  GUniFrac: Generalized UniFrac Distances, Distance-Based Multivariate Methods and Feature-Based Univariate Methods for Microbiome Data Analysis
##  arkdb: Archive and Unarchive Databases Using Flat Files
##  hdm: High-Dimensional Metrics
##  bootES: Bootstrap Confidence Intervals on Effect Sizes
##  MKdescr: Descriptive Statistics
##  RoBMA: Robust Bayesian Meta-Analyses
##  abess: Fast Best Subset Selection: "...and robust principal component analysis" - uses cov.rob from MASS
##  cSEM: Composite-Based Structural Equation Modeling
##  ANN2: Artificial Neural Networks for Anomaly Detection

##  estimatr
##  lfe: Linear Group Fixed Effects
##  clusterSEs: Calculate Cluster-Robust p-Values and Confidence Intervals
##  alpaca: Fit GLM's with High-Dimensional k-Way Fixed Effects

c1 <- c("jsonlite", "rprojroot", "RCurl", "collapse", "sfsmisc", "prophet", "brms", "rsvd", "statsExpressions", "ggstatsplot", "golem", "varhandle",
    "fit.models", "sas7bdat", "MOCHA", "lawstat", "mmrm", "lares", "caretEnsemble", "TwoWayFEWeights", "IDPmisc", "ECOSolveR", "trend", "robustlmm",
    "DRDID", "rdrobust", "robumeta", "EpiModel", "sensitivity", "ramcmc", "pammtools", "mr.raps", "NNS", "GWmodel", "simmer", "cutpointr", "datanugget",
    "rnmamod", "reproducible", "rminer", "isotree", "ExPanDaR", "metaplus", "blockCV", "soma", "NetSwan", "carat", "PreciseSums", "wrProteo", "GUniFrac",
    "arkdb", "hdm", "bootES", "MKdescr", "RoBMA", "abes", "cSEM", "ANN2", "Gmedian", "amap", "bst")
    
c2 <- c("sandwich", "plm", "estimatr", "lfe", "clubSandwich", "clusterSEs", "alpaca")

p <- p[!(p %in% c1)]
p <- p[!(p %in% c2)]

## Look these packages ...

##  LearnPCA: Functions, Data Sets and Vignettes to Aid in Learning Principal Components Analysis (PCA)
##  gclus: Clustering Graphics
##  pcadapt: Fast Principal Component Analysis for Outlier Detection
##  robfilter: Robust Time Series Filters
##  kmodR: K-Means with Simultaneous Outlier Detection
##  otrimle: Robust Model-Based Clustering
##  ICSClust: Tandem Clustering with Invariant Coordinate Selection
##  tsoutliers: Detection of Outliers in Time Series
##  dlookr: Tools for Data Diagnosis, Exploration, Transformation
##  robmixglm: Robust Generalized Linear Models (GLM) using Mixtures
##  RobLoxBioC: Infinitesimally Robust Estimators for Preprocessing -Omics Data
##  CerioliOutlierDetection: Outlier Detection Using the Iterated RMCD Method of Cerioli (2010)
##  modi: Multivariate Outlier Detection and Imputation for Incomplete Survey Data
##  robmed: (Robust) Mediation Analysis
##  drgee: Doubly Robust Generalized Estimating Equations
##  rpca: RobustPCA: Decompose a Matrix into Low-Rank and Sparse Components
##  RobPer: Robust Periodogram and Periodicity Detection Methods

## Set a range of dates and names of the packages to track
date1 <- "2012-10-01"
date2 <- Sys.Date()                                 # Today
date2 <- as.Date(format(date2, "%Y-%m-01"))         # Start of this month
date2 <- date2 - 1                                  # End of last month
date2 <- "2024-04-30"                                 # Today
    
d_robust <- getDownloads(p, date1, date2)

##------------------------------------------------------------------------------
df <- findPackage(c("outlier"), return.df=TRUE, display="console", results.longdesc=FALSE)
df <- df[,-4]           # remove the long names
head(df)

p <- df[,2]
length(p) 

d_outlier <- getDownloads(p, date1, date2)

save(d_robust, d_outlier, file=here::here("Data", "downloads.rda"))

## Do the plots now ===========================================================

p1 <- ggplot(data=d_robust$downloads.total[1:15,], aes(x=reorder(Package, V1), y=V1/1000000)) +
     geom_bar(stat="identity", fill="#377EB8") +
     coord_flip() +
     ylab("Total downloads (M)") +
     xlab(element_blank()) +
     theme_light() +
     theme(axis.text=element_text(size=12),
     axis.title=element_text(size=14,face="bold"))

p1

p2 <- ggplot(data=d_robust$downloads.monthly[1:15,], aes(x=reorder(Package, V1), y=V1/1000)) +
     geom_bar(stat="identity", fill="#377EB8") +
     coord_flip() +
     ylab("Monthly downloads (K/month)") +
     xlab(element_blank()) +
     theme_light() +
     theme(axis.text=element_text(size=12),
     axis.title=element_text(size=14,face="bold"))

p2

## Plot the left- and right-hand panels as separate files
if(FALSE) {
    cairo_pdf(filename=here::here("output", "downloads-total.pdf"), width=6, height=6)
    p1
    dev.off()
       
    cairo_pdf(filename=here::here("output", "downloads-monthly.pdf"), width=6, height=6)
    p2
    dev.off()
}
  
cairo_pdf(filename=here::here("output", "downloads.pdf"), width=12, height=6)
grid.arrange(p1, p2, nrow = 1)
dev.off()


## Figure 3....................................................................
top5 <- d_robust$downloads.monthly[1:5, 1]
P1 <- top5      # c("robustbase", "cluster", "pcaPP", "rrcov", "robust")
D1 <- cran_downloads(from = date1, to = date2, packages = P1)
D1 <- setDT(D1)
setnames(D1, "package", "Package") 

## Aggregate by month
d1 <- D1[, .(Count = sum(count)), by = .(Year = year(date), 
                                       Month = months(date),
                                       Package)]

## Create the date variable for plotting
x1 <- d1[, ':='(
    Date = as.Date(paste0(Year, "-", Month, "-01"), format = "%Y-%B-%d")
)]

library(RColorBrewer)
library(scales)
mycolors <- brewer.pal(8, "Set1")[c(1:2, 4:5, 7)]
##mycolors <- brewer.pal(8, "Dark2")
show_col(mycolors)

p3 <- ggplot(x1, aes(x = Date, y = Count, group = Package, colour = Package)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values=mycolors) +
#    scale_color_viridis(discrete=TRUE, option="viridis") +
    xlab("") + ylab("Total monthly downloads") + 
    ## ggtitle(paste0("Downloads of the R packages from CRAN (from ", date1, " to ", date2, ")")) +
#    theme_ipsum_pub() + 
    theme_light() +
    theme(plot.title = element_text(size = 12),
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"),
    legend.text = element_text(size=12))

    
p3

cairo_pdf(filename=here::here("output", "downloads-monthly-period.pdf"), width=12, height=6)
p3
dev.off()

