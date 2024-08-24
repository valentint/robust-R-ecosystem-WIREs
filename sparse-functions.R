library(rospca)
library(ltsspca)

if(FALSE) {
    pca0 <- PcaClassic(bus2, k=4, kmax=p)                   # Classic
    dd1 <- xxx.distances(bus2, pca0)
    xxx.ddplot(dd1, main="Classical PCA")
    
    pca1 <- PcaGrid(bus2, k=4, kmax=p)                      # PcaGrid
    dd1 <- xxx.distances(bus2, pca1)
    xxx.ddplot(dd1, main="PcaGrid")
    
    pca2 <- PcaHubert(bus2, k=4, kmax=p)                    # ROBPCA
    dd1 <- xxx.distances(bus2, pca2)
    xxx.ddplot(dd1, main="ROBPCA")
    
    pca3x <- ltsspca(x=bus2, kmax=4, alpha=0.5)             # ltsspca
    pca3 <- ltsspcaRw(x=bus2, obj=pca3x, k=4, alpha=0.5)
    dd1 <- xxx.distances(bus2, pca3, objclass="LTSSPCA")
    xxx.ddplot(dd1, main="LTSSPCA")
    xx <- mydiagPlot(bus2, pca3, k=4)                       # should return invisible!
    
    pca4 <- rospca(bus2, lambda=0.1, k=4)                   # ROSPCA
    dd1 <- xxx.distances(bus2, pca4, objclass="ROSPCA")
    xxx.ddplot(dd1, main="ROSPCA")
    diagPlot(pca4)
}

if(FALSE) {

    tic()
    xx <- IS.rospca(x, k=4, lambda.n=101)
    toc()
    ##  elapsed time is 30.870000 seconds 
    
    xx$tab
    plot(xx$tab$lambda, xx$tab$IS, type="l")
    xx$tab$lambda[which.max(xx$tab$IS)]
    ##  [1] 1.55
    
    tic()
    oBIC <- opt.BIC(x, k.max=4, method="qn", n.lambda=101)
    toc()
    ##  elapsed time is 334.890000 seconds
    
    xx=IS.BIC(x, oBIC, k=4, method="qn")
    xx$tab
    plot(xx$tab$lambda, xx$tab$ISx, type="l")
    xx$tab$lambda[which.max(xx$tab$ISx)]
    ##   5.461181



}

xxx.screeplot <- function(pcs, k, type = c("barplot", "lines"), main = deparse1(substitute(pcs)), ...)
{
    type <- match.arg(type)
    k <- if(missing(k)) min(10, length(pcs))
         else           min(k, length(pcs))
    names(pcs) <- NULL
    
    xp <- seq_len(k)

    dev.hold()
    on.exit(dev.flush())
    if(type == "barplot")
        barplot(pcs[xp], names.arg = names(pcs[xp]), main = main,
            ylab = "Variances", ...)
    else {
        plot(xp, pcs[xp], type = "b", axes = FALSE, main = main,
            xlab = "", ylab = "Variance", ...)
        axis(2)
        axis(1, at = xp, labels = names(pcs[xp]))
    }
    invisible()
}

xxx.distances <- function(data, obj, objclass, crit=0.975) {

    if(missing(objclass) && inherits(obj, "list"))
        stop(paste0("Unknown type of object: ", objclass, "!")) 
        
    if(!inherits(obj, "list"))
        objclass <- class(obj)
    
    r <- rankMM(data)
    loadings <- if(inherits(obj, "Pca")) getLoadings(obj) else obj$loadings
    eigenvalues <- if(inherits(obj, "Pca")) getEigenvalues(obj) else obj$eigenvalues
    scores <- if(inherits(obj, "Pca")) getScores(obj) else obj$scores
    center <- if(inherits(obj, "Pca")) getCenter(obj) else if(objclass == "LTSSPCA") obj$mu else obj$center  
    k <- length(eigenvalues)
    
    ## compute the score distances and the corresponding cutoff value
    n <- nrow(data)
    smat <- diag(eigenvalues, ncol=ncol(scores))

    ## VT::02.06.2010: it can happen that the rank of the matrix
    ##  is nk=ncol(scores), but the rank of the diagonal matrix of
    ##  eigenvalues is lower: for example if the last singular
    ##  value was 1E-7, the last eigenvalue will be sv^2=1E-14
    ##
    nk <- min(ncol(scores), rankMM(smat))
    if(nk < ncol(scores))
        warning(paste("Too small eigenvalue(s): ", eigenvalues[ncol(scores)], "- the diagonal matrix of the eigenvalues cannot be inverted!"))

    sd <- sqrt(mahalanobis(as.matrix(scores[,1:nk]), rep(0, nk), diag(eigenvalues[1:nk], ncol=nk)))
    cutoff.sd <- sqrt(qchisq(crit, k))

    ## Compute the orthogonal distances and the corresponding cutoff value
    ##  For each point this is the norm of the difference between the
    ##  centered data and the back-transformed scores

    ##  VT::21.06.2016 - the data we get here is the original data - neither centered nor scaled.
    ##      - center and scale the data
    x <- scale(data, center, FALSE)
    od <- apply(x - scores %*% t(loadings), 1, vecnorm)

    if(is.list(dimnames(scores))) {
        names(od) <- dimnames(scores)[[1]]
    }

    ## The orthogonal distances make sense only if the number of PCs is less than
    ##  the rank of the data matrix - otherwise set it to 0
    cutoff.od <- 0
    if(k != r) {
        ## the method used for computing the cutoff depends on (a) classic/robust and (b) skew
        mx <- if(inherits(obj, "PcaClassic")) "classic" else "medmad"
        cutoff.od <- rrcov:::.crit.od(od, crit=crit, method=mx)
    }

    ## flag the observations with 1/0 if the distances are less or equal the
    ##  corresponding  cutoff values
    flag.od <- rep(0, length(od))
    flag <- flag.sd <- sd <= cutoff.sd
    if(cutoff.od > 0) {
        flag.od <- od <= cutoff.od
        flag <- flag.od & flag.sd
    }
    
    return (list(od=od, cutoff.od=cutoff.od, sd=sd, cutoff.sd=cutoff.sd, flag.od=flag.od, flag.sd=flag.sd, flag=flag))
}


xxx.ddplot <- function(obj, main, xlim, ylim, off=0.02, ...) {

    ## Scale the distances
    od <- obj$od/obj$cutoff.od
    sd <- obj$sd/obj$cutoff.sd
    cutoff.od <- 1
    cutoff.sd <- 1
    
    if(all(od <= 1.E-06))
        warning("PCA diagnostic plot is not defined")
    else
    {
        if(missing(xlim))
            xlim <- c(0, max(max(sd), cutoff.sd))
        if(missing(ylim))
            ylim <- c(0, max(max(od), cutoff.od))

        plot(sd, od, xlab="Scaled score distance",
                             ylab="Scaled orthogonal distance",
                             main=main,
                             xlim=xlim, ylim=ylim, type="p", ...)
        abline(v=cutoff.sd)
        abline(h=cutoff.od)
        
        ##label.dd(sd, od, id.n.sd, id.n.od, off=off)
    }
    invisible(obj)
}

##  Calculate the Percent Explained Variance (PEV)
##
##  v: a matrix of loadings or a Pca object
##  ev: eigenvalues (if v is not a Pca object)
##  
pev <- function(x, v, ev, k=ncol(v), method="qn") {
    
    if(is(v, "Pca")) {
        k <- v$k
        ev <- v$eigenvalues[1:k]
        v <- v$loadings[,1:k]
    } else if(is(v, "princomp")) {
        k <- v$k
        ev <- v$sdev[1:k]^2
        v <- v$loadings[,1:k]  
    }
    
    ##  Centering x does not change anything
    ##  xmed <- apply(x, 2, median)
    ##  xc <- sweep(x, 2, xmed, "-", check.margin = FALSE)
    
    fmeth <- if(method=="sd") sd else qn
    scores <- x %*% v[,1:k]
    ##  pev <- sum(apply(scores[,1:k], 2, fmeth)) ^2 / sum(apply(xc, 2, fmeth)) ^2
    ##  pev <- sum(apply(scores[,1:k], 2, fmeth) ^2) / sum(apply(x, 2, fmeth))
    
    ##  cat("\n", sum(ev), sum(apply(x, 2, fmeth)^2), "\n")
    
    pev <- sum(ev) / sum(apply(x, 2, fmeth)^2)
    list(EV=sum(ev), PEV=pev)
}

## Calculate the index of sparseness using the results of opt.BIC() funtion for SRPCA(GRID)
##  The method is taken from the BIC object.
##  IS = PEV_sparse * PEV_full * PS
##  where PEV is the percent explained variance calculated as sum(VAR(scores_i))/sum(VAR(X_i)), with 
##  VAR() being the variance measure (sd or qn depending on the method) and 
##  PS is the percent sparseness calculated as df(lambda)/(p*k) with the degrees 
##  of freedom df(lambda) the number of nonzero loadings
##
##  ISx = PEVx_sparse * PEVx_full * PS
##  This is the same as IS, but PEV is calculated as PEVx = 1 - norm(scores * t(loadings) - X)^2 / norm(X)^2
##
IS.BIC <- function(x, BIC, zero_tol=1e-5) {
    p <- ncol(x)
    k <- length(BIC$opt$pc)
    method <- BIC$opt$pc[[k]]$args$method

    stopifnot(k == BIC$opt$pc[k]$k)
    nlambda <- length(BIC$opt$PCs)
    
    lambda <- ev <- pev <- pevx <- pevy <- ps <- vector(mod="numeric", length=length(nlambda))

    fmeth <- if(method=="sd") sd else qn

    for(i in 1:nlambda) {
        spc <- BIC$opt$PCs[[i]]
        lambda[i] <- spc$lambda[k]

        ## 1. Using the eigenvalues, as in pcaPP:::.sumVarP()
        ev[i] <- pev(x, v=spc, method=method)$EV
        pev[i] <- pev(x, v=spc, method=method)$PEV
        
        ## 2. Using the scores [1:k]
        scores <- x %*% spc$loadings[,1:k]
        pevy[i] <- sum(apply(scores[,1:k], 2, fmeth)) / sum(apply(x, 2, fmeth))
        
        ##  3. PEV as defined in Guerra-Urzola et al. (2021): percentage of explained variance
        pevx[i] <- 1 - norm(x %*% spc$loadings[,1:k] %*% t(spc$loadings[,1:k]) - x, type="F") ^ 2 / norm(x, type="F") ^ 2
        
        ## Proportion of sparsity: number of zero-loadings devided by number of all loadings (p*k)
        ps[i] <- length(which(abs(spc$loadings[,1:k]) <= zero_tol))/(p * k)
        
        ## cat("\n", i, round(lambda[i], 2), pev[i], pevy[i], pevx[i], ps[i], "\n")
    }
    
    is <- pev * rep(pev[1], length(pev)) * ps
    isx <- pevx * rep(pevx[1], length(pevx)) * ps
    isy <- pevy * rep(pevy[1], length(pevy)) * ps

    tab <- cbind.data.frame(lambda=lambda, EV=ev, PEV=100*pev, PEVY=pevy, PEVX=pevx, PS=ps, IS=is, ISy=isy, ISx=isx)
    return(list(tab=tab))
}

IS.rospca <- function(x, k, alpha=0.5, lambda.max=2.5, lambda.n=10, zero_tol=1e-5) {
    
    method <- "qn"
    p <- ncol(x)
    lambda <- seq(0, lambda.max, length.out=lambda.n)
    PCs <- list()
    ev <- pev <- pevx <- pevy <- ps <- vector(mod="numeric", length=lambda.n)
    
    fmeth <- if(method=="sd") sd else qn

    for(i in 1:lambda.n) {
        spc <- rospca(x, k=k, alpha=alpha, lambda=lambda[i], stand=FALSE)

        
        ## 1. Using the eigenvalues, as in pcaPP:::.sumVarP()
        ev[i] <- pev(x, v=spc$loadings, ev=spc$eigenvalues, method=method)$EV
        pev[i] <- pev(x, v=spc$loadings, ev=spc$eigenvalues, method=method)$PEV

        ## 2. Using the scores [1:k]
        scores <- x %*% spc$loadings[,1:k]
        pevy[i] <- sum(apply(scores[,1:k], 2, fmeth)) / sum(apply(x, 2, fmeth))
        
        ##  3. PEV as defined in Guerra-Urzola et al. (2021): percentage of explained variance
        pevx[i] <- 1 - norm(x %*% spc$loadings[,1:k] %*% t(spc$loadings[,1:k]) - x, type="F") ^ 2 / norm(x, type="F") ^ 2
        
        ## Proportion of sparsity: number of zero-loadings devided by number of all loadings (p*k)
        ps[i] <- length(which(abs(spc$loadings[,1:k]) <= zero_tol))/(p * k)
        
        ##  cat("\n", i, round(lambda[i], 2), pev[i], pevx[i], ps[i], "\n")
    }
    
    is <- pev * rep(pev[1], length(pev)) * ps
    isx <- pevx * rep(pevx[1], length(pevx)) * ps
    isy <- pevy * rep(pevy[1], length(pevy)) * ps

    tab <- cbind.data.frame(lambda=lambda, EV=ev, PEV=100*pev, PEVY=pevy, PEVX=pevx, PS=ps, IS=is, ISy=isy, ISx=isx)
    return(list(tab=tab))
}

plot.IS <- function(x, f.y=c("IS", "is", "var","pvar"), f.x=c("lambda", "pl0")){
    X <- xx$tab$lambda
    Y <- xx$tab$IS
    xlab <- bquote(paste(lambda))
    ylab <- "IS"
    
    ind <- which.max(xx$tab$IS)
    x.opt <- lambda.opt <- xx$tab$lambda[ind]
    ev.opt <- xx$tab$EV[ind]
    pev.opt <- xx$tab$PEV[ind]
    ps.opt <- xx$tab$PS[ind]

    f.y <- match.arg(f.y)
    f.x <- match.arg(f.x)
    
    if(f.y =="var")  {
        Y <- x$tab$EV
        ylab <- "Explained variance"
    } else if(f.y == "pvar")  {
        Y <- x$tab$PEV
        ylab <- "Percent explained variance"
    }
    
    if(f.x =="pl0")  {
        ord <- order(x$tab$PS)
        X <- 100*x$tab[ord,]$PS
        Y <- Y[ord]
        xlab <- bquote(paste(L[0], "S"))
        x.opt <- 100*ps.opt
    } 
    plot(X, Y, type="l", xlab=xlab, ylab=ylab)
    abline(v=x.opt, lty="dotted")

    txtLambda <- bquote(paste(lambda[opt], ": " ,.(round(lambda.opt,4))))
    txtPEV <- bquote(paste(PEV[opt], ": " ,.(round(pev.opt,2)), "%"))
    txtPS <- bquote(paste(L[0], "S: " ,  .(round(100*ps.opt)), "%"))
    txt <- paste0(txtLambda, "; ", txtPEV, ";", txtPS)
    txt <- bquote(paste (.(txtLambda), "; ", .(txtPEV), "; ", .(txtPS)))
    mtext(txt, line = 0.25, cex = 0.8)

    return(invisible(x))
}

## Return number of non-zero loadings on each PC, number of excluded variables EXCL
##  (i.e. variables which have zero loadings on all PCs) and number of variables 
##  in the PC space (PS=p - EXCL).
dfs <- function(loadings, zerotol=1e-5){
    k <- ncol(loadings)
    p <- nrow(loadings)
    v1 <- apply(loadings, 2, function(x){length(which(abs(x) >= zerotol))})

    ## Number of excluded variables
    cc <- length(which(rowSums(round(loadings, 5)) == 0))
    v1 <- c(v1, PS=p-cc, EXCL=cc)
    names(v1) <- c(paste0("PC", 1:k), "PS", "EXCL")
    v1
}

