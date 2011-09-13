## coeftab: new generic method
##  should retrieve a coefficient table from as many model types
##  as possible
##
## return value should always be a data frame (matrix?)
## first column should always be coefficient estimates
## what about other columns?
##   sd
##   lower and upper conf int (quadratic approx? quantiles? HPD? profile?)
##   p-value?
##   is it OK to have different defaults for different model types?
##   'type' can be one OR MORE of 'fixef', 'ranef', 'vcov'
##    (these are not distinguishable for some models?)
##
## ptype: "fixef", "ranef", "vcov"
## ctype: "profile", "quantile", "HPDinterval", "quad"
## sd: TRUE or FALSE
## clevel: numeric vector (values between 0 and 1)
## cmult: numeric vector (values > 1)
## cdf: integer

extend_tab <- function(tab,vnames) {
  newtab <- matrix(NA,nrow=length(vnames),ncol=ncol(tab),
                   dimnames=list(vnames,colnames(tab)))
  newtab[rownames(tab),] <- as.matrix(tab)
  if (inherits(tab,"data.frame")) newtab <- as.data.frame(newtab) ## kluge
  class(newtab) <- class(tab) ## preserve class of coeftabs, etc.
  newtab
}

coeftab <- function(object, ...) UseMethod("coeftab",object)

## same as emdbook::lump.mcmc.list
as.mcmc.mcmc.list <- function (x)  {
    x2 <- do.call("rbind", x)
    mcpars <- sapply(x, attr, "mcpar")
    class(x2) <- "mcmc"
    if (var(mcpars[1, ]) > 0 || var(mcpars[3, ]) > 0) 
        stop("can't combine chains with unequal start/thin")
    attr(x2, "mcpar") <- c(mcpars[1, 1], sum(mcpars[2, ]), mcpars[3, 
        1])
    x2
  }

coeftab.mcmc.list <- function(object,...) {
  coeftab.mcmc(as.mcmc.mcmc.list(object),...)
}

### MCMC-like objects ...
coeftab.mcmc <- function(object,ptype,
                         clevel=c(0.5,0.95),
                         ctype="quantile",
                         sd=FALSE,...) {
  
  if (!missing(ptype)) warning("ptype parameter ignored for 'mcmc' object")
  if (ctype%in%c("quad","profile"))
    stop("ptype=='quad' or 'profile' doesn't make sense for 'mcmc' object")
  ## FIXME: NOT sure I need this -- test
  ## if (!is.matrix(object)) object <- matrix(object,ncol=1)
  ctab <- data.frame(Estimate=apply(object,2,mean,na.rm=TRUE))
  if (sd) ctab <- cbind(ctab,`Std. Error`=apply(object,2,sd,na.rm=TRUE))
  if (!(length(clevel)==1 && is.na(clevel))) {
    qvals <- c(matrix(c((1-clevel)/2,0.5+clevel/2),ncol=2,byrow=TRUE))
    if (ctype=="quantile") {
      tmptab <- t(apply(object,2,quantile,sort(qvals),
                        na.rm=TRUE))
    } else if(ctype=="HPDinterval") {
      require(coda)
      tmptab <- do.call(cbind,
                        lapply(as.list(clevel),
                               HPDinterval,obj=object))
      tmptab <- tmptab[,order(qvals),drop=FALSE]
      ## FIXME: do something about restricting digits?
      colnames(tmptab) <- paste(sort(qvals)*100,"%",sep="")
      tmptab
    } else stop("allowed ctype (interval type) values for MCMC are 'quantile' or 'HPDinterval'")
    ctab <- cbind(ctab,tmptab)
  }
  ctab <- ctab[!rownames(ctab)=="deviance",]
  class(ctab) <- c("coeftab","data.frame")
  ctab
}

coeftab.MCMCglmm <- function(object,ptype="fixef",...) {
  clist <- c(fixef="Sol",vcov="VCV",ranef="Liab")
  comp <- clist[ptype]
  ctab <- do.call("rbind",
          lapply(object[comp],coeftab,...))
  class(ctab) <- c("coeftab","data.frame")
  ctab
}

coeftab.rjags <- function(object,...) {
  coeftab(object$BUGSoutput,...)
}

coeftab.bugs <- function(object,...) {
  ## FIXME: for efficiency, could use
  ## info from object$summary
  ##    -- if (ptype=="quantile") && all(clevel %in% c(0.5,0.95))
  ## but for now just recompute ...
  m <- if (object$n.chains>1) as.mcmc.list(object) else as.mcmc(object)
  coeftab(m,...)
}
  
## ADD: ADMB output containing mcmc info ...

###
coeftab.glmmML <- function(object,ctype="quad",...) {
  coeftab0(cbind(coef(object),object$coef.sd),...)
  
}

coeftab.lme <- function(object,ptype="fixef",
                        ctype="quad",...) {
  clist <- list(ftab=NULL, rtab=NULL, vtab=NULL)
  ## coeftab.default?
  if ("fixef" %in% ptype)
    clist$ftab <- coeftab0(summary(object)$tTable[,1:2],...)
  if ("ranef" %in% ptype) {
    ## FIXME: allow parameters to be passed through to ranef.lme?
    rr <- unlist(ranef(object))
    aa <- rep(NA,length(rr))
    clist$rtab <- coeftab0(rr,aa)
  }
  if  ("vcov" %in% ptype) {
    vv <- nlme::VarCorr(object)
    if (ncol(vv)>2) stop("can't yet handle multiple variance-covariance terms (correlation)")
    clist$vtab <- coeftab0(cbind(as.numeric(vv[,"StdDev"]),NA),...)
  }
  do.call(rbind,unname(clist))
}

coeftab.mer <- function(object,ptype="fixef",
                        ctype="quad",...) {
  clist <- list(ftab=NULL, rtab=NULL, vtab=NULL)
  if ("fixef" %in% ptype) clist$ftab <- coeftab.default(object,...) ## fixef
  if ("ranef" %in% ptype) {
    rr <- unlist(ranef(object,postVar=TRUE))
    aa <- unlist(lapply(ranef(object,postVar=TRUE),attr,"postVar"))
    clist$rtab <- coeftab0(cbind(unlist(rr),sqrt(aa)),...)
  }
  if ("vcov" %in% ptype) {
    ## FIXME: deal with correlations/test
    vv <- VarCorr(object)
    sdvec <- sapply(vv,attr,"stddev")
    if (!is.na(sdres <-attr(vv,"sc"))) {
      sdvec <- c(sdvec,resid=sdres)
    }
    names(sdvec) <- paste("sd",names(sdvec),sep=".")
    fvec <- sdvec
    mv <- (sapply(vv,nrow)>1)
    if (any(mv)) {
      warning("correlation handling for mer is a stub -- use with caution/test")
      corvec <- unlist(sapply(vv[mv],
                              function(x) {
                                cmat <- attr(x,"correlation")
                                cmat[lower.tri(cmat)]
                              }))
      names(corvec) <- paste("cor",names(corvec),sep=".")
      fvec <- c(sdvec,corvec)
    }
    clist$vtab <- coeftab0(cbind(fvec,NA),...)
  }
  do.call(rbind,unname(clist))
}

coeftab.glmm.admb <- function(object,ptype="fixef",...) {
  clist <- list(ftab=NULL, rtab=NULL, vtab=NULL)
  if ("fixef" %in% ptype) clist$ftab <- coeftab0(cbind(object$b,object$stdbeta))
  if ("ranef" %in% ptype) clist$rtab <- coeftab0(cbind(object$U,object$sd_U))
  if ("vcov" %in% ptype) clist$vtab <- coeftab0(cbind(object$S,object$sd_S))
  do.call(rbind,unname(clist))
}

## should work for: lm, lmer?
coeftab.default <- function(object,ctype="quad",...) {
  cc <- coeftab0(coef(summary(object))[,1:2],...)
  ## FIXME: deal with types that don't have a working model.matrix()
  attr(cc,"assign") <- try(attr(model.matrix(object),"assign"),silent=TRUE)
  cc
}

coeftab.admb <- function(object,...) {
  mvals <- object$coefficients
  if (!is.null(object$mcmc)) {
    ctab <- coeftab(as.mcmc(object$mcmc),...)
    ctab[,1] <- mvals
  } else {
    ctab <- coeftab0(cbind(mvals,object$se),...)
  }
  ctab
}

## generic computation -- takes data frame with columns mean and std. dev.,
##  adds conf intervals, p values
coeftab0 <- function(object,
                     ctype="quad",
                     sd=TRUE,
                     cmult=1.96,
                     clevel=c(0.5,0.95),
                     df,
                     p.val=FALSE,
                     quantiles=TRUE,
                     ...) {
  if (!missing(cmult) && !missing(clevel))
    stop("must specify at most one of 'cmult' and 'clevel'")
  if (length(clevel)>1 && !missing(df) && length(df)>1)
    stop("don't know what to do",
         "with multiple values of both 'clevel' and 'df'")
  if (p.val) {
    if (!missing(cmult))
      stop("specify clevel, not cmult, if you want p-values")
    if (!missing(df) && length(df)>1)
      stop("can only compute p-values for a single df value")
  }
  mval <- object[,1]
  sdval <- object[,2]
  if (missing(cmult)) { ## compute CIs based on 'clevel'
    if (missing(df)) {
      ## no df specified: Z-score, etc.
      cmult <- qnorm(0.5+clevel/2)
      if (p.val) {
        pvals <- 2*pnorm(abs(mval/sdval),lower.tail=FALSE)
      }
    } else {  ## not missing df
      ## t-score etc.
      if (p.val) {
        pvals <- 2*pt(abs(mval/sdval),df=df,lower.tail=FALSE)
      }
      cmult <- qt(0.5+clevel/2,df=df)
    }
    ## FIXME: turn off computation when quantiles is FALSE?
    qvals <- c(matrix(c((1-clevel)/2,0.5+clevel/2),ncol=2,byrow=TRUE))
    qnames <- paste(qvals*100,"%",sep="")[order(qvals)]
  } else {
      qnames <- paste("(",c(sort(-cmult),paste("+",sort(cmult),sep=""))," sd)",
                      sep="")
  }
  cmult <- sort(c(-cmult,cmult))
  tmptab <- sweep(outer(sdval,cmult),1,mval,"+")
  colnames(tmptab) <- qnames
  ctab <- data.frame(Estimate=mval)
  if (sd) ctab <- cbind(ctab,`Std. Error`=sdval)
  if (quantiles) ctab <- cbind(ctab,tmptab)
  if (p.val) {
    ctab <- cbind(ctab,`P(> )`=pvals)
  }
  class(ctab) <- c("coeftab","data.frame")
  ctab
}

print.coeftab <- function(x,...) printCoefmat(x,...)

melt.coeftab <- function(data,...) {
  stop("stub")
}
