## my version of coefplot (from arm package, significantly modified)

setOldClass("polr")
setOldClass("bugs")
setOldClass("glmmML")
setOldClass("lme")
setOldClass("glmmPQL")
setOldClass("MCMCglmm")
setOldClass("mcmc")
setOldClass("admb")
setOldClass("rjags")
setOldClass("glmmadmb")

## FIXME: decide whether it's OK to import from merMod
##   and induce a dependence on lme4a: dummy definition?
## "merMod"
knownclasses <- c("lm","glm","mer","merMod",
                  "bmer","bugs","polr",
                  "rjags","bugs","mcmc","MCMCglmm",
                  "glmmadmb","glmmML")

setGeneric("coefplot2",
              function(object, ...)
               standardGeneric("coefplot2"))

coefplot2.default <- function(coefs, sds, 
                              varnames=NULL, CI=2, 
                              vertical=TRUE,
                              v.axis=TRUE, h.axis=TRUE,
                              top.axis=TRUE,
                              cex.var=0.8,
                              cex.pts=0.9,
                              cex.axis=1,
                              col.pts=1, pch.pts=16,
                              var.las=2,
                              main="Regression estimates",
                              xlab="", ylab="",
                              mar=NULL,
                              plot=TRUE, add=FALSE,
                              offset=0,
                              varname.offset=0,
                              lwd.1=2,lwd.2=1,
                              lower1, upper1, lower2, upper2,
                              xlim=NULL, ylim=NULL,
                              ...)
{
  ## ?? can't decide whether we want to reset margins, or how ...
  ## par() says that saving and resetting ALL non-RO parameters is 'not good practice'
  op <- par(c("mar","mai"))  ## only save parameters we will be messing with
  on.exit(par(op))
  min.mar <- par('mar')

  ## assume for now that CIs are included in coeftab -- otherwise
  ##  we have to redo it (call coeftab0?)
  if (inherits(coefs,"coeftab")) {
    coff <- 0
    n.x <- nrow(coefs)
    if ("Std. Error" %in% colnames(coefs)) coff <- 1
    ctab2 <- coefs[,-seq(1,1+coff),drop=FALSE] #
    if (ncol(ctab2)>3) {  ## 2 sets of confidence intervals
      lower2 <- ctab2[,1]      
      lower1 <- ctab2[,2]
      upper1 <- ctab2[,3]
      upper2 <- ctab2[,4]
      CI <- 2
    } else {
      lower2 <- lower1 <- ctab2[,1]
      upper2 <- upper1 <- ctab2[,2]
      CI <- 1
    }
    coefs <- coefs[,1]
  } else {
    if (is.list(coefs)){
      coefs <- unlist(coefs)
    }
    n.x <- length(coefs)
    if (missing(lower1)) lower1 <- coefs-sds
    if (missing(lower2)) {
      if (CI==1) lower2 <- lower1 else lower2 <- coefs-2*sds
    }
    if (missing(upper1)) upper1 <- coefs+sds
    if (missing(upper2)) {
      if (CI==1) upper2 <- upper1 else upper2 <- coefs+2*sds
    }
  }
  idx <- 1:n.x
  coefs.h <- upper2
  coefs.l <- lower2
  ##
  if (is.null(varnames)) {
    maxchar <- maxwid <- 0
  }
  else{
    maxchar <- max(sapply(varnames, nchar))
    maxwid <- max(strwidth(varnames,units="inches"))
  }
        
  ## add margin to the axis
  k <- 1/n.x   
  if (plot) {
      dlim <- range(c(coefs.l,coefs.h,coefs),na.rm=TRUE)
      
      if (vertical){
        if(!add){
          if (missing(mar)) {
            mai <- par("mai")
            mai[2] <- maxwid+0.5
            par(mai=mai)
            if (top.axis) {
              mar <- par("mar")
              mar[1] <- 2.1
              mar[3] <- 5.1
            }
          }
          par(mar=mar)
          if (missing(xlim)) xlim <- dlim
          plot(c(coefs.l, coefs.h), c(idx+k,idx-k), type="n",
               axes=FALSE, main=main, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...) 
          if (h.axis){                                                  
            if (top.axis) axis(3) else axis(1)
          }
          if (v.axis){
            axis(2, (n.x:1)+varname.offset, varnames[n.x:1], las=var.las, tck=FALSE, 
                 lty=0, cex.axis=cex.var) 
          }
          abline(v=0, lty=2)
        }

        if (add) idx <- idx+offset
        points(coefs, idx, pch=pch.pts, cex=cex.pts, col=col.pts)
        segments (upper2, idx, lower2, idx, lwd=lwd.2, col=col.pts)     
        if (CI==2){
          segments (upper1, idx, lower1, idx, lwd=lwd.1, col=col.pts)
        }
      } # end of if vertical
    else{ # horizontal
      if(!add){
        if (missing(mar)) {
          mar <- par("mar")
          mar[1] <- maxchar/10 + 2.1
        }
        par(mar=mar)
        if (missing(ylim)) ylim <- dlim
        plot(c(idx+k,idx-k), c(coefs.l, coefs.h), type="n", axes=FALSE, 
          main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
             ...)
        if (v.axis){
          axis(2, las=var.las)                                
          #axis(4, las=var.las)
        }
        if (h.axis){
          axis(1, (1:n.x)+varname.offset, varnames[1:n.x], las=var.las, tck=FALSE, 
               lty=0, cex.axis=cex.var) 
        }
        abline(h=0, lty=2)
      }
      if (add) idx <- idx+offset
      points(idx, coefs, pch=pch.pts, cex=cex.pts, col=col.pts)
      segments (idx, upper2, idx, lower2, lwd=lwd.2, col=col.pts)     
      if (CI==2){
        segments (idx, upper1, idx, lower1, lwd=lwd.1, col=col.pts)     
      }
    } ## if horizontal
    } ## if plot
  else { ## no plot (FIXME: why is this here???)
    ## FIXME: carry through margin setting etc.?
    if (vertical){
      if (missing(mar)) {
        mar[2] <- max(min.mar[2], trunc(mar[2] + maxchar/10))  + 0.1
      }
      par(mar=mar)
      plot(c(coefs.l, coefs.h), c(idx+k,idx-k), type="n",                                     
          axes=FALSE, main="", xlab=xlab, ylab=ylab,
           xlim=xlim,ylim=ylim,...)
    }
    else{ # horizontal
      if (missing(mar)) {
        mar[1] <- max(min.mar[1], trunc(mar[1] + maxchar/10)) + 0.1
      }
      par(mar=mar)
      plot(c(idx+k,idx-k), c(coefs.l, coefs.h), type="n", axes=FALSE, 
        main=main, xlab=xlab, ylab=ylab,...)                                                  
    }
  }
  invisible(list(idx=idx))
}

setMethod("coefplot2", signature(object = "numeric"),
  function(object, ...) {
    coefplot2.default(object, ...) } )

## should work for any model type for which 'coeftab' works;
##  still need to set 'coefplot' method (could try defining 'ANY' method
coefplot2.model <- function(object,
                            varnames=NULL,
                            intercept=FALSE,
                            var.idx =NULL,
                            ...) {
  if (!missing(intercept) && !missing(var.idx))
    stop("must specify at most one of intercept and var.idx")
  ctab <- coeftab(object, ...)
  if (!missing(var.idx)) {
    ctab <- ctab[var.idx,]
  } else if (!intercept) ctab <- ctab[-1,,drop=FALSE]
  if (is.null(varnames)) varnames <- rownames(ctab)
  if (length(varnames)!= nrow(ctab))
    stop(message="length of varnames != length of predictors",
         " (varnames must include a name for the constant/intercept)")
  class(ctab) <- c("coeftab","data.frame") ## FIXME:: kluge to restore class
  ## plotting; strip 'ptype' argument from plot call
  xargs <- list(...)
  xargs$ptype <- NULL
  do.call(coefplot2.default,c(list(coefs=ctab, varnames=varnames), xargs))
}

coefplot2.fitList <- function(object, col.pts=1:length(object),
                              pch.pts=16,
                              offset=0, spacing=0.1,
                              varnames=NULL, intercept=FALSE, var.idx=NULL,
                              xlim=NULL, ylim=NULL, vertical=TRUE,
                              merge.names=TRUE,
                              legend=FALSE,
                              legend.x="bottomright",
                              legend.args=NULL,
                              ...) {
  ## generate coef tab
  noint <- missing(intercept)
  noidx <- missing(var.idx)
  if (!noint && !noidx) stop("must specify at most one of intercept and var.idx")
  classes <- sapply(object,class)
  ## badclass <- unique(classes[!classes %in% knownclasses])
  ## if (length(badclass)>0)
  ## stop(paste("unknown model type(s):",paste(badclass,collapse=", ")))
  coeflist<- lapply(object,
                    function(x) {
                      if (inherits(x,"coeftab")) { x } else
                          coeftab(x,sd=FALSE,p.val=FALSE, ...)})
  ## browser()
  if (merge.names) {
    allnames <- unique(unlist(lapply(coeflist,rownames)))
    coeflist <- lapply(coeflist,extend_tab,vnames=allnames)
  }
  ## index specified:
  if (!noidx) {
    if (is.list(var.idx)) {
      coeflist <- mapply(function(X,i) X[i,],coeflist,var.idx,SIMPLIFY=FALSE)
    } else coeflist <- lapply(coeflist,function(x) x[var.idx,])
  }
  ## drop intercepts if appropriate
  has_int <- sapply(coeflist,function(x) rownames(x)[1]=="(Intercept)")
  if (!intercept) {
    if (all(!has_int)) {
      warning("dropping first coefficient from each model (non-standard naming)")
      coeflist <- lapply(coeflist,function(x) x[-1,])
    } else {
      if (!all(has_int)) warning("dropping intercepts by name; not all models have intercepts")
      coeflist <- lapply(coeflist,function(x) x[rownames(x)!="(Intercept)",])
    }
  }
  if (is.null(varnames)) varnames <- rownames(coeflist[[1]])
  if (length(varnames)!= nrow(coeflist[[1]]))
    stop(message="length of varnames != length of predictors",
         " (varnames must include a name for the constant/intercept)")
  n <- length(coeflist)
  offsetvec <- seq(offset,by=spacing,length.out=n, ...)
  ## browser()
  ## lims <- ## range(unlist(lapply(coeflist,function(x)x[,-1])),na.rm=TRUE)
  ##  we DO want to examine the estimate column -- for when all quantiles are NA
  lims <- range(unlist(coeflist),na.rm=TRUE)
  ## browser()
  vlims <- c(1,length(varnames)+max(offsetvec))
  if (vertical) {
    if (missing(xlim)) xlim <- lims
    if (missing(ylim)) ylim <- vlims
  } else {
    if (missing(ylim)) ylim <- lims
    if (missing(xlim)) xlim <- vlims
  }
  xargs <- list(...)
  xargs$ptype <- NULL
  do.call(coefplot2.default,
          c(list(coefs=coeflist[[1]],
                 col.pts=col.pts[1],
                 offset=offsetvec[1],
                 varnames=varnames,
                 xlim=xlim,ylim=ylim,
                 varname.offset=mean(offsetvec)),
            xargs))
  if (n>1) {
    for (i in 2:n) {
      do.call(coefplot2.default,
              c(list(coefs=coeflist[[i]],col.pts=col.pts[i],
                     offset=offsetvec[i],add=TRUE),xargs))
    }
  }
  if (legend) {
    Lnames <- if (!is.null(names(object)))names(object) else paste("model",1:n)
    Largs <- list(x=legend.x,
                  legend=rev(Lnames),
                  pch=rev(pch.pts),
                  col=rev(col.pts),
                  lty=1)
    if (!is.null(legend.args)) {
      Largs[names(legend.args)] <- legend.args
    }
    do.call("legend",Largs)
  }
}               

for (i in knownclasses) { ## see top of file for 'knownclasses'
  setMethod("coefplot2",signature(object=i), coefplot2.model)
}

setMethod("coefplot2", signature(object = "list"),
          coefplot2.fitList)


setMethod("coefplot2", signature(object = "ANY"),
          function(object,...) {
            coefplot2.model(object,...)
          })

as.coeftabList <- function(...,merge.names=TRUE) {
    L <- list(...)
    coeflist<- lapply(L,
                      function(x) {
                          if (inherits(x,"coeftab")) { x } else
                          coeftab(x,sd=FALSE,p.val=FALSE, ...)})
    ## FIXME:: should merge.names apply in coeftabList or in melt.coeftabList??
    if (merge.names) {
        allnames <- unique(unlist(lapply(coeflist,rownames)))
        coeflist <- lapply(coeflist,extend_tab,vnames=allnames)
    }
    if (is.null(Lnames <- names(L))) {
        ## try to guess names from input (fragile?? use mod[1..n] instead?)
        Lnames <- sapply(as.list(match.call()),deparse)[-1]
        Lnames <- Lnames[seq_along(L)]
    }
    names(coeflist) <- Lnames
    class(coeflist) <- "coeftabList"
    coeflist
}

melt.coeftabList <- function(data,fortify=FALSE,...) {
    ## combined function for melting alone (produces best human-readable data frame)
    ##  or melting+fortifying (adds extra info usable by ggplot2)
    if (!fortify) {
        ff <- function(d,n) data.frame(d,model=n,check.names=FALSE)
    } else {
        ff <- function(d,n) data.frame(d,model=n,param=rownames(d),check.names=FALSE)
    }
    m1 <- mapply(ff,data,names(data),SIMPLIFY=FALSE)
    m2 <- do.call(rbind,m1)
    if (fortify) {
        vlocs <- match(c("2.5%","25%","75%","97.5%"),names(m2))
        names(m2)[vlocs] <- c("lwr","lwr2","upr2","upr")
        ## avoid plyr dependency
        ## plyr::rename(m2,c(`2.5%`="lwr",
        ##                         `25%`="lwr2",
        ##                         `75%`="upr2",
        ##                         `97.5%`="upr"))
        rownames(m2) <- NULL
    }
    m2
}

fortify.coeftabList <- function(data,...) {
    melt(data,fortify=TRUE,...)
}

autoplot.coeftabList <- function(data,dodge.width=0.5,horizontal=FALSE,
                                 zeroline=TRUE,...) {
    ## FIXME: what other built-in options should be allowed?
    param <- Estimate <- lwr <- upr <- model <- NULL ## avoid check 'no visible binding' false positive
    g0 <- ggplot(fortify(data),aes(x=param,y=Estimate,ymin=lwr,ymax=upr,colour=model))
    if (zeroline) g0 <- g0 + geom_hline(yintercept=0)
    g0 <- g0 + geom_pointrange(position=position_dodge(width=dodge.width))
    if (!horizontal) g0 <- g0 + coord_flip()
    g0
}

if (FALSE) {
    ## testing
    ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
    group <- gl(2,10,20, labels=c("Ctl","Trt"))
    weight <- c(ctl, trt)
    lm.D9 <- lm(weight ~ group)
    lm.D90 <- lm(weight ~ group - 1) # omitting intercept
    L1 <- as.coeftabList(lm.D9,lm.D90)  ## not necessarily a sensible answer
    require(ggplot2)
    autoplot(L1)
}
