\name{coefplot2}
%\docType{genericFunction}
\alias{coefplot2}
\alias{coefplot2.default}
\alias{coefplot2.model}
\alias{coefplot2.fitList}
\alias{coefplot2,numeric-method}
\alias{coefplot2,lm-method}
\alias{coefplot2,glm-method}
\alias{coefplot2,bugs-method}
\alias{coefplot2,polr-method}
\alias{coefplot2,mcmc-method}
\alias{coefplot2,mer-method}
\alias{coefplot2,merMod-method}
\alias{coefplot2,jags-method}
\alias{coefplot2,rjags-method}
\alias{coefplot2,list-method}
\alias{coefplot2,glmm.admb-method}
\alias{coefplot2,MCMCglmm-method}
\alias{coefplot2,glmmPQL-method}
\alias{coefplot2,admb-method}
\alias{coefplot2,glmmML-method}
\alias{coefplot2,lme-method}
\alias{coefplot2,ANY-method}


\title{Generic Function for Making Coefficient Plots}
\description{
  Functions that plot the coefficients with confidence or
  credible intervals from a wide range of statistical models
}
\usage{
coefplot2(object,\dots)

\method{coefplot2}{default}(coefs, sds, varnames=NULL, CI=2,
                vertical=TRUE, v.axis=TRUE, h.axis=TRUE, top.axis=TRUE,
                cex.var=0.8, cex.pts=0.9, 
                col.pts=1, pch.pts=16, var.las=2,
                main="Regression estimates",
                xlab="", ylab="", mar=NULL, plot=TRUE,
                add=FALSE, offset=0, lwd.1=2, lwd.2=1,
                lower1, upper1, lower2, upper2, \dots)
\S4method{coefplot2}{list}(object, col.pts=1:length(object),
                             pch.pts=16,
                             offset=0, spacing=0.1,
                             varnames=NULL, intercept=FALSE, var.idx=NULL,
                             xlim=NULL, ylim=NULL, vertical=TRUE,
                             merge.names=TRUE,
                             legend=FALSE,legend.args=NULL,
                             \dots)
\S4method{coefplot2}{numeric}(object, \dots)
\S4method{coefplot2}{lm}(object, varnames=NULL, intercept=FALSE, var.idx=NULL, \dots)
\S4method{coefplot2}{glm}(object, varnames=NULL, intercept=FALSE, var.idx=NULL, \dots)
\S4method{coefplot2}{polr}(object, varnames=NULL,  intercept=FALSE, var.idx=NULL,\dots)
\S4method{coefplot2}{glmmML}(object, varnames=NULL,  intercept=FALSE, var.idx=NULL,\dots)
\S4method{coefplot2}{lme}(object, varnames=NULL,  intercept=FALSE, var.idx=NULL,\dots)
\S4method{coefplot2}{glmmPQL}(object, varnames=NULL,  intercept=FALSE, var.idx=NULL,\dots)
\S4method{coefplot2}{admb}(object, varnames=NULL,  intercept=FALSE, var.idx=NULL,\dots)
\S4method{coefplot2}{MCMCglmm}(object, varnames=NULL,  intercept=FALSE, var.idx=NULL,\dots)
\S4method{coefplot2}{list}(object, col.pts=1:length(object),
                             pch.pts=16,
                             offset=0, spacing=0.1,
                             varnames=NULL, intercept=FALSE, var.idx=NULL,
                             xlim=NULL, ylim=NULL, vertical=TRUE,
                             merge.names=TRUE,
                             legend=FALSE,legend.args=NULL,
                             \dots)
}

\arguments{
  \item{object}{fitted model object, or a vector of coefficients}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{coefs}{a vector of coefficients.}
  \item{sds}{a vector of sds of coefficients.}
  \item{CI}{confidence interval, default is 2, which will plot plus and minus 2 sds or
    95\% CI. If CI=1, plot plus and minus 1 sds or 50\% CI instead.}
  \item{lower1}{lower limit of inner confidence interval}
  \item{upper1}{upper limit of inner confidence interval}
  \item{lwd.1}{line width for the inner part of the confidence range}
  \item{lower2}{lower limit of outer confidence interval}
  \item{upper2}{upper limit of outer confidence interval}
  \item{lwd.2}{line width for the outer part of the confidence range}
  \item{xlim}{x limits}
  \item{ylim}{y limits}
  \item{varnames}{a vector of variable names, default is NULL, which will use
    the names of variables; if specified, the length of varnames must be equal to
    the length of predictors, including the intercept}
  \item{vertical}{orientation of the plot, default is TRUE which will plot
    variable names in the 2nd axis. If FALSE, plot variable names in the first
    axis instead.}
  \item{v.axis}{(logical) show bottom axis?}
  \item{top.axis}{(logical) show axis on top (vs. bottom) of plot?}
  \item{h.axis}{(logical) show left axis?}
  \item{cex.var}{Font size of variable names}
  \item{cex.pts}{Size of data points}
  \item{col.pts}{Color of points and segments}
  \item{pch.pts}{Symbol of points}
  \item{var.las}{Orientation of variable names against the axis:
    see the usage of \code{las} in \code{\link{par}}}
  \item{main}{The main title (on top) using font and size (character
          expansion) \code{par("font.main")} and color \code{par("col.main")}.}
  \item{xlab}{X axis label using font and character expansion
          \code{par("font.lab")} and color \code{par("col.lab")}.}
  \item{ylab}{Y axis label, same font attributes as \code{xlab}.}
  \item{mar}{A numerical vector of the form \code{c(bottom, left, top, right)}
          which gives the number of lines of margin to be specified on
          the four sides of the plot. The default is \code{c(1,3,5.1,2)}.}
  \item{plot}{(logical) plot the estimates?}
  \item{add}{(logical) plot over the existing plot?}
  \item{offset}{added extra spaces to separate from the existing dots}
%  \item{lower.bound}{default is -Inf.}
  \item{var.idx}{the index of the variables: default
    is to plot all the variables.}
  \item{intercept}{(logical) plot intercept parameter?}
  \item{spacing}{in multi-model plots, spacing between points from
    different models}
  \item{merge.names}{(logical) attempt to merge names from different
    models?}
  \item{legend}{(logical) plot legend?}
  \item{legend.args}{(list) list of arguments to \code{\link{legend}},
    overriding built-in defaults}
}

\details{
  This function plots coefficients from bugs, lm, glm and polr
  with 1 sd and 2 sd interval bars.
  Current modeling classes known include
  \code{polr}, \code{bugs}, \code{glmmML}, code{lme},
  \code{glmmPQL}, \code{MCMCglmm}, \code{mcmc}, \code{admb},
  \code{rjags}, and \code{glmm.admb}
}
\note{This is a heavily modified and extended version of the
  original \code{\link[arm]{coefplot}} in the \code{arm} package
}

\value{
   Plot of the coefficients from a bugs, lm or glm fit. You can add the intercept, 
   the variable names and the display the result of the fitted model. 
}

\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006}
            
	  \author{Yu-Sung Su \email{suyusung@tsinghua.edu.cn} (original),
	    Ben Bolker \email{bolker@mcmaster.ca} (hacked version)
	  }
        
\seealso{\code{\link[arm]{display}},
         \code{\link[graphics]{par}},
         \code{\link[stats]{lm}},
         \code{\link[stats]{glm}},
         \code{\link[arm]{bayesglm}},
         \code{\link[graphics]{plot}}
        }
\examples{
old.par <- par(no.readonly = TRUE)

set.seed(1001)
 y1 <- rnorm(1000,50,23)
 y2 <- rbinom(1000,1,prob=0.72)
 x1 <- rnorm(1000,50,2) 
 x2 <- rbinom(1000,1,prob=0.63) 
 x3 <- rpois(1000, 2) 
 x4 <- runif(1000,40,100) 
 x5 <- rbeta(1000,2,2) 
 
 longnames <- c("a long name01","a long name02","a long name03",
                "a long name04","a long name05")

 fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5)
 fit2 <- glm(y2 ~ x1 + x2 + x3 + x4 + x5, 
            family=binomial(link="logit"))
 op <- par()
 # plot 1
 par (mfrow=c(2,2))
 coefplot2(fit1)
 coefplot2(fit2, col.pts="blue")
 
 # plot 2
 longnames <- c("(Intercept)", longnames) 
 coefplot2(fit1, longnames, intercept=TRUE, CI=1)
 
 # plot 3
 coefplot2(fit2, vertical=FALSE, var.las=1, frame.plot=TRUE)

if (require(arm)) {
 # plot 4: comparison to show bayesglm works better than glm
 n <- 100
 x1 <- rnorm (n)
 x2 <- rbinom (n, 1, .5)
 b0 <- 1
 b1 <- 1.5
 b2 <- 2
 y <- rbinom (n, 1, plogis(b0+b1*x1+b2*x2))
 y <- ifelse (x2==1, 1, y)
 x1 <- rescale(x1)
 x2 <- rescale(x2, "center")
 
 M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"))
       display (M1)
 M2 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"))
       display (M2)

#=================== 
#    stacked plot
#===================
  coefplot2(M2, xlim=c(-1,5), intercept=TRUE)
  coefplot2(M1, add=TRUE, col.pts="red")
  
#==================== 
# arrayed plot       
#====================
  par(mfrow=c(1,2))
  x.scale <- c(0, 7.5) # fix x.scale for comparison
  coefplot2(M1, xlim=x.scale, main="glm", intercept=TRUE)
  coefplot2(M2, xlim=x.scale, main="bayesglm", intercept=TRUE)

# plot 5: the ordered logit model from polr
 M3 <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
 coefplot2(M3, main="polr")
   
 M4 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
 coefplot2(M4, main="bayespolr", add=TRUE, col.pts="red")

## plot 6: plot bugs & lmer
# par <- op
# M5 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
# M5.sim <- mcsamp(M5)
# coefplot2(M5.sim, var.idx=5:22, CI=1, ylim=c(18,1), main="lmer model")

}

# plot 7: plot coefficients & sds vectors
 coef.vect <- c(0.2, 1.4, 2.3, 0.5)
 sd.vect <- c(0.12, 0.24, 0.23, 0.15)
 longnames <- c("var1", "var2", "var3", "var4")
 coefplot2 (coef.vect, sd.vect, varnames=longnames, main="Regression Estimates")
 coefplot2 (coef.vect, sd.vect, varnames=longnames, vertical=FALSE, 
    var.las=1, main="Regression Estimates")
    
par(old.par)
}

\keyword{hplot}
\keyword{dplot}
\keyword{methods}
\keyword{manip}
