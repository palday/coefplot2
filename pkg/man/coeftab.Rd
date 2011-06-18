\name{coeftab}
\alias{coeftab}
\alias{coeftab0}
\alias{coeftab.default}
\alias{coeftab.MCMCglmm}
\alias{coeftab.rjags}
\alias{coeftab.bugs}
\alias{coeftab.mcmc}
\alias{coeftab.mcmc.list}
\alias{coeftab.lme}  
\alias{coeftab.glmmML}
\alias{coeftab.mer}
\alias{coeftab.glmm.admb}
\alias{coeftab.admb}
\alias{print.coeftab}
\title{
  Extract coefficient table
}
\description{
  Generic function to extract tables of coefficients, standard errors,
  etc., from various classes of fitted models
}
\usage{
coeftab(object, \dots)
coeftab0(object,ctype="quad",sd=TRUE,
                     cmult=1.96,clevel=c(0.5,0.95),
                     df,p.val=FALSE,quantiles=TRUE,\dots)
\S3method{coeftab}{MCMCglmm}(object,ptype="fixef",\dots)
\S3method{coeftab}{rjags}(object,\dots)
\S3method{coeftab}{bugs}(object,\dots)
\S3method{coeftab}{mcmc}(object, ptype, clevel = c(0.5, 0.95), ctype =
                 "quantile", sd = FALSE, \dots)
\S3method{coeftab}{mcmc.list}(object, \dots)
\S3method{coeftab}{lme}(object,ptype="fixef",ctype="quad",\dots) 
\S3method{coeftab}{glmmML}(object,ctype="quad",\dots)
\S3method{coeftab}{mer}(object,ptype="fixef",ctype="quad",\dots)
\S3method{coeftab}{glmm.admb}(object,ptype="fixef",\dots)
\S3method{coeftab}{admb}(object,\dots)
\S3method{print}{coeftab}(x,\dots)
}
\arguments{
  \item{object}{a fitted model object}
  \item{x}{a fitted model object}
  \item{ctype}{(character) one of "quad" (quadratic/Wald), "profile"
    (likelihood profile), "quantile" (quantiles of posterior
    density), "HPDinterval" (highest posterior density)}
  \item{ptype}{parameter type: one or more of "fixef" (default:
    fixed effect parameters), "ranef" (posterior modes of random
    effects), "vcov" (parameters of random effects variance-covariance
    matrices)}
  \item{sd}{(logical) include standard deviation in parameter table?}
  \item{clevel}{(Numeric) Vector of confidence (alpha) or quantiles to compute}
  \item{cmult}{(Numeric) Vector of multiples of standard error to
    compute}
  \item{df}{(integer) Degrees of freedom for p-value calculation}
  \item{p.val}{(logical) include p-values in parameter table?}
  \item{quantiles}{(logical) include quantiles in parameter table?}
  \item{\dots}{other stuff: in the case of \code{print}, arguments
  to be passed to \code{\link{printCoefmat}}}
  }
  \value{
    An object of class \code{coeftab}, containing one row per parameter
    and 
  }
  \author{
    Ben Bolker
  }
  \seealso{
    \code{\link[arm]{coefplot}},  \code{\link{coefplot2}}
  }
  \examples{
set.seed(1001)
 y1 <- rnorm(1000,50,23)
 y2 <- rbinom(1000,1,prob=0.72)
 x1 <- rnorm(1000,50,2) 
 x2 <- rbinom(1000,1,prob=0.63) 
 x3 <- rpois(1000, 2) 
 x4 <- runif(1000,40,100) 
 x5 <- rbeta(1000,2,2) 
 
 fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5)
 fit2 <- glm(y2 ~ x1 + x2 + x3 + x4 + x5, 
            family=binomial(link="logit"))

 coeftab(fit1)
 coeftab(fit1,p.val=TRUE)
 coeftab(fit2)
}
\keyword{misc}
