library(coefplot2)
library(lme4)
library(glmmADMB)
if (FALSE)  {
  ## lme4
  epil2 <- transform(epil2,
                     indiv=factor(seq(nrow(epil2))),
                     subject=factor(subject))
  epil2_glmer_0 <- glmer(y~Base*trt+Age+Visit+(Visit|subject),
             data=epil2,family="poisson")
  epil2_glmer_1 <- update(epil2_glmer_1,.~.+(1|indiv))
  ##
  ## glmmADMB (0.5-2)
  epil2_glmmADMB <- glmmadmb(y~Base*trt+Age+Visit+(Visit|subject),
                              data=epil2, family="nbinom")
  save(list=ls(pattern="epil2_"),file="epil2_fits.RData")
}
load("epil2_fits.RData")

convert_oldmer <- function(x) {
    cc <- class(x)
    if (!(cc=="mer" && attr(cc,"package")=="lme4"))
        stop("this function is designed to 'mer' objects from (old) lme4",
             " to 'mer' objects from lme4.0")
    attr(cc,"package") <- "lme4.0"
    class(x) <- cc
    x
}

coeftab(convert_oldmer(epil2_glmer_0))
coefplot2(list(epil2_glmer_0,epil2_glmer_1,epil2_glmmADMB))
