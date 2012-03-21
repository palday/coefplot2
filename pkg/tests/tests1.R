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
coeftab(epil2_glmer_0)
coefplot2(list(epil2_glmer_0,epil2_glmer_1,epil2_glmmADMB))
