library(coefplot2)
library(lme4)
library(glmmADMB)
if (FALSE)  {
  ## lme4
  epil2$indiv <- 1:nrow(epil2)
  epil2_glmer_0 <- glmer(y~Base*trt+Age+Visit+(Visit|subject),
             data=epil2,family="poisson")
  epil2_glmer_1 <- update(epil2_glmer_1,.~.+(1|indiv))
  ##
  ## glmmADMB (0.5-2)
  epil2_glmmADMB <- glmm.admb(y~Base*trt+Age+Visit,
                              random=~Visit, group="subject",
                              data=epil2, family="nbinom")
}
load("epil2_fits.RData")
coeftab(epil2_glmer_0)
coefplot2(list(epil2_glmer_0,epil2_glmer_1,epil2_glmmADMB))
