library(lme4)
library(coefplot2)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
gm1 <- glmer(incidence ~ period + (1|herd), cbpp)
coeftab(fm1,ptype="vcov")


## methods("coeftab")
## debug(coefplot2:::coeftab.lmerMod)
