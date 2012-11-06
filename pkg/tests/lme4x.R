library(lme4)
library(coefplot2)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
gm1 <- glmer(incidence ~ period + (1|herd), cbpp)
coeftab(fm1,ptype="vcov")
coeftab(fm1,ptype="sdcor") ## NULL
coeftab(fm1)
coeftab(gm1)
coefplot2(fm1,ptype="vcov")
coefplot2(fm1,intercept=TRUE)
detach("package:lme4")
## 
if (FALSE) {
## problems with lme4.0/lme4 coexistence ...
library(lme4.0)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
gm1 <- glmer(incidence ~ period + (1|herd), cbpp)
coeftab(fm1,ptype="vcov")
coeftab(fm1,ptype="sdcor")
coeftab(fm1)
coeftab(gm1)
coefplot2(gm1)
}
