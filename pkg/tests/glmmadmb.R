library("coefplot2")
data(cbpp,package="lme4")
if (FALSE) {
## FIXME: 6 Nov 2012 glmmADMB not available for Windows binaries on r-forge?
library("glmmADMB")
g1 <- glmmadmb(cbind(incidence, size - incidence) ~ period + (1 | herd),
               family="binomial",data=cbpp)
coeftab(g1)
coeftab(g1,ptype="vcov")
coeftab(g1,ptype=c("fixef","vcov"))
}
