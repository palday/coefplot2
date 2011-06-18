## compare/contrast original and hacked coefplot
library(arm)
library(coefplot2)

## replicate examples from coefplot
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

n <- 100
x1 <- rnorm (n)
x2 <- rbinom (n, 1, .5)
b0 <- 1
b1 <- 1.5
b2 <- 2
y <- rbinom (n, 1, invlogit(b0+b1*x1+b2*x2))
y <- ifelse (x2==1, 1, y)
x1 <- rescale(x1)
x2 <- rescale(x2, "center")
      
M1 <- glm (y ~ x1 + x2, family=binomial(link="logit"))
M2 <- bayesglm (y ~ x1 + x2, family=binomial(link="logit"))
M3 <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
M4 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)

coef.vect <- c(0.2, 1.4, 2.3, 0.5)
sd.vect <- c(0.12, 0.24, 0.23, 0.15)

pdf("test3_arm.pdf")
## plot 1
op <- par()
par (mfrow=c(2,2))
coefplot(fit1)
coefplot(fit2, col.pts="blue")
      
## plot 2
longnames.i <- c("(Intercept)", longnames) 
coefplot(fit1, longnames.i, intercept=TRUE, CI=1)
      
## plot 3
coefplot(fit2, vertical=FALSE, var.las=1, frame.plot=TRUE)

par(mfrow=c(1,1))
# plot 4: comparison to show bayesglm works better than glm    
##=================== 
##    stacked plot
##===================
coefplot(M2, xlim=c(-1,5), intercept=TRUE)
## BMB: change from coefplot example (set intercept=TRUE)
coefplot(M1, add=TRUE, col.pts="red", intercept=TRUE)
       
##==================== 
## arrayed plot       
##====================
par(mfrow=c(1,2))
x.scale <- c(0, 7.5) # fix x.scale for comparison
coefplot(M1, xlim=x.scale, main="glm", intercept=TRUE)
coefplot(M2, xlim=x.scale, main="bayesglm", intercept=TRUE)

par(mfrow=c(1,1))
## plot 5: the ordered logit model from polr

coefplot(M3, main="polr")
    
coefplot(M4, main="bayespolr", add=TRUE, col.pts="red")

## FIXME: commented out in arm -- can it be resurrected?
## plot 6: plot bugs & lmer
## par <- op
## M5 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
## M5.sim <- mcmcsamp(M5)
## coefplot(M5.sim, var.idx=5:22, CI=1, ylim=c(18,1), main="lmer model")
     
     
## plot 7: plot coefficients & sds vectors
longnames <- c("var1", "var2", "var3", "var4")
coefplot (coef.vect, sd.vect, varnames=longnames, main="Regression Estimates")
coefplot (coef.vect, sd.vect, varnames=longnames, vertical=FALSE, 
          var.las=1, main="Regression Estimates")
dev.off()

par(old.par)

pdf("test3_c2.pdf")
## now with coefplot2
## plot 1
par(mfrow=c(2,2))
coefplot2(fit1)
coefplot2(fit2, col.pts="blue")
      
## plot 2
coefplot2(fit1, longnames.i, intercept=TRUE, CI=1)

## plot 3
coefplot2(fit2, vertical=FALSE, var.las=1, frame.plot=TRUE)

par(mfrow=c(1,1))

coefplot2(M2, xlim=c(-1,5), intercept=TRUE)
coefplot2(M1, add=TRUE, offset=0.1, col.pts="red",intercept=TRUE)

coefplot2(list(M2,M1),xlim=c(-1,5),intercept=TRUE)


##==================== 
## arrayed plot       
##====================
par(mfrow=c(1,2))
x.scale <- c(0, 7.5) # fix x.scale for comparison
coefplot2(M1, xlim=x.scale, main="glm", intercept=TRUE)
coefplot2(M2, xlim=x.scale, main="bayesglm", intercept=TRUE)

par(mfrow=c(1,1))
## plot 5: the ordered logit model from polr

coefplot2(M3, main="polr")
coefplot2(M4, main="bayespolr", add=TRUE, offset=0.1, col.pts="red")

coefplot2(list(M3,M4), main="polr/bayespolr")


## FIXME: commented out in arm -- can it be resurrected?
## plot 6: plot bugs & lmer
## par <- op
## M5 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
## M5.sim <- mcmcsamp(M5)
## coefplot2(M5.sim, var.idx=5:22, CI=1, ylim=c(18,1), main="lmer model")
     
     
## plot 7: plot coefficients & sds vectors
longnames <- c("var1", "var2", "var3", "var4")
coefplot2 (coef.vect, sd.vect, varnames=longnames, main="Regression Estimates")
coefplot2 (coef.vect, sd.vect, varnames=longnames, vertical=FALSE, 
          var.las=1, main="Regression Estimates")

dev.off()

