

## Assignement 1:
 
library("arm")    # Library arm contains "invlogit()" as a function se we do not need our own inv.logit()

wells <- read.table ("wells.dat")
attach.all (wells)   # attach makes that you can call the variable names without calling the dataframe. Thus "wells$switch" becomes just "switch"

hist (dist, breaks=seq(0,10+max(dist[!is.na(dist)]),10), 
   xlab="Distance (in meters) to the nearest safe well", 
   ylab="", main="", mgp=c(2,.5,0))

## Assignment 2:

fit.1 <- glm (switch ~ dist, family=binomial(link="logit"))
display (fit.1)  # Display is a nicer looking (but with less info) version of "summary()" which is in the "arm" package.

## Assignment 3:

dist100 <- dist/100
fit.2 <- glm (switch ~ dist100, family=binomial(link="logit"))
display (fit.2)   

## Assignment 4:

plot(dist,switch, xlab="Distance (in meters) to nearest safe well", ylab="Pr (switching)")
curve (invlogit(coef(fit.1)[1]+coef(fit.1)[2]*x), lwd=1, add=TRUE)
points (dist, jitter(switch), pch=20, cex=.1)

## Assignement 5:

hist (arsenic, breaks=seq(0,.25+max(arsenic[!is.na(arsenic)]),.25), freq=TRUE, xlab="Arsenic concentration in well water", ylab="", main="", mgp=c(2,.5,0))

## Logistic regression with second input variable

fit.3 <- glm (switch ~ dist100 + arsenic, family=binomial(link="logit"))
display (fit.3)

anova(fit.1, fit.3, test="Chisq")


## Assignement 6:

plot(arsenic, switch, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water", ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
curve (invlogit(coef(fit.3)[1]+coef(fit.3)[2]*0.5+coef(fit.3)[3]*x), from=0.5, lwd=.5, add=TRUE)
points (arsenic, jitter(switch), pch=20, cex=.1)
text (1.5, .78, "if dist = 0", adj=0, cex=.8)
text (2.2, .6, "if dist = 50", adj=0, cex=.8)

# Assignment 7:

fit.4 <- glm (switch ~ dist100 + arsenic + dist100:arsenic, 
  family=binomial(link="logit"))
display(fit.4)

## Assignment 8:

c.dist100 <- dist100 - mean (dist100)
c.arsenic <- arsenic - mean (arsenic)

## Assignement 9:

fit.5 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic,
  family=binomial(link="logit"))
display(fit.5)

anova(fit.3, fit.4, fit.5, test="Chisq")

## Assignement 10:

plot(arsenic, switch, xlim=c(0,max(arsenic)), xlab="Arsenic concentration in well water",
   ylab="Pr (switching)", type="n", xaxs="i", yaxs="i", mgp=c(2,.5,0))
curve (invlogit(cbind (1, 0, x, 0*x) %*% coef(fit.4)), lwd=.5, add=TRUE)
curve (invlogit(cbind (1, 0.5, x, 0.5*x) %*% coef(fit.4)), lwd=.5, add=TRUE)
points (arsenic, jitter(switch), pch=20, cex=.1)
text (1.5, .8, "if dist = 0", adj=0, cex=.8)
text (2.2, .6, "if dist = 50", adj=0, cex=.8)


