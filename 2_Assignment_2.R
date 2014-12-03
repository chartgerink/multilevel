## Read the data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88


# A1:
poll <- read.table ( "polls.subset.dat")
head(poll)

# A2:
summary(poll.subset)

# A3:
lm(bush ~ 1, data=poll)

# A4
library(lme4)
m1 <- lmer(bush ~ 1 + (1 | state), data=poll, family=binomial(link="logit"))
summary(m1)

# A5
m2  <-  lmer(bush ~ 1 + age +  (1 | state), data=poll, family=binomial(link="logit"))
summary(m2)

anova(m1,m2)

# A6 
m3 <-  lmer(bush ~ 1 + age +  (1  +  age | state), data=poll, family=binomial(link="logit"))
anova(m1,m3)


#A7

inv.logit <- function(x){
  return(exp(x)/(1+exp(x)))
  }

logit <- function(p){
  return(log(p/(1-p)))
}

plot(jitter(polls$age), jitter(polls$bush), type="p")
curve(inv.logit(fixef(m3)[1] + fixef(m3)[2] * x), add=TRUE, col="black", lwd=2)
for(i in c(1:51)){
  curve(inv.logit(coef(m3)$state[i,1] + coef(m3)$state[i,2] * x), add=TRUE, col="gray")
}

## More detail:
plot(jitter(polls$age), jitter(polls$bush), type="p", ylim=c(.5,.7))
curve(inv.logit(fixef(m3)[1] + fixef(m3)[2] * x), add=TRUE, col="black", lwd=2)
for(i in c(1:51)){
  curve(inv.logit(coef(m3)$state[i,1] + coef(m3)$state[i,2] * x), add=TRUE, col="gray")
}

