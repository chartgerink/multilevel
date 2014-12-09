
library(plyr)
library(arm) # contains invlogit()



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A1:
## Open FirstSex.
load("Tenure.RData")

# us ls() to see the name of the dataset:
ls()

# The data is called tenure, lets inspect the first few rows:
head(tenure)
# it clearly is a person period dataset.

# First, the timepoints:
time <- sort(unique(tenure$PERIOD))

# Next, the risk
# Use plyr to run through the person period dataset in chunks of period and count the rows
risk <- ddply(tenure, .(PERIOD), function(x){return(length(x$ID))})[,2]

# occurance:
# Use plyr to run through the person period dataset in chunks of period and sum the events
# if the number of events is 0 then explictly return a 0
occur <- ddply(tenure, .(PERIOD), function(x){sum(x$EVENT)})[,2]

# censor
# make t-1 zeros and then the difference between risk and occur:
censor <- c( rep(0, length(time)-1), risk[length(time)] - occur[length(time)])

# hazard: very simple in vector form:
hazard <- occur / risk

# survival: slightly trickier. Lets create a loop:
survival <- rep(NA, length(time))
for(i in time){
	# for the first timepoint:
	if(i == 1){
		survival[i] <- 1 - hazard[i]
	} else {
		survival[i] <- survival[i-1] * (1- hazard[i])
	}
}
# check:
survival

# paste together and print to see the life table
life.table <- data.frame(time=time, risk=risk, occur=occur, censor=censor, hazard=hazard, survival=survival)
print(life.table)



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A2:

# first create the higher order terms of PERIOD
tenure$PERIOD2 <- tenure$PERIOD^2
tenure$PERIOD3 <- tenure$PERIOD^3
tenure$PERIOD4 <- tenure$PERIOD^4

# fit the model
m1 <- glm(EVENT ~ PERIOD + PERIOD2 + PERIOD3 + PERIOD4, data= tenure, family=binomial)
m1

# check the coefs:
coef(m1)
b <- coef(m1)

# now we need to compute the predicted hazard at each timepoint
# lets write it out in full:
y.lin <- b[1] + b[2]*time + b[3]*time^2 + b[4]*time^3 + b[5]*time^4
pred.hazard <- invlogit(y.lin)

# lets add it to our life table
life.table$model.hazard <- pred.hazard
print(life.table)

# and, lets plot the observed and the predicted hazard (not in the assignment):
plot(hazard ~ time, type="l")
points(pred.hazard, type="l", col="red")
# That is a pretty good fit!




## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A3:

# Open dataset
load("Depression.RData")
ls()

# inspect
head(depression)
summary(depression)
dim(depression)
# etc.


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A4:

# we have PERIOD, and EVENT.
# lets see all periods:
unique(depression$PERIOD)

# thats quite a lot... Lets start the examination of time bottum up:
# First, create some higher order versions of time:
depression$PERIOD2 <- depression$PERIOD^2
depression$PERIOD3 <- depression$PERIOD^3
depression$PERIOD4 <- depression$PERIOD^4
depression$PERIOD5 <- depression$PERIOD^5
depression$PERIOD6 <- depression$PERIOD^6
depression$PERIOD7 <- depression$PERIOD^7
depression$PERIOD8 <- depression$PERIOD^8

# Now, build models:
m0 <- glm(EVENT ~ 1, data=depression, family=binomial)
m1 <- glm(EVENT ~ 1 + PERIOD, data=depression, family=binomial)
m2 <- glm(EVENT ~ 1 + PERIOD + PERIOD2, data=depression, family=binomial)
m3 <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3, data=depression, family=binomial)
m4 <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4, data=depression, family=binomial)
m5 <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + PERIOD5, data=depression, family=binomial)
m6 <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + PERIOD5 + PERIOD6, data=depression, family=binomial)
m7 <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + PERIOD5 + PERIOD6 + PERIOD7, data=depression, family=binomial)
m8 <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + PERIOD5 + PERIOD6 + PERIOD7 + PERIOD8, data=depression, family=binomial)

# and compare:
anova(m0, m1, m2, m3, m4, m5, m6, m7, m8, test="Chisq")

## USING CHI^2 TEST we would decide for model 4 using a 4th order polynomial.
# You should also try to check AIC and BIC!
# And, for interpretation it would have been good to center PERIOD first (using PERIOD <- PERIOD - mean(PERIOD))



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A5: Plotting of the hazard using the 4th order model:

b <- coef(m4)
time <- sort(unique(depression$PERIOD))
y.lin <- b[1] + b[2]*time + b[3]*time^2 + b[4]*time^3 + b[5]*time^4
pred.hazard <- invlogit(y.lin)

plot(pred.hazard ~ time, type="l")



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A6: Males and females:
# First add FEMAL to the model:
m4a <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + FEMALE, data=depression, family=binomial)

# Then plot males:
b <- coef(m4a)
time <- sort(unique(depression$PERIOD))
y.lin <- b[1] + b[2]*time + b[3]*time^2 + b[4]*time^3 + b[5]*time^4
pred.hazard.male <- invlogit(y.lin)
plot(pred.hazard.male ~ time, type="l")

# and add females
y.lin <- b[1] + b[2]*time + b[3]*time^2 + b[4]*time^3 + b[5]*time^4 + b[6] * 1
pred.hazard.female <- invlogit(y.lin)
points(pred.hazard.female, type="l", lty=2)

# oops, that is an ugly plot. Lets change the order:
plot(pred.hazard.female ~ time, type="l")
points(pred.hazard.male, type="l", lty=2)
# Pretty cool...


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A8. Check whether an interaction exists.
# refit model 4a (redundant)
# add CENSAGE
# add interaction
m4a <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + FEMALE, data=depression, family=binomial)
m4b <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + FEMALE + CENSAGE, data=depression, family=binomial)
m4c <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + FEMALE * CENSAGE, data=depression, family=binomial)
anova(m4a, m4b, m4c, test="Chisq")

# Model with interaction is not a significant improvement. Hence the linear additivity assumption seems to hold.


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# A9. Check whether there is an interction with time.
m4a <- glm(EVENT ~ 1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4 + FEMALE, data=depression, family=binomial)
# add interaction of female and time:
m4d <- glm(EVENT ~ (1 + PERIOD + PERIOD2 + PERIOD3 + PERIOD4) * FEMALE, data=depression, family=binomial)
# have a look at the fitted model
summary(m4d)
anova(m4a, m4d, test="Chisq")

# No improvement, so no interaction between female and time: the proportionality assumption seems to hold. 
# However, you should also check this for the individual components of time if you want to be fully torough.

