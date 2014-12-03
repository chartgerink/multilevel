

# Annoted .R script of the assignment for lecture 4.
# I hope this is off help for the exam.



# ========================================
# Q1:
# ========================================
#Open the datafile, and select only those recorded stops which are associated with drugs. How many rows of data do you now have?


stops <- read.table("stops.dat.txt", header=TRUE)  # opens the file. Always use summary() and head() to see if your data is opened properly.
summary(stops)
head(stops)  # Make sure the dataframe (here called "stops") contains seperate rows. Attend to "header=TRUE" if the file contains headers.

# This line selects from "stops" all thos rows in which "stop$crime==4": thus those rows for which the recorded stops are associated with drugs.
data <- stops[stops$crime==4,]
# similarly "precinct <- stops[stops$precinct==1,]" would select a subset where precinct = 1.

dim(data)  # dim gives the dimensions of the dataframe (rows, cols). So, there are 225 rows in the new dataframe.

# you can also use:
length(data$stops)
# which counts the number of entries in the stops column.

# note that
length(unique(data$stops))
# counts the number of unique entries of the vector "stops". This might also be off use.



# ========================================
# Q2:
# ========================================
#Lets start by fitting a glm poisson model to the data. We start with a simple model with just an intercept. Fit the model.


# Note here the use of "glm" (no hierarchical / multi-level structure) and "family=poison" for the link function.
m1 <- glm(stops ~ 1, data=data, family=poisson)

#inspect the fitted model:
summary(m1)
# you will see that there is only an intercept and the estimate value is 4.2002. The standard error of this estimate is 0.008 and the p-value (z-test whether the coefficient is equal to 0) is < .0001
# You will also see the deviance and the AIC.



# ========================================
#Q3:
# ========================================
#What is the rate parameter of the poison distribution that is estimated? And thus, what is the expected number of stops?


coef(m1)  # Gives the value of the intercept (the output of the linear predictor for this simple model). Output is 4.200238
exp(coef(m1)) # Use the link function of the poisson "exp()" to go from the linear predictor to the expected value of the poisson distribution. Output is 66.7

mean(data$stops) # this computes the mean number of stops directly from the data. Note that this is equal to the mean number of stops predicted by the model.



# ========================================
#Q4:
# ========================================
#Now, see if there is a fixed effect for the population size. Add this effect to the model and compare these two models using a $\chi^2$ test.#


# 1. We add "pop" to the model
m2 <- glm(stops ~ 1 + pop, data=data, family=poisson)

# 2. We inspect the new model
summary(m2)
# this model contains an intercept and a fixed effect of pop (both significantly different from 0). Note that the coefficient for pop is very small, so it is probably a good idea (to ease interpretation) to specify the population in 1000's or somthing (e.g. "data$pop.1000 <- data$pop / 1000;")

# Now, model comparisons: note the use of "test="Chisq"": this is only necessary for glm comparisons.
anova(m1,m2, test="Chisq")
# in the output you will see that the df of the deviance test is 1 (for pop) and that the chi-square test is significant. Hence, its a good idea to include pop in the model.




# ========================================
#Q5:
# ========================================
#Plot for both the 25% percentile and the 75% percentile of the population size the distribution of the predicted number of stops (use "histogram" from the "lattice" package).


# 1. get the percentiles using the "quantile()" function. The first argument is the vector of data you want to use, the second specifies the percentiles you are interested in. Also try "quantile(data$pop, c(.05, .5, .95))"
q <- quantile(data$pop, c(.25,.75))
q  # always inspect what you created by printing it to the screen.

# 2. Get the coefficients from model 2 (the intercept and the slope for "pop").
c <- coef(m2)
c

# now, the linear part of the model for the lower quartile is given by:
#  c[1] + c[2]*q1
# thus, the intercept, plus the slope time the value of "pop" for the 25th percentile

# we than simulate a number of draws from the poison distribution with the appropriate parameter:
n <- 1000  # number of draws
sim1 <- rpois(n, exp(c[1] + c[2]*q[1]))  # get 1000 (n) draws from poison distribution "rpoison()" with a parameter given by exp(c[1] + c[2]*q[1])
sim2 <- rpois(n, exp(c[1] + c[2]*q[2])) # similarly but now for percentile 75

library(lattice)  # use lattice for plotting
histogram(sim1)  # this gives the histogram of expected observations for the 25% percentile of pop. Thus, this is how many stops we would expect for precinct with a small population.

# However, it might be nice to plot the results in 1 plot:
# 1. We add the data to a dataframe
plot.data <- data.frame(c(sim1,sim2), c(rep(0,n),rep(1,n)))
# this gives a long column with sim1 (1000 entries) en sim2 (1000 entries) and a long row with 0's (1000x) and 1's (1000x) to identify the percentile

# 2. I set readable names for the columns:
names(plot.data) <- c("result", "quantile")

# I plot a histogram ( ~ result) split by "quantile" using plot.data as the data.
histogram( ~ result | quantile, data=plot.data)
# from this plot it is pretty clear that the number of stops for precincts with a large population is higher.



# ========================================
# Q6
# ========================================
#Now fit a hierarchical poison model to the data. Start by fitting a null model, with random intercepts for the precincts.


library(lme4)  # first include library lme4 so that calls to lmer() actually work

# fit the "null model" (overall intercept, and random intercept per precinct). Do not forget the link function
hm1 <- lmer(stops ~ 1 + (1 | precinct), data=data, family=poisson)

# inspect your fitter model:
summary(hm1)
# for a null model you should note the value of the intercept (3.98392) in this case, and the variance of the intercept which is 0.50176. This is relatively large compared to the value of the intercept (that is a fuzzy criteria, but you could say that 95% percent of the intercepts lie between 3.98392 + 1.96*sqrt(0.5176) and 3.98392 - 1.96*sqrt(0.5176)  (this is an asymptotic confidence interval).



# ========================================
# Q7:
# ========================================
#Add a fixed effect of population to this hierarchical model. What do you find?

# We add population (which we know to be important from before) to the model:
hm2 <- lmer(stops ~ 1 + pop + (1 | precinct), data=data, family=poisson)
summary(hm2)  ## FAILS TO CONVERGE!

# now, this "fails to converge". Why? probably because the population is different for most of the precinct and hence the two variable (precinct and population) are heavily related. This can happen in real analysis. You could perhaps specify population differently (e.g. low, medium, high), or choose to omit the random term of precinct. In any case, I will make sure this does not happen in the exam...




# ========================================
# Q8:
# ========================================
#Add a fixed effect of ethnicity to the hierarchical model. What do you find? Compare this model to the ``null'' model.


# so, pop is removed because of the issue in Q7
# we add "factor(eth)" to the model (otherwise "eth" is treated continuous which obviously it is not).
# you could also say "data$eth2 <- factor(data$eth)" to explicitly add a new column to the dataframe with the factor ethnicity.

# we fit the model:
hm3 <- lmer(stops ~ 1 + factor(eth) + (1 | precinct), data=data, family=poisson)

# we inspect the model:
summary(hm3)
# note the dummies for factor(eth)2 and factor(eth)3: the coefficient uses a contrast and the baseline is factor(eth)1. All coefficients differ from 0 significantly.

# now, compare to the null model:
anova(hm1, hm3)
# note1: Not neccesary to specify "test=Chisq"
# note2: The AIC is lower, the BIC is lower, and the deviance test is significant. Hence, model "hm3" is better than the null model.




# ========================================
# Q8:
# ========================================
#Given your previous output, for which ethnicity is the number of stops the highest? Would you trust this conclusion? (If no, why not?). 

# this is a interpretation question. However, look at the output of:
fixef(hm3)  # which shows only the fixed effects. Do recall coef(), and ranef() for the total effect and the random effect of lmer models
# you see that for factor(eth)2 the fixef is -.2137, and for factor(eth)3 it is -1.038.
# Thus, the largest difference between ethnicity is factor(eth)1 (which is 0 since its the reference categorie) and factor(eth)3. # 1 is black, #3 is white:
# compute:
exp(fixef(hm3)[1] + 0) # linear part "fixef(hm3)[1] + 0" for blacks, than use link function to get the rate parameter of the associated poison distribution.
# E(yi | xi) = 74.57

# and:
exp(fixef(hm3)[1] + 1* fixef(hm3)[3]) # linear part + link function for whites
# E(yi | xi) = 26.4

# difference:
exp(fixef(hm3)[1] + 0) - exp(fixef(hm3)[1] + 1* fixef(hm3)[3]) # => 48.17

# so, we expect 48 more stops on blacks than on whites. That is a huge difference! (its more than 2 as often)

# Now, would you trust this? Well this is an interpretation question: is the difference really "caused by" race? Or do blacks live in bigger precincts? Are ther other things that drive the difference? Without a thorough examination of other (joint) causes I would be very carefull to publish the conclusion that "blacks are much more likely to be stopped by the police".


# ========================================
# Q9:
# ========================================

#How do the differences between race compare to the differences between precincts?

# here I am asking you to "compare the random term and the fixed term of the model. If you look at my results for Q8 you will see that the largest difference "caused by" ethnicity is 48.17 stops. What is the difference caused by precincts? (can we get some ballpark figure?)

# lets examine the random effect:
summary(hm3)
# we see the variance of the random intercept is .50177

# we can now say that for small precincts (smallest 5% - using asymptotic confidence interval) for blacks (reference category) the expected count is:
exp(4.31179 - (1.96*sqrt(.50177)))
# where I use 1.96 * SD as the lower 5% boundary. (sqrt(VARIANCE) = SD)

# the upper 95% bound is at:
exp(4.31179 + (1.96*sqrt(.50177)))

# and the difference between small and large precincts is:
exp(4.31179 + (1.96*sqrt(.50177))) - exp(4.31179 - (1.96*sqrt(.50177)))
# about 280 stops.

# compare this to the differnce "caused by" ethnicity: 48 stops. It seems like the variation between precincts is much more important than the variation between ethnicity.
