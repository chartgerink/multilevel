
### NEEDS FINISHING:


## Assignement 1:
data <- read.table("purchase_data.csv", sep=",",  quote = "\"'", header=TRUE)
summary(data)
head(data)

# 2: Plots
library(lattice)
xyplot(jitter(purchase) ~ strategy | customer, data=data)
xyplot(advertisement~ jitter(purchase), data=data)


# 3:
library(lme4)
m.0 <- lmer(purchase ~ 1 + (1 | customer), data=data, family=binomial(link = "logit"))
summary(m.0)

# 4 :
m.1 <-lmer(purchase ~ 1 + (1 | customer) + (1|product_id), data=data, family=binomial(link = "logit"))
summary(m.1)
anova(m.0, m.1)

# 5:

m.2 <-lmer(purchase ~ 1 + strategy +  (1 | customer), data=data, family=binomial(link = "logit"))
summary(m.2)
anova(m.0, m.2)


m.3 <-lmer(purchase ~ 1 + strategy +  (1 + strategy | customer), data=data, family=binomial(link = "logit"))
summary(m.3)
anova(m.0, m.2, m.3)

### And so on... Keep comparing models and select one.
## It is actually not trivial which one you would use...


##
# For the final assignment:
## 1. Take a plot that you created from the data in assignment 2
## 2. Examine the model that you specified
## 3. Write out the linear part of the model (and do draws for the errors etc)
##     3a. Start out with one "group" (e.g. customer) first before trying multiple
## 4. Model the probabilities -> p.i
## 5. Use rbinom(1, 1, p.i) to create the purchase data
## 6. Redo your plot with your new data


