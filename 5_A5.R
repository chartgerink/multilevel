library(foreign)
library(plyr)

# A1:

set.seed(180)
n <- 200  # Number of people 
id <- paste("PP", c(1:n))  # Create an ID
time <- rpois(n, 5)  # Random event occurrence
censor <- rep(0,n)

# Random censoring of highest
censor[time==max(time)] <-  1- rbinom( length(time[time==max(time)]), 1, .5)  
person.data <- data.frame(
	id = id,
	time = time,
	censor = censor)

person.data

mytable <- table(person.data$id, person.data$time)
life.table <- data.frame(margin.table(mytable, 2))
names(life.table) <- c("period", "count")

# risk:
life.table$risk <- n - cumsum(life.table$count) + life.table$count

# censor:
life.table$censor <- c(rep(0, length(life.table$risk)-1), sum(person.data$censor))

# occur 
life.table$occur <- life.table$count - life.table$censor

# hazard
life.table$hazard <- life.table$occur / life.table$risk
plot(life.table$hazard, type="l")

# survival
survive <- rep(NA, length(life.table$hazard))
survive[1] <- 1 * (1- life.table$hazard[1])
for(i in 2:length(life.table$hazard)){
	survive[i] <- survive[i-1] * (1- life.table$hazard[i])
}
life.table$survive <- survive
life.table



# A2: THIS ONE IS NOT GENERAL: WE MAKE A BETTER ONE IN CLASS:

min <- min(person.data$time)

# For each id create a data.frame
person.period <- ddply(person.data, .(id), function(x, min){
	max <- x$time
	id = rep(x$id, (max+1)-min)
	period = c(min: max)
	event = c(
		rep(0, max-min),
		ifelse(x$censor==0,1,0)
		)
	data.frame(id=id, period=period, event=event)
	}, min=min) 

head(person.period)

# A3: SAME HERE:

pp.data <- person.period

unique.period <- unique(pp.data$period)
for(i in unique.period){
	new.col <- ncol(pp.data)+1  # Which column to update?
	# Add identifier for period
	pp.data[, new.col] <- ifelse(pp.data$period==i, 1,0)   
	x <- length(names(pp.data))
	names(pp.data) <- c(names(pp.data)[1:x-1], paste("Period", i, sep="_"))
}

head(pp.data)



### SECOND PART:

# A4:
## Open FirstSex.
data <- read.spss('FirstSex.sav', to.data.frame=T)
head(data)

### To pp:
min <- min(data$TIME)
# For each id create a data.frame
pp <- ddply(data, .(ID), function(x, min){
	max <- x$TIME
	id = rep(x$ID, (max+1)-min)
	period = c(min: max)
	event = c(
		rep(0, max-min),
		ifelse(x$CENSOR==0,1,0)
		)
	data.frame(id=id, period=period, event=event)
	}, min=min) 

head(pp)

# Add dummies
unique.period <- unique(pp$period)
for(i in unique.period){
	new.col <- ncol(pp)+1  # Which column to update?
	# Add identifier for period
	pp[, new.col] <- ifelse(pp$period==i, 1,0)   
	x <- length(names(pp))
	names(pp) <- c(names(pp)[1:x-1], paste("Period", i, sep="_"))
}
head(pp)

# A5: 
# Fit Logit model:
mod1 <- glm(event ~ -1 + Period_7 + Period_8 + Period_9 + Period_10 + Period_11 + Period_12, data=pp, family=binomial(link=probit))
summary(mod1)


# A6:
coef <- coef(mod1)

inv.logit <- function(x){
  return(exp(x)/(1+exp(x)))
  }

inv.logit(coef)  # Hazard.
## : Compute survival probabilities like done in A1.

# A7:
plot(inv.logit(coef), type="l")


