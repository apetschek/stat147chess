# Read in the data
train=read.csv("/Users/andrewpetschek/Dropbox/Stat 149/Final Project/train.csv")
summary(train)

# get means of numerical columns where NAs exist
r1.mean = mean(na.omit(train$r1))
r2.mean = mean(na.omit(train$r2))
r3.mean = mean(na.omit(train$r3))
r.intl.mean = mean(na.omit(train$r.intl))
r.quick.mean = mean(na.omit(train$r.quick))

# mark categorical data as factor
train$lapsed = as.factor(train$lapsed)
#train$age
train$sex = factor(train$sex)
train$region = as.factor(train$region)
#train$nregions
train$memtype = as.factor(train$memtype)
#train$memmonths
train$mem_mag1 = as.factor(train$mem_mag1)
train$mem_mag2 = as.factor(train$mem_mag2)
train$hasmail = as.factor(hasemail)
#train$r1
#train$r2
#train$r3
#train$r.quick
train$extra = as.factor(train$extra)
train$intl = as.factor(train$intl)
#train$r.intl
#train$allames1yr
#train$allgames5yr
#train$fastevents
#train$medevents
#train$slowevents
#train$nfloor

summary(train)
# count initial number of rows
nrow(train)

# missing data- preliminary solution: just remove all NA's (note this leaves > 1% of data)
train = na.omit(train)
# count number of rows to see how many dropped
nrow(train)
summary(train)

# plot histograms to get sense of data
hist(train$age)
hist(train$nregions)
hist(train$memmonths)
hist(train$r1)
hist(train$r2)
hist(train$r3)
hist(train$r.quick)
hist(train$r.intl)
hist(train$allgames1yr) 
hist(train$allgames5yr) 
hist(train$fastevents)  
hist(train$medevents)   
hist(train$slowevents)  
hist(train$nfloor)      

# Seem to be some extreme values here, can take sqrt of data
#train$allames1yr.SQ = sqrt(train$allgames1yr)
#train$allgames5yr.SQ = sqrt(train$allgames5yr)
#train$fastevents.SQ = sqrt(train$fastevents)
#train$medevents.SQ = sqrt(train$medevents)
#train$slowevents.SQ = sqrt(train$slowevents)
#train$nfloor.SQ =  sqrt(train$nfloor)

# check to see if sqrt helped
#hist(train$allgames1yr.SQ) #sqrt
#hist(train$allgames5yr.SQ) #sqrt
#hist(train$fastevents.SQ)  #sqrt
#hist(train$medevents.SQ)   #sqrt
#hist(train$slowevents.SQ)  #sqrt
#hist(train$nfloor.SQ)      #sqrt

# create initial glm using all 23 variables (1 of which is our response: 'lapsed')
mod = glm(lapsed ~ age + sex + region + nregions + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r2 + r3 + r.quick + extra + r.intl + allgames1yr + allgames5yr + fastevents + medevents + slowevents + nfloor, family = binomial, data = train)
summary(mod)

#look for colinearity - doesnt work for some reason.
#mod1 <- lm(lapsed ~ age + sex + region + nregions + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r2 + r3 + r.quick + extra + r.intl + allgames1yr + allgames5yr + fastevents + medevents + slowevents + nfloor, data = train)
#data.frame(vif = car::vif(mod1))

# anova test with linear terms only
drop1(mod, test = "LRT")

mod2 <- update(mod, .~. - mem_mag2)
drop1(mod2,test = "LRT")

mod3 <- update(mod2, .~. - slowevents)
drop1(mod3,test = "LRT")

mod4 <- update(mod3, .~. - extra)
drop1(mod5,test="LRT")

mod5 = update(mod4,.~. - sex)
drop1(mod5,test="LRT")

mod6 = update(mod5,.~. - r1)
drop1(mod6 ,test="LRT")

mod7 = update(mod6,.~. - r.quick)
drop1(mod7 ,test="LRT")

# Plot Residuals (Jack Knife and Binned Avg.) - looking for non linearities
sum((residuals(mod7, type="deviance"))^2)
mod7$deviance
par(mfrow = c(1,2))
plot(mod7$fitted.values, residuals(mod7, type="deviance"),
     xlab="fitted probabilities",
     ylab="deviance residuals")

library(arm)
binnedplot(mod7$fitted.values, rstudent(mod7),
           xlab="Averaged fitted probabilities",
           ylab="Averaged jackknifed residuals")

# Cook's distance - want < 1
plot(cooks.distance(mod7), type="h",
     xlab="Observation index",
     ylab="Cook's distances")

summary(mod7)

