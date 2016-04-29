# Read in the data
raw.train=read.csv("train.csv")
test=read.csv("test.csv")

raw.train$r3r2delta = raw.train$r3- raw.train$r2
raw.train$r2r1delta = raw.train$r2 - raw.train$r1
raw.train$r3quickdelta = raw.train$r3 - raw.train$r.quick

mean.r = rep(0,nrow(raw.train))
for(i in 1:nrow(raw.train)){
  #only take avg over non-NA rankings
  mask = is.na(raw.train[i,11:13])
  mean.r[i] = mean(raw.train[i,11:13][!mask])
} 
mean.r
raw.train$mean.r = mean.r
raw.train

#train = raw.train
# split into train/valid
set.seed(2)
# 80% training, 20% valid
raw.train.num = floor(.8 * nrow(raw.train))
train.mask = sample(1:nrow(raw.train),size=raw.train.num)
train = raw.train[train.mask,]
valid = raw.train[-train.mask,]

na.convert.mean = function (frame) 
{
  vars <- names(frame)
  if (!is.null(resp <- attr(attr(frame, "terms"), "response"))) {
    vars <- vars[-resp]
    x <- frame[[resp]]
    pos <- is.na(x)
    if (any(pos)) {
      frame <- frame[!pos, , drop = FALSE]
      warning(paste(sum(pos), "observations omitted due to missing values in the response"))
    }
  }
  for (j in vars) {  #j is variable names
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {   # factors
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {   # matrices
        ats <- attributes(x)
        x.na <- 1*pos
        #               x[pos] <- 0
        w <- !pos
        n <- nrow(x)
        TT <- array(1, c(1, n))
        xbar <- (TT %*% x)/(TT %*% w)
        xbar <- t(TT) %*% xbar
        x[pos] <- xbar[pos]
        attributes(x) <- ats
        attributes(x.na) <- ats
        dimnames(x.na)[[2]]=paste(dimnames(x)[[2]],".na",sep='')
        frame[[paste(j,".na",sep='')]] <- x.na 
      } else {   # ordinary numerical vector
        ats <- attributes(x)
        x[pos] <- mean(x[!pos])
        #               x[pos] <- 0
        x.na <- 1*pos
        frame[[paste(j,".na",sep='')]] <- x.na 
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}

# mark categorical data as factor
train$lapsed = as.numeric(train$lapsed) - 1
train$sex = factor(train$sex)
train$region = as.factor(train$region)
train$memtype = as.factor(train$memtype)
train$mem_mag1 = as.factor(train$mem_mag1)
train$mem_mag2 = as.factor(train$mem_mag2)
train$hasemail = as.factor(train$hasemail)
train$extra = as.factor(train$extra)
train$intl = as.factor(train$intl)

train.na = na.convert.mean(train)

# Seem to be some extreme values here, can take sqrt of data
train.na$allgames1yr.SQ = sqrt(train.na$allgames1yr)
train.na$allgames5yr.SQ = sqrt(train.na$allgames5yr)
train.na$fastevents.SQ = sqrt(train.na$fastevents)
train.na$medevents.SQ = sqrt(train.na$medevents)
train.na$slowevents.SQ = sqrt(train.na$slowevents)
train.na$nfloor.SQ =  sqrt(train.na$nfloor)

# calculate means of training
factor.mask = c(3,4,6,8,9,10,19,15,16,28,29,30,31,32,33,34,35,36,37)
train.na[,-factor.mask]
train.na.means = apply(train.na[,-factor.mask],2,mean)
train.na.means


#install.packages('gam')
library(gam)

# fit GAMs / smoothers and plot to get idea of how to transform data in our glm
names(train.na)
mod1 = gam(lapsed~s(age),family = binomial, data = train.na)
mod2 = gam(lapsed~s(nregions),family = binomial, data = train.na)
mod3 = gam(lapsed~s(memmonths),family = binomial, data = train.na)
mod4 = gam(lapsed~s(r1),family = binomial, data = train.na)
mod5 = gam(lapsed~s(r2),family = binomial, data = train.na)
mod6 = gam(lapsed~s(r3),family = binomial, data = train.na)
mod7 = gam(lapsed~s(r.quick),family = binomial, data = train.na)
mod8 = gam(lapsed~s(r.intl),family = binomial, data = train.na)
mod9 = gam(lapsed~s(allgames1yr.SQ),family = binomial, data = train.na)
mod10 = gam(lapsed~s(allgames5yr.SQ),family = binomial, data = train.na)
mod11 = gam(lapsed~s(fastevents.SQ),family = binomial, data = train.na)
mod12 = gam(lapsed~s(medevents.SQ),family = binomial, data = train.na)
mod13 = gam(lapsed~s(slowevents.SQ),family = binomial, data = train.na)
mod14 = gam(lapsed~s(mean.r),family = binomial, data = train.na)
mod15 = gam(lapsed~s(r3r2delta),family = binomial, data = train.na)
mod16 = gam(lapsed~s(r2r1delta),family = binomial, data = train.na)
mod17 = gam(lapsed~s(r3quickdelta),family = binomial, data = train.na)

# plot
par(mfrow=c(4,4))
plot(mod1,residuals = TRUE,se= TRUE)
plot(mod2,residuals = TRUE,se= TRUE)
plot(mod3,residuals = TRUE,se= TRUE)
plot(mod4,residuals = TRUE,se= TRUE)
plot(mod5,residuals = TRUE,se= TRUE)
plot(mod6,residuals = TRUE,se= TRUE)
plot(mod7,residuals = TRUE,se= TRUE)
plot(mod8,residuals = TRUE,se= TRUE)
plot(mod9,residuals = TRUE,se= TRUE)
plot(mod10,residuals = TRUE,se= TRUE)
plot(mod11,residuals = TRUE,se= TRUE)
plot(mod12,residuals = TRUE,se= TRUE)
plot(mod13,residuals = TRUE,se= TRUE)
plot(mod14,residuals = TRUE,se= TRUE)
plot(mod15,residuals = TRUE,se= TRUE)
plot(mod16,residuals = TRUE,se= TRUE)

par(mfrow=c(1,1))
plot(mod17,residuals = TRUE,se= TRUE)

# add in square and cubic terms based on plots above
train.na$r3quickdelta.sq = (train.na$r3quickdelta - mean(train.na$r3quickdelta))^2
train.na$r.intl.sq = (train.na$r.intl - mean(train.na$r.intl))^2
train.na$r2r1delta.sq = (train.na$r2r1delta - mean(train.na$r2r1delta))^2
train.na$r3r2delta.sq = (train.na$r3r2delta - mean(train.na$r3r2delta))^2
train.na$age.sq = (train.na$age - mean(train.na$age))^2
train.na$age.cu = (train.na$age - mean(train.na$age))^3
names(train.na)

# fit all single interactions, and drop 1 until best single interaction model found

mod = glm(lapsed ~ .,family = binomial , data = train.na)
mod = update(mod,.~. - fastevents - slowevents - medevents - allgames1yr - allgames5yr - nfloor - r3r2delta.na - r2r1delta.na - mean.r.na) # remove non sqrt terms
mod = update(mod,.~.- r3)
mod = update(mod,.~.- slowevents.SQ)
mod = update(mod,.~.- r2)
mod = update(mod,.~.- nregions - r.intl - r.quick.na - r3quickdelta.na - nfloor.SQ)
mod = update(mod,.~.- r.intl.sq)
mod = update(mod,.~.- r2r1delta.sq)
mod = update(mod,.~.- r2r1delta)
mod = update(mod,.~.- age.na)
mod = update(mod,.~.- allgames5yr.SQ)
mod = update(mod,.~.- r2.na)
drop1(mod,test="Chi")
summary(mod) #### OK ALL SIGNIFICANT #lapsed ~ age + sex + region + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r.quick + extra + intl + r3r2delta + r3quickdelta + mean.r + r1.na + r3.na + r.intl.na + allgames1yr.SQ + fastevents.SQ + medevents.SQ + r3quickdelta.sq + r3r2delta.sq + age.sq + age.cu


# add interactions

mod1 = update(mod, .~. + allgames1yr.SQ:age.sq)
mod2 = update(mod, .~. + allgames1yr.SQ:age.cu)
mod3 = update(mod, .~. + allgames1yr.SQ:memmonths)
mod4 = update(mod, .~. + allgames1yr.SQ:memtype)
mod5 = update(mod, .~. + allgames1yr.SQ:mem_mag1)
mod6 = update(mod, .~. + allgames1yr.SQ:hasemail)
mod7 = update(mod, .~. + age.sq:age.sq)
mod8 = update(mod, .~. + age.sq:age.cu)
mod9 = update(mod, .~. + age.sq:memmonths)
mod10 = update(mod, .~. + age.sq:memtype)
mod11 = update(mod, .~. + age.sq:mem_mag1)
mod12 = update(mod, .~. + age.sq:hasemail)
mod13 = update(mod, .~. + hasemail:memmonths)
mod14 = update(mod, .~. + hasemail:memtype)
mod15 = update(mod, .~. + hasemail:mem_mag1)
mod16 = update(mod, .~. + region:age)
mod17 = update(mod, .~. + region:allgames1yr.SQ)
mod18 = update(mod, .~. + region:hasemail)
# 
# 
anova(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,test="Chi")
# 
mod1 = update(mod8, .~. + allgames1yr.SQ:age.sq)
mod2 = update(mod8, .~. + allgames1yr.SQ:age.cu)
mod3 = update(mod8, .~. + allgames1yr.SQ:memmonths)
mod4 = update(mod8, .~. + allgames1yr.SQ:memtype)
mod5 = update(mod8, .~. + allgames1yr.SQ:mem_mag1)
mod6 = update(mod8, .~. + allgames1yr.SQ:hasemail)
mod7 = update(mod8, .~. + age.sq:age.sq)
#mod8 = update(mod8, .~. + age.sq:age.cu)
mod9 = update(mod8, .~. + age.sq:memmonths)
mod10 = update(mod8, .~. + age.sq:memtype)
mod11 = update(mod8, .~. + age.sq:mem_mag1)
mod12 = update(mod8, .~. + age.sq:hasemail)
mod13 = update(mod8, .~. + hasemail:memmonths)
mod14 = update(mod8, .~. + hasemail:memtype)
mod15 = update(mod8, .~. + hasemail:mem_mag1)
mod16 = update(mod8, .~. + region:age)
mod17 = update(mod8, .~. + region:allgames1yr.SQ)
mod18 = update(mod8, .~. + region:hasemail)
# 
anova(mod8,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,test="Chi")

#mod1 = update(mod1, .~. + allgames1yr.SQ:age.sq)
mod2 = update(mod1, .~. + allgames1yr.SQ:age.cu)
mod3 = update(mod1, .~. + allgames1yr.SQ:memmonths)
mod4 = update(mod1, .~. + allgames1yr.SQ:memtype)
mod5 = update(mod1, .~. + allgames1yr.SQ:mem_mag1)
mod6 = update(mod1, .~. + allgames1yr.SQ:hasemail)
mod7 = update(mod1, .~. + age.sq:age.sq)
#mod8 = update(mod1, .~. + age.sq:age.cu)
mod9 = update(mod1, .~. + age.sq:memmonths)
mod10 = update(mod1, .~. + age.sq:memtype)
mod11 = update(mod1, .~. + age.sq:mem_mag1)
mod12 = update(mod1, .~. + age.sq:hasemail)
mod13 = update(mod1, .~. + hasemail:memmonths)
mod14 = update(mod1, .~. + hasemail:memtype)
mod15 = update(mod1, .~. + hasemail:mem_mag1)
mod16 = update(mod1, .~. + region:age)
mod17 = update(mod1, .~. + region:allgames1yr.SQ)
mod18 = update(mod1, .~. + region:hasemail)

anova(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18,test="Chi")

#mod1 = update(mod16, .~. + allgames1yr.SQ:age.sq)
mod2 = update(mod16, .~. + allgames1yr.SQ:age.cu)
mod3 = update(mod16, .~. + allgames1yr.SQ:memmonths)
mod4 = update(mod16, .~. + allgames1yr.SQ:memtype)
mod5 = update(mod16, .~. + allgames1yr.SQ:mem_mag1)
mod6 = update(mod16, .~. + allgames1yr.SQ:hasemail)
mod7 = update(mod16, .~. + age.sq:age.sq)
#mod8 = update(mod16, .~. + age.sq:age.cu)
mod9 = update(mod16, .~. + age.sq:memmonths)
mod10 = update(mod16, .~. + age.sq:memtype)
mod11 = update(mod16, .~. + age.sq:mem_mag1)
mod12 = update(mod16, .~. + age.sq:hasemail)
mod13 = update(mod16, .~. + hasemail:memmonths)
mod14 = update(mod16, .~. + hasemail:memtype)
mod15 = update(mod16, .~. + hasemail:mem_mag1)
#mod16 = update(mod16, .~. + region:age)
mod17 = update(mod16, .~. + region:allgames1yr.SQ)
mod18 = update(mod16, .~. + region:hasemail)

anova(mod16,mod2,mod3,mod4,mod5,mod6,mod7,mod9,mod10,mod11,mod12,mod13,mod14,mod15,mod17,mod18,test="Chi")
 
modB = mod10

summary(modB) # glm(formula = lapsed ~ age + sex + region + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r.quick + extra + intl + r3r2delta + r3quickdelta + mean.r + r1.na + r3.na + r.intl.na + allgames1yr.SQ + fastevents.SQ + medevents.SQ + r3quickdelta.sq + r3r2delta.sq + age.sq + age.cu + age.sq:age.cu + allgames1yr.SQ:age.sq + age:region + memtype:age.sq, family = binomial, data = train.na)

#modBfull = glm(formula = lapsed ~ age + sex + region + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r.quick + extra + intl + r3r2delta + r3quickdelta + mean.r + r1.na + r3.na + r.intl.na + allgames1yr.SQ + fastevents.SQ + medevents.SQ + r3quickdelta.sq + r3r2delta.sq + age.sq + age.cu + age.sq:age.cu + allgames1yr.SQ:age.sq + age:region + memtype:age.sq, family = binomial, data = train.na)

# Plot Residuals (Jack Knife and Binned Avg.) - looking for non linearities
sum((residuals(modBfull, type="deviance"))^2)
par(mfrow = c(1,1))
plot(modBfull$fitted.values, residuals(modBfull, type="deviance"),
     xlab="fitted probabilities",
     ylab="deviance residuals")

library(arm)
binnedplot(modBfull$fitted.values, rstudent(modBfull),
           xlab="Averaged fitted probabilities",
           ylab="Averaged jackknifed residuals")

# Cook's distance - want < 1
plot(cooks.distance(modBfull), type="h",
     xlab="Observation index",
     ylab="Cook's distances")

which.max(cooks.distance(modB))
train.na[7408,]

# Process Validation Data
# mark categorical data as factor
valid$lapsed = as.numeric(valid$lapsed) - 1
valid$sex = factor(valid$sex)
valid$region = as.factor(valid$region)
valid$memtype = as.factor(valid$memtype)
valid$mem_mag1 = as.factor(valid$mem_mag1)
valid$mem_mag2 = as.factor(valid$mem_mag2)
valid$hasemail = as.factor(valid$hasemail)
valid$extra = as.factor(valid$extra)
valid$intl = as.factor(valid$intl)
# Handle missing values by filling the missing values with the mean
# from the training set and then creating a new indicator variables
# showing that there was a missing value
valid.na = na.convert.mean(valid)
names(valid.na)
for(i in 1:nrow(valid.na)){
  if(valid.na[i,]$age.na == 1){
    valid.na[i,]$age = train.na.means['age']
  }
  if(valid.na[i,]$r1.na == 1){
    valid.na[i,]$r1 = train.na.means['r1']
  }
  if(valid.na[i,]$r2.na == 1){
    valid.na[i,]$r2 = train.na.means['r2']
  }
  if(valid.na[i,]$r3.na == 1){
    valid.na[i,]$r3 = train.na.means['r3']
  }
  if(valid.na[i,]$r.quick.na == 1){
    valid.na[i,]$r.quick = train.na.means['r.quick']
  }
  if(valid.na[i,]$r.intl.na == 1){
    valid.na[i,]$r.intl = train.na.means['r.intl']
  }
  if(valid.na[i,]$r3r2delta.na == 1){
    valid.na[i,]$r3r2delta = train.na.means['r3r2delta']
  }
  if(valid.na[i,]$r2r1delta.na == 1){
    valid.na[i,]$r2r1delta = train.na.means['r2r1delta']
  }
  if(valid.na[i,]$r3quickdelta.na == 1){
    valid.na[i,]$r3r2delta = train.na.means['r3quickdelta']
  }
  if(valid.na[i,]$mean.r.na == 1){
    valid.na[i,]$mean.r = train.na.means['mean.r']
  }
}

# Compute square roots
valid.na$allgames1yr.SQ = sqrt(valid.na$allgames1yr)
valid.na$allgames5yr.SQ = sqrt(valid.na$allgames5yr)
valid.na$fastevents.SQ = sqrt(valid.na$fastevents)
valid.na$medevents.SQ = sqrt(valid.na$medevents)
valid.na$slowevents.SQ = sqrt(valid.na$slowevents)
valid.na$nfloor.SQ =  sqrt(valid.na$nfloor)

valid.na$r3quickdelta.sq = (valid.na$r3quickdelta - mean(valid.na$r3quickdelta))^2
valid.na$r.intl.sq = (valid.na$r.intl - mean(valid.na$r.intl))^2
valid.na$r2r1delta.sq = (valid.na$r2r1delta - mean(valid.na$r2r1delta))^2
valid.na$r3r2delta.sq = (valid.na$r3r2delta - mean(valid.na$r3r2delta))^2
valid.na$age.sq = (valid.na$age - mean(valid.na$age))^2
valid.na$age.cu = (valid.na$age - mean(valid.na$age))^3


# Process Test Data
# mark categorical data as factor
test$sex = factor(test$sex)
test$region = as.factor(test$region)
test$memtype = as.factor(test$memtype)
test$mem_mag1 = as.factor(test$mem_mag1)
test$mem_mag2 = as.factor(test$mem_mag2)
test$hasemail = as.factor(test$hasemail)
test$extra = as.factor(test$extra)
test$intl = as.factor(test$intl)

test$r3r2delta = test$r3- test$r2
test$r2r1delta = test$r2 - test$r1
test$r3quickdelta = test$r3 - test$r.quick

mean.r = rep(0,nrow(test))
for(i in 1:nrow(test)){
  #only take avg over non-NA rankings
  mask = is.na(test[i,11:13])
  mean.r[i] = mean(test[i,11:13][!mask])
} 
mean.r
test$mean.r = mean.r
# Handle missing values by filling the missing values with the mean
# from the training set and then creating a new indicator variables
# showing that there was a missing value
test.na = na.convert.mean(test)
names(test.na)
for(i in 1:nrow(test.na)){
  if(test.na[i,]$age.na == 1){
    test.na[i,]$age = train.na.means['age']
  }
  if(test.na[i,]$r1.na == 1){
    test.na[i,]$r1 = train.na.means['r1']
  }
  if(test.na[i,]$r2.na == 1){
    test.na[i,]$r2 = train.na.means['r2']
  }
  if(test.na[i,]$r3.na == 0){
    test.na[i,]$r3 = train.na.means['r3']
  }
  if(test.na[i,]$r.quick.na == 0){
    test.na[i,]$r.quick = train.na.means['r.quick']
  }
  if(test.na[i,]$r.intl.na == 0){
    test.na[i,]$r.intl = train.na.means['r.intl']
  }
  if(test.na[i,]$r3r2delta.na == 0){
    test.na[i,]$r3r2delta = train.na.means['r3r2delta']
  }
  if(test.na[i,]$r2r1delta.na == 0){
    test.na[i,]$r2r1delta = train.na.means['r2r1delta']
  }
  if(test.na[i,]$r3quickdelta.na == 0){
    test.na[i,]$r3r2delta = train.na.means['r3quickdelta']
  }
  if(test.na[i,]$mean.r.na == 0){
    test.na[i,]$mean.r = train.na.means['mean.r']
  }
}


# Compute square roots
test.na$allgames1yr.SQ = sqrt(test.na$allgames1yr)
test.na$allgames5yr.SQ = sqrt(test.na$allgames5yr)
test.na$fastevents.SQ = sqrt(test.na$fastevents)
test.na$medevents.SQ = sqrt(test.na$medevents)
test.na$slowevents.SQ = sqrt(test.na$slowevents)
test.na$nfloor.SQ =  sqrt(test.na$nfloor)

test.na$r3quickdelta.sq = (test.na$r3quickdelta - mean(test.na$r3quickdelta))^2
test.na$r.intl.sq = (test.na$r.intl - mean(test.na$r.intl))^2
test.na$r2r1delta.sq = (test.na$r2r1delta - mean(test.na$r2r1delta))^2
test.na$r3r2delta.sq = (test.na$r3r2delta - mean(test.na$r3r2delta))^2
test.na$age.sq = (test.na$age - mean(test.na$age))^2
test.na$age.cu = (test.na$age - mean(test.na$age))^3


discrepancy = function (y,phat) {
  return (-mean(y * log(phat) + (1 - y)*log(1-phat)))
}

#Predict on test set
preds = predict(modBfull, newdata = test.na, type = "response" )

discrepancy(valid.na$lapsed,preds)

#mod = 0.553
#modB = 0.550
prediction = write.csv(x = preds,file = "predictionsmodbfull.csv")
