
# Read in the data
raw.train=read.csv("train.csv")

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

factor.mask = c(3,4,6,8,9,10,19,15,16,28,29,30,31,32,33,34,35,36,37)
train.na[,-factor.mask]
train.na.means = apply(train.na[,-factor.mask],2,mean)
train.na.means



# Process Validation Data
# mark categorical data as factor
valid$lapsed = as.numeric(valid$lapsed) - 1
valid$sex = factor(valid$sex)
valid$region = as.factor(valid$region)
valid$memtype = as.factor(valid$memtype)
valid$mem_mag1 = as.factor(valid$mem_mag1)
valid$mem_mag2 = as.factor(valid$mem_mag2)
valid$hasmail = as.factor(valid$hasemail)
valid$extra = as.factor(valid$extra)
valid$intl = as.factor(valid$intl)
# Handle missing values by filling the missing values with the mean
# from the training set and then creating a new indicator variables
# showing that there was a missing value
valid.na = na.convert.mean(valid)
names(valid.na)
for(i in 1:nrow(valid.na)){
  if(valid.na[i,]$age.na == 0){
    valid.na$age = train.na.means['age']
  }
  if(valid.na[i,]$r1.na == 0){
    valid.na$r1 = train.na.means['r1']
  }
  if(valid.na[i,]$r2.na == 0){
    valid.na$r2 = train.na.means['r2']
  }
  if(valid.na[i,]$r3.na == 0){
    valid.na$r3 = train.na.means['r3']
  }
  if(valid.na[i,]$r.quick.na == 0){
    valid.na$r.quick = train.na.means['r.quick']
  }
  if(valid.na[i,]$r.intl.na == 0){
    valid.na$r.intl = train.na.means['r.intl']
  }
  if(valid.na[i,]$r3r2delta.na == 0){
    valid.na$r3r2delta = train.na.means['r3r2delta']
  }
  if(valid.na[i,]$r2r1delta.na == 0){
    valid.na$r2r1delta = train.na.means['r2r1delta']
  }
  if(valid.na[i,]$r3quickdelta.na == 0){
    valid.na$r3r2delta = train.na.means['r3quickdelta']
  }
  if(valid.na[i,]$mean.r.na == 0){
    valid.na$mean.r = train.na.means['mean.r']
  }
}
# Compute square roots
valid.na$allgames1yr.SQ = sqrt(valid.na$allgames1yr)
valid.na$allgames5yr.SQ = sqrt(valid.na$allgames5yr)
valid.na$fastevents.SQ = sqrt(valid.na$fastevents)
valid.na$medevents.SQ = sqrt(valid.na$medevents)
valid.na$slowevents.SQ = sqrt(valid.na$slowevents)
valid.na$nfloor.SQ =  sqrt(valid.na$nfloor)

### MODEL BUILDING ###

mod.glm = glm(lapsed ~.,family=binomial,data=train.na)

#install.packages('gam')
library(gam)

#train.na$lapsed = train.na$lapsed - 1
mod = gam(lapsed ~ s(age)+s(nregions)+s(memmonths)+s(r1)+s(r2)+ s(r3)+s(r.quick)+s(mean.r)+s(allgames1yr.SQ)+s(allgames5yr.SQ)+s(fastevents.SQ)+s(medevents.SQ)+s(slowevents.SQ)+s(nfloor.SQ)+s(r3quickdelta)+s(r3r2delta)+s(r2r1delta)+ sex +region + memtype +mem_mag1 + mem_mag2 + hasemail + extra + r.intl + r.intl.na + r.quick.na + r3.na + r2.na + r1.na + age.na + r3r2delta.na + r2r1delta.na + mean.r.na + r3quickdelta.na, family = binomial, data = train.na)


# train a neural network
library(nnet)

mod.nnet = nnet(lapsed~., size=i, linout=T,data = train.na) 
valid.preds = predict(mod.nnet, newdata = valid.na, type = "raw" )
print(discrepancy(valid$lapsed,valid.preds))
mod.nnet = nnet(lapsed~., size=3, data = train.na)
plot(fitted(mod.nnet),residuals(mod.nnet))
fitted(mod.nnet)

summary(mod)
# anova test with linear terms only
drop1(mod, test = "Chisq")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")
mod.glm <- update(mod.glm, .~. - )
drop1(mod.glm, test = "LRT")

# Plot Residuals (Jack Knife and Binned Avg.) - looking for non linearities
sum((residuals(mod, type="deviance"))^2)
par(mfrow = c(1,2))
plot(mod$fitted.values, residuals(mod, type="deviance"),
     xlab="fitted probabilities",
     ylab="deviance residuals")

library(arm)
binnedplot(mod$fitted.values, rstudent(mod),
           xlab="Averaged fitted probabilities",
           ylab="Averaged jackknifed residuals")

# Cook's distance - want < 1
plot(cooks.distance(mod.glm), type="h",
     xlab="Observation index",
     ylab="Cook's distances")

summary(mod.glm)

discrepancy = function (y,phat) {
  return (-mean(y * log(phat) + (1 - y)*log(1-phat)))
}

# Predict on Validation Set
valid.preds = predict(mod.nnet, newdata = valid.na, type = "raw" )
discrepancy(valid$lapsed,valid.preds)

test=read.csv("test.csv")
test

# Process Test Data
# mark categorical data as factor
test$sex = factor(test$sex)
test$region = as.factor(test$region)
test$memtype = as.factor(test$memtype)
test$mem_mag1 = as.factor(test$mem_mag1)
test$mem_mag2 = as.factor(test$mem_mag2)
test$hasmail = as.factor(test$hasemail)
test$extra = as.factor(test$extra)
test$intl = as.factor(test$intl)
# Handle missing values by filling the missing values with the mean
# from the training set and then creating a new indicator variables
# showing that there was a missing value
test.na = na.convert.mean(test)
names(test.na)
for(i in 1:nrow(test.na)){
  if(test.na[i,]$age.na == 0){
    test.na$age = train.na.means['age']
  }
  if(test.na[i,]$r1.na == 0){
    test.na$r1 = train.na.means['r1']
  }
  if(test.na[i,]$r2.na == 0){
    test.na$r2 = train.na.means['r2']
  }
  if(test.na[i,]$r3.na == 0){
    test.na$r3 = train.na.means['r3']
  }
  if(test.na[i,]$r.quick.na == 0){
    test.na$r.quick = train.na.means['r.quick']
  }
  if(test.na[i,]$r.intl.na == 0){
    test.na$r.intl = train.na.means['r.intl']
  }
  if(test.na[i,]$r3r2delta.na == 0){
    test.na$r3r2delta = train.na.means['r3r2delta']
  }
  if(test.na[i,]$r2r1delta.na == 0){
    test.na$r2r1delta = train.na.means['r2r1delta']
  }
  if(test.na[i,]$r3quickdelta.na == 0){
    test.na$r3r2delta = train.na.means['r3quickdelta']
  }
  if(test.na[i,]$mean.r.na == 0){
    test.na$mean.r = train.na.means['mean.r']
  }
}
# Compute square roots
test.na$allgames1yr.SQ = sqrt(test.na$allgames1yr)
test.na$allgames5yr.SQ = sqrt(test.na$allgames5yr)
test.na$fastevents.SQ = sqrt(test.na$fastevents)
test.na$medevents.SQ = sqrt(test.na$medevents)
test.na$slowevents.SQ = sqrt(test.na$slowevents)
test.na$nfloor.SQ =  sqrt(test.na$nfloor)

summary(test.na)

#Predict on test set
preds = predict(mod.glm, newdata = test.na, type = "response" )

#Save predictions
prediction = write.csv(x = preds,file = "prediction5.csv")

