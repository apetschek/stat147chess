
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


#install.packages('gam')
library(gam)

#train.na$lapsed = train.na$lapsed - 1
mod = gam(lapsed ~ s(age)+s(nregions)+s(memmonths)+s(r1)+s(r2)+ s(r3)+s(r.quick)+s(mean.r)+s(allgames1yr.SQ)+s(allgames5yr.SQ)+s(fastevents.SQ)+s(medevents.SQ)+s(slowevents.SQ)+s(nfloor.SQ)+s(r3quickdelta)+s(r3r2delta)+s(r2r1delta)+ sex +region + memtype +mem_mag1 + mem_mag2 + hasemail + extra + r.intl + r.intl.na + r.quick.na + r3.na + r2.na + r1.na + age.na + r3r2delta.na + r2r1delta.na + mean.r.na + r3quickdelta.na, family = binomial, data = train.na)







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
# Handle missing values
valid.na = na.convert.mean(valid)
# Compute square roots
valid.na$allgames1yr.SQ = sqrt(valid.na$allgames1yr)
valid.na$allgames5yr.SQ = sqrt(valid.na$allgames5yr)
valid.na$fastevents.SQ = sqrt(valid.na$fastevents)
valid.na$medevents.SQ = sqrt(valid.na$medevents)
valid.na$slowevents.SQ = sqrt(valid.na$slowevents)
valid.na$nfloor.SQ =  sqrt(valid.na$nfloor)

summary(valid.na)

valid.preds = predict(mod.glm, newdata = valid.na, type = "response" )
discrepancy(valid$lapsed,valid.preds)

test=read.csv("/Users/andrewpetschek/Dropbox/Stat 149/Final Project/test.csv")
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
test.na = na.convert.mean(test)

test.na$allgames1yr.SQ = sqrt(test.na$allgames1yr)
test.na$allgames5yr.SQ = sqrt(test.na$allgames5yr)
test.na$fastevents.SQ = sqrt(test.na$fastevents)
test.na$medevents.SQ = sqrt(test.na$medevents)
test.na$slowevents.SQ = sqrt(test.na$slowevents)
test.na$nfloor.SQ =  sqrt(test.na$nfloor)

summary(test.na)

preds = predict(mod.glm, newdata = test.na, type = "response" )

prediction = write.csv(x = preds,file = "prediction5.csv")

