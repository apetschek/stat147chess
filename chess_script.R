# Read in the data
train=read.csv("/Users/andrewpetschek/Dropbox/Stat 149/Final Project/train.csv")
summary(train)


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
train$lapsed = as.factor(train$lapsed)
#train$age
train$sex = factor(train$sex)
train$region = as.factor(train$region)
#train$nregions
train$memtype = as.factor(train$memtype)
#train$memmonths
train$mem_mag1 = as.factor(train$mem_mag1)
train$mem_mag2 = as.factor(train$mem_mag2)
train$hasmail = as.factor(train$hasemail)
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

train.na = na.convert.mean(train)


summary(train.na)
# count initial number of rows
nrow(train)

# missing data- preliminary solution: just remove all NA's (note this leaves < 1% of data)
# count number of rows to see how many dropped


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
mod = glm(lapsed ~ age + sex + region + nregions + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r2 + r3 + r.quick + extra + r.intl + allgames1yr + allgames5yr + fastevents + medevents + slowevents + nfloor + r.intl.na + r.quick.na+ r3.na +age.na ,family = binomial, data = train.na)
summary(mod)

train.na$lapsed = as.numeric(train.na$lapsed)
train.na$lapsed

#look for colinearity - doesnt work for some reason.
mod = lm(lapsed ~ age + sex + region + nregions + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r2 + r3 + r.quick + extra + r.intl + allgames1yr + allgames5yr + fastevents + medevents + slowevents + nfloor + r.intl.na + r.quick.na+ r3.na +age.na, data = train.na)

data.frame(vif = car::vif(mod))

mod2 = update(mod, . ~. - allgames5yr)

data.frame(vif = car::vif(mod2))

mod3 = update(mod2, . ~. - r3)

data.frame(vif = car::vif(mod3))


train.na$lapsed = train.na$lapsed - 1
mod.glm = glm(lapsed ~ age + sex + region + nregions + memtype + memmonths + mem_mag1 + mem_mag2 + hasemail + r1 + r2 + r.quick + extra + r.intl + allgames1yr + fastevents + medevents + slowevents + nfloor + r.intl.na + r.quick.na+ r3.na +age.na ,family = binomial, data = train.na)

# anova test with linear terms only
drop1(mod.glm, test = "LRT")

mod2.glm <- update(mod.glm, .~. - r.intl)
drop1(mod2.glm,test = "LRT")

mod3.glm <- update(mod2.glm, .~. - r.quick)
drop1(mod3.glm,test = "LRT")

mod4.glm <- update(mod3.glm, .~. - slowevents)
drop1(mod4.glm,test="LRT")

mod5.glm = update(mod4.glm,.~. - r.intl.na)
drop1(mod5.glm,test="LRT")

mod6.glm = update(mod5.glm,.~. - nregions)
drop1(mod6.glm ,test="LRT")

mod7.glm = update(mod6.glm,.~. - r.quick.na)
drop1(mod7.glm ,test="LRT")

# Plot Residuals (Jack Knife and Binned Avg.) - looking for non linearities
sum((residuals(mod7.glm, type="deviance"))^2)
par(mfrow = c(1,2))
plot(mod7.glm$fitted.values, residuals(mod7.glm, type="deviance"),
     xlab="fitted probabilities",
     ylab="deviance residuals")

library(arm)
binnedplot(mod7.glm$fitted.values, rstudent(mod7.glm),
           xlab="Averaged fitted probabilities",
           ylab="Averaged jackknifed residuals")

# Cook's distance - want < 1
plot(cooks.distance(mod7.glm), type="h",
     xlab="Observation index",
     ylab="Cook's distances")

summary(mod7.glm)

test=read.csv("/Users/andrewpetschek/Dropbox/Stat 149/Final Project/test.csv")
test

# mark categorical data as factor
test$lapsed = as.factor(test$lapsed)
#train$age
test$sex = factor(test$sex)
test$region = as.factor(test$region)
#train$nregions
test$memtype = as.factor(test$memtype)
#train$memmonths
test$mem_mag1 = as.factor(test$mem_mag1)
test$mem_mag2 = as.factor(test$mem_mag2)
test$hasmail = as.factor(test$hasemail)
#train$r1
#train$r2
#train$r3
#train$r.quick
test$extra = as.factor(test$extra)
test$intl = as.factor(test$intl)
#train$r.intl
#train$allames1yr
#train$allgames5yr
#train$fastevents
#train$medevents
#train$slowevents
#train$nfloor

test.na = na.convert.mean(test)
summary(test.na)

preds = predict(mod7.glm, newdata = test.na, type = "response" )
preds[1:10]

prediction = write.csv(x = preds,file = "/Users/andrewpetschek/Dropbox/Stat 149/Final Project/prediction.csv")
