# Square Root of final 5 features for better spread

# Read in the data
train=read.csv("/Users/andrewpetschek/Dropbox/Stat 149/Final Project/train.csv")
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
train$sex = factor(train$sex)
train$region = as.factor(train$region)
train$memtype = as.factor(train$memtype)
train$mem_mag1 = as.factor(train$mem_mag1)
train$mem_mag2 = as.factor(train$mem_mag2)
train$hasmail = as.factor(train$hasemail)
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


library(gam)

#train.na$lapsed = train.na$lapsed - 1
mod.glm = gam(lapsed ~ s(age:memmonths)+ s(age:r3) +s(age) + sex + region + s(nregions) + memtype + s(memmonths) + mem_mag1 + mem_mag2 + hasemail + s(r1) + s(r2) + s(r3) + s(r.quick) + extra + r.intl + s(allgames1yr) + s(allgames5yr) + s(fastevents) + s(medevents) + s(slowevents) + s(nfloor) + r.intl.na + r.quick.na + r3.na + age.na, family = binomial(link = "cloglog"),data = train.na)



summary(mod.glm)
# anova test with linear terms only
drop1(mod.glm, test = "Chisq")
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
sum((residuals(mod.glm, type="deviance"))^2)
par(mfrow = c(1,2))
plot(mod.glm$fitted.values, residuals(mod.glm, type="deviance"),
     xlab="fitted probabilities",
     ylab="deviance residuals")

library(arm)
binnedplot(mod.glm$fitted.values, rstudent(mod.glm),
           xlab="Averaged fitted probabilities",
           ylab="Averaged jackknifed residuals")

# Cook's distance - want < 1
plot(cooks.distance(mod.glm), type="h",
     xlab="Observation index",
     ylab="Cook's distances")

summary(mod.glm)

test=read.csv("/Users/andrewpetschek/Dropbox/Stat 149/Final Project/test.csv")
test

# mark categorical data as factor
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

test.na$allgames1yr.SQ = sqrt(test.na$allgames1yr)
test.na$allgames5yr.SQ = sqrt(test.na$allgames5yr)
test.na$fastevents.SQ = sqrt(test.na$fastevents)
test.na$medevents.SQ = sqrt(test.na$medevents)
test.na$slowevents.SQ = sqrt(test.na$slowevents)
test.na$nfloor.SQ =  sqrt(test.na$nfloor)

summary(test.na)

preds = predict(mod.glm, newdata = test.na, type = "response" )

prediction = write.csv(x = preds,file = "/Users/andrewpetschek/Dropbox/Stat 149/Final Project/prediction5.csv")

