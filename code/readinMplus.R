
##########################################
## Read in the model output from Mplus  ##
##########################################
library(MplusAutomation)
library(tidySEM)
library(plyr)

get.relevant <- function(a) {
  summ <- a$summaries
  summ[,c("Filename", "Parameters", "LL", "LLCorrectionFactor")]
}

#Multiple-group SEM
m1.age <- readModels("model1_age.out")
m2.age <- readModels("model2_age.out")
m3.age <- readModels("model3_age.out")
m4.age <- readModels("model4_age.out")
m5.age <- readModels("model5_age.out")

m1.childgender <- readModels("model1_childgender.out")
m2.childgender <- readModels("model2_childgender.out")
m3.childgender <- readModels("model3_childgender.out")
m4.childgender <- readModels("model4_childgender.out")
m5.childgender <- readModels("model5_childgender.out")

m1.parentgender <- readModels("model1_parentgender.out")
m2.parentgender <- readModels("model2_parentgender.out")
m3.parentgender <- readModels("model3_parentgender.out")
m4.parentgender <- readModels("model4_parentgender.out")
m5.parentgender <- readModels("model5_parentgender.out")


age.fit <- rbind.fill(get.relevant(m1.age), get.relevant(m2.age), get.relevant(m3.age), get.relevant(m4.age), get.relevant(m5.age))

age.m1m3 <- c(15, 18.0465, 1.20256, .2602)
age.m2m4 <- c(15, 22.1486, 1.11384, .1040)
age.m5m3 <- c(34, 52.8297, 1.10695, .0207)
age.m5m4 <- c(15, 32.5013, 1.06937, .0055)
age.modelcomp <- rbind(age.m1m3, age.m2m4, age.m5m3, age.m5m4)

childgender.fit <- rbind.fill(get.relevant(m1.childgender), get.relevant(m2.childgender), get.relevant(m3.childgender), get.relevant(m4.childgender), get.relevant(m5.childgender))
childgender.m1m3 <- c(5, 5.9359, 1.1914, .3125)
childgender.m2m4 <- c(5, 4.8589, 1.1512, .4333)
childgender.m5m3 <- c(12, 3.7517, 1.0795, .9875)
childgender.m5m4 <- c(5, 1.7818, 1.0405, .8784)
childgender.modelcomp <- rbind(childgender.m1m3, childgender.m2m4, childgender.m5m3, childgender.m5m4)

parentgender.fit <- rbind.fill(get.relevant(m1.parentgender), get.relevant(m2.parentgender), get.relevant(m3.parentgender), get.relevant(m4.parentgender), get.relevant(m5.parentgender))
parentgender.m1m3 <- c(5, 8.4915, 1.1979, .1311)
parentgender.m2m4 <- c(5, 21.9142, 1.0914, .0001)
parentgender.m5m3 <- c(12, 83.6085, 1.1473, .0001)
parentgender.m5m4 <- c(5, 35.4991, 1.1666, .0001)
parentgender.modelcomp <- rbind(parentgender.m1m3, parentgender.m2m4, parentgender.m5m3, parentgender.m5m4)

write.csv(cbind(age.fit, childgender.fit, parentgender.fit), "modelfit.csv")