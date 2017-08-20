#Lab 4 Scratch Work
#Initialize the dataframe
setwd("C:/Users/mamro_000/Desktop/Berkeley Courses/W203 Stats/Week 13 Files/W2013_Lab4_Casey_Mamroth_Kayath")
crime <- read.csv("crime.csv")
library(car)
names(crime)
head(crime)

#Start with an investigation of crime rate
hist(crime$crmrte)
hist(log(crime$crmrte))

#Start with year, it has no missing values and is just 87 for every value, not 
#very meaningful
head(crime$year)
summary(crime$year)
length(crime$year)

#Check a few initial variables and see if anything correlates
hist(crime$crmrte)
hist(crime$polpc)

#Here we see that there no relationship between the average sentence length
#and the crime rate by county
plot(crime$avgsen, crime$crmrte)
model1 <- lm(crime$crmrte ~ crime$avgsen)
abline(model1)
summary(model1)

#Tax revenue per capita seems to be strong predictor of crime rate
lcrmrte <- log(crime$crmrte)
ltaxpc <- log(crime$taxpc)
plot(ltaxpc, lcrmrte)
model3 <- lm(lcrmrte~ltaxpc)
abline(model3)
summary(model3)

#Here we look at the same relationship but with the log transform removed
#This seems to be a stronger relationship than the log transforms
summary(crime$taxpc)
hist(log(crime$taxpc))
hist(crime$taxpc)
plot(crime$taxpc, crime$crmrte)
model2 <- lm(crime$crmrte ~ crime$taxpc)
abline(model2)
summary(model2)

#Percent minority seems to also be a poor predictor
hist(crime$pctmin80)
summary(crime$pctmin80)
plot(crime$pctmin80, crime$crmrte)
abline(lm(crime$crmrte~crime$pctmin80))
summary(lm(crime$crmrte~crime$pctmin80))
plot(crime$pctmin80, log(crime$crmrte))

#Check Percent male
hist(crime$pctymle)
plot(crime$pctymle, crime$crmrte)
abline(lm(crime$crmrte~crime$pctymle))
summary(lm(crime$crmrte~crime$pctymle))

#Looking police per capita, polpc
hist(crime$polpc)
plot(crime$polpc, crime$crmrte)
abline(lm(crime$crmrte~crime$polpc))

model_new <- lm(crime$crmrte~crime$taxpc+crime$polpc+crime$density)
model_new1 <- lm(crime$crmrte~crime$polpc+crime$density)
summary(model_new)
summary(model_new1)
plot(crime$taxpc, crime$density)
scatterPlot(crime$taxpc, crime$density)

model_new2 <- lm(crime$crmrte~crime$density+crime$pctymle)
summary(model_new2)
summary(lm(crime$crmrte~crime$density))
test1 <- lm(crime$density~crime$pctymle)
summary(test1)


#Check manufactoring wages
hist(crime$wmfg)
plot(crime$wmfg, crime$crmrte)
abline(lm(crime$crmrte~crime$wmfg))
summary(lm(crime$crmrte~crime$wmfg))

#Manufacturing wages vs services
plot(crime$wmfg, crime$wser)
summary(lm(crime$wmfg~crime$wser))

summary(lm(crime$crmrte~crime$wser))
plot(crime$wser, crime$crmrte)

hist(crime$wser)
summary(crime$wser)
head(crime$wser)
sort(crime$wser, decreasing = TRUE)

#Crime rate with each type of wage data
plot(crime$wmfg, crime$crmrte)
abline(lm(crime$crmrte~crime$wmfg))
summary(lm(crime$crmrte~crime$wmfg))

plot(crime$wser, crime$crmrte)
abline(lm(crime$crmrte~crime$wser))
summary(lm(crime$crmrte~crime$wser))

plot(crime$wtrd, crime$crmrte)
abline(lm(crime$crmrte~crime$wtrd))
summary(lm(crime$crmrte~crime$wtrd))

plot(crime$wfed, crime$crmrte)
abline(lm(crime$crmrte~crime$wfed))
summary(lm(crime$crmrte~crime$wfed))

model_v1 <- lm(crime$crmrte~crime$wfed+crime$density+crime$pctymle)
summary(model_v1)

wage <- rowMeans(cbind(crime$wcon, crime$wtuc, crime$wtrd,
               crime$wfir, crime$wser, crime$wmfg,
               crime$wfed, crime$wsta, crime$wloc), na.rm=TRUE)
names(crime)
hist(wage)
summary(lm(crime$crmrte~wage))

summary(lm(crime$crmrte~wage+crime$density+crime$pctymle))

scatterplotMatrix(~crime$density+crime$polpc+crime$taxpc)
cor(crime$polpc, crime$density)
cor(crime$polpc, crime$taxpc)
cor(crime$density, crime$taxpc)

scatterplotMatrix(~crime$prbarr+crime$prbconv+crime$prbpris)
cor(crime$prbarr, crime$prbconv)
cor(crime$prbconv, crime$prbpris)
cor(crime$prbpris, crime$prbarr)
scatterplotMatrix(~crime$crmrte+crime$prbarr+crime$prbconv+crime$prbpris)


for(i in c(5:23)){
  dat <- crime[i]
  model_i <- lm(crime$crmrte~dat)
  print(summary(model_i)$r.sqaured)
}

lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$prbarr+
               +crime$prbconv+crime$polpc+crime$pctmin80+x))$adj.r.squared)


model_test <- lm(crime$crmrte~crime$density+crime$prbarr+
                 +crime$prbconv+crime$polpc+crime$pctmin80)
summary(model_test)
coefficients(model_test)

model_mich <- lm(crime$crmrte~crime$density+crime$pctmin80+crime$pctymle)
summary(model_mich)

hist(crime$mix)
hist(log(crime$mix))

