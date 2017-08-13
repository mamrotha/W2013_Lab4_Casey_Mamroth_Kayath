#Lab 4 Scratch Work
#Initialize the dataframe
setwd("C:/Users/mamro_000/Desktop/Berkeley Courses/W203 Stats/Week 13 Files/W2013_Lab4_Casey_Mamroth_Kayath")
crime <- read.csv("crime.csv")
names(crime)

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
plot(crime$pctmin80, log(crime$crmrte))
