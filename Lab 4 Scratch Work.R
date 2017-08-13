#Lab 4 Scratch Work
#Initialize the dataframe
setwd("C:/Users/mamro_000/Desktop/Berkeley Courses/W203 Stats/Week 13 Files/W2013_Lab4_Casey_Mamroth_Kayath")
crime <- read.csv("crime.csv")
names(crime)

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
plot(crime$taxpc, crime$crmrte)
model2 <- lm(crime$crmrte ~ crime$taxpc)
abline(model2)
summary(model2)


hist(crime$pctmin80)
summary(crime$pctmin80)
