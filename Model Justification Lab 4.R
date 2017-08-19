setwd("C:/Users/mamro_000/Desktop/Berkeley Courses/W203 Stats/Week 13 Files/W2013_Lab4_Casey_Mamroth_Kayath")
crime <- read.csv("crime.csv")
library(car)
names(crime)

model_f <- lm(crime$crmrte~crime$density+crime$prbarr+
                   +crime$prbconv+crime$polpc+crime$pctmin80)
summary(model_f)

#I approach building the model by building it one piece at a time.  Start with
#the most highly correlated variable to use as the first independant variable
#then continuing adding additional independant variables that add the most value.
#In this case, density is by far the most well correlated variable and we start
#there.
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+x))$adj.r.squared)

#From here the variable pctmin80 adds to most predictive power to the model. 
#Also important to note is that the western counties apparently have a higher
#minority makeup and therefore the variable west loses most of it's value once
#pctmin80 is included. 
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$pctmin80+x))$adj.r.squared)

#Next prbconv adds the most value, interestingly, only slightly more than taxpc
#Also important to note is that prbconv has very low correlation with everything
#besides crime rate so it will almost always add value to a model.
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$pctmin80+crime$prbconv
             +x))$adj.r.squared)

#Now add prbarr, also important to note is that because prbarr has very little
#correlation with prbconv, and it seems both add a significant amount of value.
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$pctmin80+crime$prbconv
             +crime$prbarr+x))$adj.r.squared)

#From here, polpc adds by far the most value to the model.  Taxpc adds some, 
#but after adding polpc, any gains from adding taxpc have been lost.
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$prbarr+
               +crime$prbconv+crime$polpc+crime$pctmin80+x))$adj.r.squared)

#From here, no variable adds any more value than 0.6 which we deem to be 
#insignificant from here.

#Final
dat_f<-data.frame(crmrte,density,prbarr,prbconv,polpc,min80)
cor(dat_f)

#Test Stuff
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$polpc+crime$pctmin80+crime$prbconv
             +crime$prbarr+x))$adj.r.squared)

lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$polpc+x))$adj.r.squared)

lapply(crime[4:26], function(x) cor(crime$prbconv,x))
lapply(crime[4:26], function(x) cor(crime$wser,x))

west<-crime$west
dat_t<-data.frame(crmrte,density,prbarr,prbconv,polpc,min80,west)
cor(dat_t)
cor(taxpc,polpc)
cor(west, density)