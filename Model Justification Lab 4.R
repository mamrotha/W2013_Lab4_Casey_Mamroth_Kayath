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
Model1_r<-summary(lm(crime$crmrte~crime$density))$adj.r.squared
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+x))$adj.r.squared-Model1_r)

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

#we also check that we can't remove any variables
lapply(crime[5:26], function(x) 
  summary(lm(crime$crmrte~crime$density+crime$prbarr+
               +crime$prbconv+crime$polpc+crime$pctmin80-x))$adj.r.squared)

#From here, no variable adds any more value than 0.006 which we deem to be 
#insignificant from here.

#Final

dat_f<-data.frame(crime$crmrte,crime$density,crime$prbarr,
                  crime$prbconv,crime$polpc,crime$pctmin80)
cor(dat_f)

#Test Stuff
lapply(crime[5:26], function(x) cor(crime$prbarr,x))

mod_sum<-summary(lm(crime$crmrte~crime$density+crime$prbconv+
                      crime$prbarr+crime$polpc+
                      crime$pctmin80-crime$pctmin80))
adjsq<-mod_sum$adj.r.squared
check<-mod_sum$residual
lapply(crime[5:26], function(x) cor(check,x))
check
adjsq

AIC(model_f)
AIC(lm(crime$crmrte~crime$density))

lapply(crime[5:26], function(x) 
  AIC(lm(crime$crmrte~crime$density+crime$prbarr+crime$pctmin80+
           crime$polpc+x)))

summary(lm(log(crime$crmrte)~crime$prbarr+crime$prbconv+crime$prbpris+crime$avgsen
           +crime$polpc+log(crime$density)+crime$taxpc+crime$west+crime$central
           +crime$urban+crime$pctmin80+crime$wcon+crime$wtuc+crime$wtrd+crime$wfir
           +crime$wser+crime$wmfg+crime$wfed+crime$wsta+crime$wloc+crime$mix
           +crime$pctymle))

summary(lm(log(crime$crmrte)~log(crime$density)+crime$prbarr
             +crime$prbconv+log(crime$polpc)+crime$pctmin80))

summary(lm(log(crime$crmrte)~log(crime$density)+crime$prbarr
           +crime$prbconv+log(crime$polpc)+crime$pctmin80))


summary(lm(crime$crmrte~crime$prbarr+crime$prbconv+crime$prbpris+crime$avgsen
           +crime$polpc+crime$taxpc+crime$west+crime$central
           +crime$urban+crime$pctmin80+crime$wcon+crime$wtuc+crime$wtrd+crime$wfir
           +crime$wser+crime$wmfg+crime$wfed+crime$wsta+crime$wloc+crime$mix
           +crime$pctymle))

summary(lm(crime$wfed~crime$prbarr+crime$prbconv+crime$prbpris+crime$avgsen
           +crime$polpc+crime$density+crime$taxpc+crime$west+crime$central
           +crime$urban+crime$pctmin80+crime$wcon+crime$wtuc+crime$wtrd
           +crime$wfir+crime$wser+crime$wmfg+crime$crmrte+crime$wsta+crime$wloc
           +crime$mix+crime$pctymle))

redsid<-summary(lm(crime$crmrte~crime$density))$residual
lapply(crime[5:26], function(x) cor(resid,x))

summary(lm(crime$crmrte~crime$wcon+crime$wtuc+crime$wtrd+crime$wfir
           +crime$wser+crime$wmfg+crime$wfed+crime$wsta+crime$wloc))

model_1<-summary(lm(crime$crmrte~crime$prbarr+crime$prbconv+crime$prbpris
                    +crime$avgsen+crime$polpc+crime$density+crime$taxpc
                    +crime$west+crime$central+crime$urban+crime$pctmin80
                    +crime$wcon+crime$wtuc+crime$wtrd+crime$wfir+crime$wser
                    +crime$wmfg+crime$wfed+crime$wsta+crime$wloc+crime$mix
                    +crime$pctymle))
