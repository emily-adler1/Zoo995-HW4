##load packages
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)

##set working directory
setwd("/Users/rileybook/Documents/uw/academics/fall 2022/zoo 955/hw 4")

##read data
data <- read.delim("bees.txt")

##organize data
data<- data%>%
  #remove unnecessary columns
  select(-Rawdata, -X, -Y)%>%
  #convert Infection levels to binary with (1= infection present, 0= infection absent)
  mutate(Infection= as.factor(ifelse(Infection>0, "1", "0")),
         Hive= as.factor(Hive))

##analyze data

#look at variance in spore density among hives
data%>%
  ggplot(aes(x= Hive, y= Spobee))+
  geom_jitter(size=2, width=0.5) +
  ggtitle("Variance vs Hive")



#transform spore density to (try to) reduce variance

#log10
data%>%
  ggplot(aes(x= Hive, y= log10(Spobee+1)))+
  geom_jitter(size=2, width=0.5) +
  ggtitle("Log 10 + 1 transformed variance vs Hive")
#pretty good

#natural log
data%>%
  ggplot(aes(x= Hive, y= log(Spobee+1)))+
  geom_jitter(size=2, width=0.5)
#pretty good

#square root
data%>%
  ggplot(aes(x= Hive, y= sqrt(Spobee)))+
  geom_jitter(size=2, width=0.5)
#not helpful

#cube root
data%>%
  ggplot(aes(x= Hive, y= Spobee^(1/3)))+
  geom_jitter(size=2, width=0.5)
#not helpful

#transform Spobee using log + 1 transformation in the dataframe
data$logSpobee= log10(data$Spobee + 1)
hist(data$logSpobee)
#logSpobee is not normally distributed

#make a simple linear model of transformed spore density as a function of infection status
#number of bees, and the interaction of infection status and number
#of bees
mod.1<- lm(logSpobee~ BeesN * Infection, data = data)
summary(mod.1)

#plot standardized residuals of mod.1 x hive to investigate hive effect
mod.1.res<- residuals(mod.1, type= "pearson")
#add residuals to data
mod.1.res.plot<- tibble(data$Hive,mod.1.res)
#residuals vs. predictor plot of mod.1
plot(data$Hive,mod.1.res, xlab = "", ylab ="")
title(main="Residuals for Mod 1",
     xlab="Hive",
     ylab="Residuals")
#residuals are not homogenous across hives

#fit the "beyond optimal" model using lme4
#just re-iterating, this is the lm() of all explanatory variables (# bees, infection)
#with no random component
mod.1<- lm(logSpobee~ BeesN * Infection, data = data)
summary(mod.1)

#this is the random intercept model with Hive as the random component
mod.2<- lmer(logSpobee~ BeesN * Infection + (1|Hive), data= data)
summary(mod.2)

#this is the random intercept and slope model
mod.3<- lmer(logSpobee~ BeesN * Infection + (BeesN|Hive), data= data)
summary(mod.3)

#compare mod.1, mod.2, and mod.3
anova(mod.3, mod.2, mod.1)
#AIC mod.1= 205.63
#AIC mod.2= 133.32, so this is the best one
#AIC mod.3= 137.01

#mod.2 (the random intercept model) is the best
#check the model
mod.2.res<- residuals(mod.2, type="pearson")
#mod.2 standardized residuals x fitted values
plot(mod.2, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values Mod 2")

#this doesn't look great
#mod.2 standardizes residuals x # of bees
plot(data$BeesN, mod.2.res, ylab= "Residuals", xlab = "Number of Bees in Hive", main = "Residuals vs Number of Bees for Mod 2")
#mod.2 standardized residuals x infection
plot(data$Infection, mod.2.res, ylab= "Residuals", xlab = "Infection", main = "Residuals vs Infection for Mod 2")

#look at summary of mod.2 (full)
summary(mod.2)
#Infection is the only significant explanatory variable

#fit full model using ML
mod.2.full.ml<-lmer(logSpobee~ BeesN * Infection + (1|Hive), REML= FALSE, data= data)
#fit reduced model using ML
summary(mod.2.full.ml)
mod.2.red.ml<- lmer(logSpobee~ BeesN + Infection + (1|Hive), REML= FALSE, data= data)
summary(mod.2.red.ml)
#compare full and reduced models using ML
anova(mod.2.full.ml, mod.2.red.ml)
#drop the interaction term because the models aren't significantly different

#next, drop # of bees because it is the least significant variable
mod.2.nobees<- lmer(logSpobee~ Infection + (1|Hive), REML= FALSE, data= data)
summary(mod.2.nobees)
anova(mod.2.red.ml, mod.2.nobees)
#drop # of bees because the models aren't significantly different
#we are left with Infection as the only fixed effect

#refit mod.2.nobees using REML
mod.2.nobees.REML<- lmer(logSpobee~ Infection + (1|Hive), data= data)
summary(mod.2.nobees.REML)
mod.2.nobees.REML.res<- residuals(mod.2.nobees.REML, type="pearson")

#histogram of residuals
hist(mod.2.nobees.REML.res, xlab = "Residual Values", main = "Histogram of residuals after removing # of bees from model")
#normal-ish

#residuals vs. fitted
plot(mod.2.nobees.REML, xlab= "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values REML model w/out # of bees")
#still wider range of resid at lower fitted values than we'd like to see

#residuals x predictors
plot(data$Infection, mod.2.nobees.REML.res,  ylab= "Residuals", xlab = "Infection", main = "Residuals vs Infection for Mod 2 without # of Bees")

#calculate correlation between observations from the same hive
data<- data%>%
  group_by(Hive)%>%
  mutate(var.hive=var(Spobee))

var(data$var.hive)/(var(data$var.hive)+var(mod.2.nobees.REML.res))
#1



