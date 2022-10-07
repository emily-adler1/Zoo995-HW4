#Homework4 Riley and Emily

library(tidyverse)
library(ggplot2)
library(lattice)

data = read.table(file="/Users/emily/Documents/limnology_seminar/HW4/Zoo995-HW4/Bees.txt", header = TRUE)


##### Q1. Does variance of spore density appear homogenous among hives? Why or why not? #####

#Check whether the variances are equal across hives
boxplot(Spobee ~ Hive, data = data)


#Not homogenous 

##### Q2. Try some transformations of the response variable to homogenize the variances (or at least improve it). Which transformation of spore density seems reasonable? Why? #####

data$LogSpobee <- log10(data$Spobee +1)


##### Q3 Q3. Develop a simple linear model for transformed spore density. Include infection (fInfection01), number of bees (sBeesN)#####
#and their interaction as explanatory variables. Check for a hive effect by plotting standardized residuals (see the residuals(yourmodel, type='pearson') function) against hive ID (fhive). S
#how your code and your plots. Do residuals look homogenous among hives?

datatransformed$Infection <- as.logical(datatransformed$Infection)

mod <- lm(LogSpobee ~ Hive * BeesN + Hive * Infection, data = datatransformed)
E <- rstandard(mod)
boxplot(E~Hive, data= data, axes = FALSE, ylim = x(-3,3))
abline(0,0); axis(2)
plot(mod, select = c(1))

#Residuals do not look homogenous, this indicates we need a random effect

##### Q4####

# Using hive as a random effect is important since there are mulitple oversvations from the same hive, so those observations are coorelated. There are 24 hives so using hive as a fixed effect would be 
# too many degrees of freedom. Using hive as a random effect allows for correlation between the hive obersvations so we only need to estimate one variance.

###

  







  