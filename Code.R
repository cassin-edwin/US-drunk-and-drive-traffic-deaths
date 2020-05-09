# Created by Cassin Thangam Edwin and Rajasekhar Devineni

setwd("E:/UTD Spring Semester/Applied Econometrics/Project")
library(lmtest)
library(sandwich)
library(foreign)
library(plm)
library(dplyr)
library(ggplot2)
df <- read.dta('car_fatalities.dta')
head(df)
View(df)

# Feature Engineering
df$popwoutdry <- df$pop - (df$dry/100)*df$pop
df$fatpop <- (df$allmort + df$allnite)/(df$popwoutdry)
df$percent1 <- ((df$allmort + df$allnite)/(df$popwoutdry))*100



# First Regression trying out all the variables and then only regressing the significant variables
lm1 <-lm(formula = (allmort~(comserd+sobapt+mlda+miles)), data = df)
plot(lm1)
summary(lm1)

# Regression with all the punishment variables
lm3 <- lm(formula = allmort ~ (beertax+mlda+jaild+comserd), data = df)
summary(lm3)

# Regression with quadratic variable
lm4 <- lm(formula = (allmort ~ (beertax + I(beertax^2) +perinc+miles+sobapt+mlda+spircons)), data = df )
summary(lm4)

# Regression on lm4 by adding an irrelevant variable
lm5 <- lm(formula = (allmort ~ (beertax + I(beertax^2) +perinc+miles+sobapt+mlda+spircons+comserd)), data = df)
summary(lm5)

# Regression with an interaction variable as spircons and perinc variables are inter related with each other
lm6 <- lm(formula = (allmort ~ (spircons+perinc+I(spircons*perinc))), data = df)
summary(lm6)

# Plot for residuals vs Fitted values to find heteroskedasticity
plot(lm1)

# Since we find that some heteroskedasticity was present in the model, we can use the estimator but the standard errors are incorrect.
# Regression with white robust standard errors
lm2 <- coeftest(lm1,vcov. = vcovHAC)
summary(lm2)

# Panel data model with Overall
summary(plm(formula = (allmort~(comserd + aidall + sobapt + mlda + spircons + miles)), data = df, index = c('state','year'), model = 'pooling'))

# Fixed effects model 'within'
a <- plm(formula = (allmort~(jaild+ comserd+ aidall+ unrate+ spircons+ miles + gspch)), data = df, index = c('state','year'), model = 'within')

#Random effects model with 'between'
summary(plm(formula = (allmort~(comserd+ aidall+ sobapt + miles)), data = df, index = c('state','year'), model = 'between'))

#Random effects model 
b <- plm(formula = (allmort~(jaild+ comserd+ aidall+ unrate+ spircons+ miles + gspch)), data = df, index = c('state','year'), model = 'random')


# Hausman test to decide between fixed or random models. 
# Null hypothesis is chosen to be random effects model
# Alternate hypothesis is chosen to be fixed effects model. 
phtest(a,b)


#The overall analysis says that not all the punishment variables significantly help in reducing the traffic deaths.
