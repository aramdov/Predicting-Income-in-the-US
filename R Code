setwd("C:/Users/aramd/Desktop/Fall 2018 classes stuff/Econ 140B"
      df <- read.csv("cps_earnings.csv")
      View(df)
      summary(df)
      str(df)
      
##### Component 1 - preparing the data for analysis
library(tidyr)
library(dplyr)
library(AER)

#1) Removing/subsetting income reported as 99999999
df1 <- subset(df,
                    subset = incwage != 9999999)
hist(df1$incwage)

#2) Examine data on edu,gender,race and marital status
names(df1$married)
summary(df1$educ)
str(df1)
View(df1)

#2) Regrouping categorical variables into dummy variables (Component 1 part 2)
df1$hs      <- ifelse(df1$educ == "high school diploma or equivalent" | df1$educ == "some college but no degree" |
                       df1$educ == "associate's degree, occupational/vocational program"  |
                       df1$educ == "associate's degree, academic program", 1, 0)
df1$married <- ifelse(df1$marst == "married, spouse present" | df1$marst == "married, spouse absent ", 1, 0)
df1$male    <- ifelse(df1$sex  == "male", 1, 0)
df1$white   <- ifelse(df1$race == "white", 1, 0)
df1$bach    <- ifelse(df1$educ == "bachelor's degree", 1, 0)
df1$grad    <- ifelse(df1$educ == "master's degree" | df1$educ == "professional school degree" |
                       df1$educ == "doctorate degree", 1, 0)

##### Component 2 - Constructing and estimating a model

# Part 1 - Regress income as a function only of education
# Part 1 -Intercept/Reference category is Less than high school observations
reg1 <- lm(incwage ~ hs + bach + grad, data = df1)
summary(reg1)

#Part 2 - Include gender and race and interaction terms
#Also include interactions to see if there is difference in income between
# males and females holding education level constant, as well as
# difference in income between white males & females and non white males
# & females holding education level constant
reg2 <- lm(incwage ~ hs + bach + grad + male + white + male*hs + male*bach + male*grad + white*hs + white*bach + white*grad + male*white*hs + male*white*bach + male*white*grad, data = df1)
summary(reg2)

#Part 3 - Include marital status in model
# Interaction term for married & males and married white males to see
# if there is a difference bettwen married males and non married males.
# Also to see if there is a difference between married white males
# and non married white males. This is also applied to females too
# we can check all those differences for females too.
reg3 <- lm(incwage ~ hs + bach + grad + male + white + male*hs + male*bach + male*grad + white*hs + white*bach + white*grad + male*white*hs + male*white*bach + male*white*grad + married + married*male + married*male*white, data = df1)
summary(reg3)

##### Component 3
#Part 3 testing if health level is valid as instrument for getting bachelors degree
# and higher, if health level is correlated with higher education
df1$higheredu         <- ifelse(df1$bach == 1 | df1$grad == 1, 1, 0)
df1$excelhealth       <- ifelse(df1$health == "excellent", 1, 0)
df1$verygoodhealth    <- ifelse(df1$health == "very good", 1, 0)
df1$averagehealth     <- ifelse(df1$health == "good" | df1$health == "fair", 1, 0)
df1$poorhealth        <- ifelse(df1$health == "poor", 1, 0)

#1st stage of 2SLS estimate, see coefficients of instruments to see which instrument is best
#instruments are split up binary variables from health category
reg4 <- lm(higheredu ~ (excelhealth) + (verygoodhealth) + (poorhealth) + male + white + married + married*male + married*male*white , data = df1)
summary(reg4)

#2SLS estimate with AER package and highest T value instrument which was excelhealth
#I believe excelhealth captured most variation in higheredu so I only include that
tslsmodel <- ivreg(incwage ~ higheredu +  male +  white + white*male | 
                  I(excelhealth) + male + white + white*male, data = df1)
summary(tslsmodel)

#Compare the new value of higheredu accounting for instrument excelhealth since it was best instrument
#Now put new value back into original regression
reg5 <- lm(incwage ~ higheredu + male + white + white*male, data = df1)
summary(reg5)
