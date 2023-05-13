#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("olsrr")
#install.packages("leaps")
#install.packages("corrplot")

#Importing necessary packages
library(readr)
library(tidyr)
library(dplyr)
library(olsrr)
library(leaps)
library(corrplot)
#Load in Life Expectancy Dataset
data<- read_csv("C:/Users/pmsa/Desktop/practiceCode/Life_Expectancy_Data.csv",col_names=c("country","year", "lifeExpectancy", "status", "adultMortality","infantDeaths","alcohol","percentageExpenditure","hepatitisB","measles","BMI","under5deaths","polio","totalExpenditure","diphtheria","HIVAIDS","GDP","population","thinness1to19","thinness5to9","incomeComposition","schooling"))
summary(data)
#Get number of unique countries in this set
length(unique(data$country))

#Remove any rows with empty values
df = data %>% drop_na()
summary(df)

#EDA: Exploratory Data Analysis
length(unique(df$country))

#Histograms of Variable Distributions
hist(df$lifeExpectancy, main="Life Expectancy", xlab="Life Expectancy in Years")
hist(df$status, main="Developing Country Status", xlab="0 is developing, 1 is developed")
hist(df$adultMortality,main="Adult Mortality Rates of Both Sexes",xlab="Number of mortalities between 15 and 60 years per 1000 population")
hist(df$infantDeaths, main="Infant Deaths",xlab="Number of Infant Deaths per 1000 Population")
hist(df$alcohol,main="Alcohol Consumption",xlab="Liters of Pure Alcohol Consumer Per Capita (Ages 15+)")
hist(df$percentageExpenditure,main="Percent Expenditure on Healthcare", xlab="Percentage of GDP per Capita spent on healthcare")
hist(df$hepatitisB, main="Hepatitis B",xlab="Percentage of Hepatitis B Immunization Coverage among 1-year-olds")
hist(df$measles,main="Measles",xlab="Number of Reported Cases per 1000 Population")
hist(df$BMI,main="BMI",xlab="Average Body Mass Index of entire population")
hist(df$under5deaths, main="Under Age 5 Deaths",xlab="Number of Under-Five deaths per 1000 population")
hist(df$polio,main="Polio",xlab="Percentage of Polio (Pol3) Immunization Coverage among 1-year-olds")
hist(df$totalExpenditure,main="Total Expenditure",xlab="General government expenditure on health as a percentage of total government expenditure")
hist(df$diphtheria,main="Diphtheria",xlab="Percentage of Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds")
hist(df$HIVAIDS,main="HIV/AIDS",xlab="Deaths per 1000 Live Births with HIV/AIDS (0-4 years of age)")
hist(df$GDP,main="GDP",xlab="Gross Domestic Product per capita (in USD)")
hist(df$population,main="Population",xlab="Population of the Country")
hist(df$thinness1to19,main="Thinness 1-19 Years of Age",xlab="Prevalence of thinness among children and adolescents for Age 10 to 19 as a percentage")
hist(df$thinness5to9,main="Thinness 5-9 Years of Age",xlab="Prevalence of thinness among children and adolescents for Age 5 to 9 as a percentage")
hist(df$incomeComposition,main="Income Composition of Resources",xlab="Human Development Index in terms of income composition of resources (index ranging from 0 to 1)")
hist(df$schooling,main="Schooling",xlab="Number of Years of Schooling")

#Boxplots of Variable Distributions
boxplot(df$lifeExpectancy, main="Life Expectancy", xlab="Life Expectancy in Years")
boxplot(df$status, main="Developing Country Status", xlab="0 is developing, 1 is developed")
boxplot(df$adultMortality,main="Adult Mortality Rates of Both Sexes",xlab="Number of mortalities between 15 and 60 years per 1000 population")
boxplot(df$infantDeaths, main="Infant Deaths",xlab="Number of Infant Deaths per 1000 Population")
boxplot(df$alcohol,main="Alcohol Consumption",xlab="Liters of Pure Alcohol Consumer Per Capita (Ages 15+)")
boxplot(df$percentageExpenditure,main="Percent Expenditure on Healthcare", xlab="Percentage of GDP per Capita spent on healthcare")
boxplot(df$hepatitisB, main="Hepatitis B",xlab="Percentage of Hepatitis B Immunization Coverage among 1-year-olds")
boxplot(df$measles,main="Measles",xlab="Number of Reported Cases per 1000 Population")
boxplot(df$BMI,main="BMI",xlab="Average Body Mass Index of entire population")
boxplot(df$under5deaths, main="Under Age 5 Deaths",xlab="Number of Under-Five deaths per 1000 population")
boxplot(df$polio,main="Polio",xlab="Percentage of Polio (Pol3) Immunization Coverage among 1-year-olds")
boxplot(df$totalExpenditure,main="Total Expenditure",xlab="General government expenditure on health as a percentage of total government expenditure")
boxplot(df$diphtheria,main="Diphtheria",xlab="Percentage of Diphtheria tetanus toxoid and pertussis (DTP3) immunization coverage among 1-year-olds")
boxplot(df$HIVAIDS,main="HIV/AIDS",xlab="Deaths per 1000 Live Births with HIV/AIDS (0-4 years of age)")
boxplot(df$GDP,main="GDP",xlab="Gross Domestic Product per capita (in USD)")
boxplot(df$population,main="Population",xlab="Population of the Country")
boxplot(df$thinness1to19,main="Thinness 1-19 Years of Age",xlab="Prevalence of thinness among children and adolescents for Age 10 to 19 as a percentage")
boxplot(df$thinness5to9,main="Thinness 5-9 Years of Age",xlab="Prevalence of thinness among children and adolescents for Age 5 to 9 as a percentage")
boxplot(df$incomeComposition,main="Income Composition of Resources",xlab="Human Development Index in terms of income composition of resources (index ranging from 0 to 1)")
boxplot(df$schooling,main="Schooling",xlab="Number of Years of Schooling")

#Analyzing normality of the Life Expectancy variable
hist(df$lifeExpectancy, probability=TRUE, main="Life Expectancy",xlab="Life Expectancy in Years")
curve(dnorm(x,mean=mean(df$lifeExpectancy),sd=sd(df$lifeExpectancy)),add=TRUE,col="blue")
qqnorm(df$lifeExpectancy)
qqline(df$lifeExpectancy, col="blue")
shapiro.test(df$lifeExpectancy)

#Multicollinearity
numVar = df %>% select_if(is.numeric)
numVarCor = cor(numVar)
corrplot(numVarCor,method="color",addCoef.col = "black",number.cex=0.5)
#variables with high levels of multicollinearity:
#percentageExpenditure, under5deaths,thinness1to19, incomeComposition


#Choosing Predictors for the Regression
m = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + GDP + population + thinness1to19 + thinness5to9 + incomeComposition + schooling, data=df)
summary(m)

#method 1: Remove Predictors whose p-value is greater than 0.05 1 by 1 and monitor adjusted R^2 value
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + GDP + population + thinness1to19 + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
#Initial Adj R^2: 0.8336
#Highest P-value Predictor is: thinness1to19
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + GDP + population + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
#Adj R^2: 0.8337
#Highest P-value Predictor is: GDP
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + population + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
#Adj R^2: 0.8338
#Highest P-value Predictor is: population
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
#Adj R^2: 0.8339
#Highest P-value Predictor is: measles
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
#Adj R^2: 0.834
#Highest P-value Predictor is: hepatitisB
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
#Adj R^2: 0.8339
#Adj R^2 decreased in previous change, so the previous selection of predictors might be optimal
#Evaluate Regression Metrics on Model 1:
m1 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + thinness5to9 + incomeComposition + schooling, data=df)
summary(m1)
AIC(m1)
BIC(m1)
ols_mallows_cp(m1,m)
plot(m1)
car::vif(m1)

#Method 2: Remove all predictor vals with p-value greater than 0.05 at once
m2 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + GDP + population + thinness1to19 + thinness5to9 + incomeComposition + schooling, data=df)
summary(m2)
#Predictors with p-value > 0.05:
#hepatitisB,measles,polio,totalExpenditure,GDP,population,thinness1to19,thinness5to9
m2 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + BMI + under5deaths + diphtheria + HIVAIDS + incomeComposition + schooling, data=df)
summary(m2)
AIC(m2)
BIC(m2)
ols_mallows_cp(m2,m)
plot(m2)
car::vif(m2)
#results in an Adj R^2 of 0.8331, inferior to method 1

#method 3: LEAPS
m3 = regsubsets(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + measles + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + GDP + population + thinness1to19 + thinness5to9 + incomeComposition + schooling,
                data =df,
                nbest = 1,      # 1 best model for each number of predictors
                nvmax = NULL,    # NULL for no limit on number of variables
                force.in = NULL, force.out = NULL,
                method = "exhaustive")
m3
m3subset = summary(m3)
m3subset
m3df = m3subset$outmat
m3df
which.max(m3subset$adjr2)
m3subset$which[15,]

#Utilizing the LEAPS method, we created a model utilizing the following 15 predictor variables:
#Status, adultMortality, infantDeaths, alcohol, percentageExpenditure, hepatitisB, BMI, under5deaths
#Polio, totalExpentiture, diphtheria, HIVAIDS, thinness5to9, incomeComposition, schooling
#It is worth noting, that Model 1 and Model 3 ended up choosing the exact same predictor variables
m3 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + BMI + under5deaths + polio + totalExpenditure + diphtheria + HIVAIDS + thinness5to9 + incomeComposition + schooling, data=df)
summary(m3)
AIC(m3)
BIC(m3)
ols_mallows_cp(m3,m)
plot(m3)
car::vif(m3)

#model 4, removing variables from Model 1 with high collinearity
m4 = lm(lifeExpectancy ~ status + adultMortality + infantDeaths + alcohol + percentageExpenditure + hepatitisB + BMI + polio + totalExpenditure + diphtheria + HIVAIDS + thinness5to9 + incomeComposition + schooling, data=df)
summary(m4)

AIC(m4)
BIC(m4)
ols_mallows_cp(m4,m)
plot(m4, which=1:5)
car::vif(m4)


