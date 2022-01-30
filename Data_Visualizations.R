#Number of trips in Izmir metro by months

install.packages("ggplot2")
library(ggplot2)

ggplot_facet_analysis <- ggplot(gerceklesen_sefer_sayilari, aes(x=AY, y=IZMIR_METRO, group=IZMIR_METRO)) + 
  geom_boxplot(aes(fill=IZMIR_METRO))

ggplot_facet_analysis + facet_grid(. ~ AY) #by months

#First, let’s get to know our data (numerically) / STEP 1

# The mean is calculated by dividing the sum of the values that make up a variable by the total number of values.
# calculate the mean of multiple variables
colMeans(gerceklesen_sefer_sayilari[,c("IZMIR_METRO","KARSIYAKA_TRAMVAYI", "KONAK_TRAMVAYI" )],na.rm = T)

#First, let’s get to know our data (numerically) / STEP 2

# We continue the calculations with the highest average "IZMIR METRO"
# The middle point of a variable arranged in ascending or descending order is called the median.
median(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)

#First, let’s get to know our data (numerically) / STEP 3

# We continue the calculations with the highest average "IZMIR METRO"
# Variance is a widely used measure to understand how spread out a variable is.
var(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)

#First, let’s get to know our data (numerically) / STEP 4

# We continue the calculations with the highest average "IZMIR METRO"
# Standart Devision is the square root of the variance.
sd(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)

#First, let’s get to know our data (numerically) / STEP 5

# We continue the calculations with the highest average "IZMIR METRO"
# The skewness value gives information about the shape of the distribution. A completely symmetrical distribution has a skewness of 0.

# let's calculate skewness:
library(moments)
skewness(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)

First, let’s get to know our data (numerically) / STEP 6

# We continue the calculations with the highest average "IZMIR METRO"
# Standart Devision is the square root of the variance.
sd(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)


#Density Plot of Izmır Metro

d_plot<-density(gerceklesen_sefer_sayilari$IZMIR_METRO)
plot(d_plot,frame=FALSE,col="steelblue",main="Density plot of Izmır Metro")

Confidence Intervals

# Confidence intervals can be established using an assumption about the distribution, information from the sample, and an appropriate estimator. A confidence interval indicates which interval the population parameter is likely to be. But this does not mean that the universe parameter definitely falls within this range.

# number of observations(n)
AA_n <- sum(!is.na(gerceklesen_sefer_sayilari$IZMIR_METRO))

# mean
AA_m <- mean(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)

# standart devision
AA_sd <- sd(gerceklesen_sefer_sayilari$IZMIR_METRO,na.rm = T)

# %95 confidence interval
low <- AA_m - 1.96 * (AA_sd/sqrt(AA_n))
low

# confidence intervals continue

norm.interval = function(data, variance, conf.level = 0.95) {
   z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
   xbar = mean(data)
   sdx = sqrt(variance/length(data))
   c(xbar - z * sdx, xbar + z * sdx)
}

# Standart devision = 2750.969 = ~3000 

ızmır.var<-3000^2
norm.interval(gerceklesen_sefer_sayilari$IZMIR_METRO, ızmır.var)

# t-test

ızmır.CI <- t.test(gerceklesen_sefer_sayilari$IZMIR_METRO)$conf.int
ızmır.CI

ızmır.t.test<- t.test(gerceklesen_sefer_sayilari$IZMIR_METRO, alternative = c("less"), mu = 10000, conf.level = 0.95)
ızmır.t.test

names(ızmır.t.test)
ızmır.t.test$p.value 
ızmır.t.test$estimate 

#Is the data normal?
with(gerceklesen_sefer_sayilari, qqnorm(IZMIR_METRO))
Welch Two Sample t-test

# hypothesis I
  
# h0 = The amount of CO2 emission is not related to the number of passengers on the flights. (As the number of passengers increases, does the energy and power increase?)

# h1 = The amount of CO2 emission is related to the number of passengers on the flights. (As the number of passengers increases, does the energy and power increase?)

gerceklesen_sefer_sayilari$IZMIR_METRO = rnorm(15)
emisyon_degerleri_co2$IZMIR_METRO = rnorm(13)
t.test(gerceklesen_sefer_sayilari$IZMIR_METRO, emisyon_degerleri_co2$IZMIR_METRO)

# hypothesis II
# h0= Metro services produce more CO2 than tram services.
# h1= Metro services do not produce more CO2 than tram services.

emisyon_degerleri_co2$IZMIR_METRO = rnorm(13)
emisyon_degerleri_co2$KARSIYAKA_TRAMVAYI= rnorm(13) 

t.test(emisyon_degerleri_co2$IZMIR_METRO, emisyon_degerleri_co2$KARSIYAKA_TRAMVAYI)

# hypothesis II
# h0= The amount of energy consumption per passenger of the passengers using the tram is less than the amount of energy consumption per passenger of the passengers using the metro.
# h1= The amount of energy consumption per passenger of the passengers using the tram is not less than the amount of energy consumption per passenger of the passengers using the metro. (unrelated)

enerji_tuketimi_yolcubasina$KARSIYAKA_TRAMVAYI = rnorm(16)
enerji_tuketimi_yolcubasina$IZMIR_METRO = rnorm(16)

t.test(enerji_tuketimi_yolcubasina$KARSIYAKA_TRAMVAYI, enerji_tuketimi_yolcubasina$IZMIR_METRO)

Analysis of Variance (ANOVA) or Regression

# ANOVA (Analysis of Variance) is used to test whether more than two quantitative population averages are equal. 
# This test is also called the "F test". 
# In the ANOVA test, comparisons are made between variances, so this test is called "analysis of variance".

# Regression analysis is an analysis method used to measure the relationship between two or more quantitative variables. #If the analysis is done using a single variable, it is called univariate regression, if more than one variable is used, it is called multivariate regression analysis.

# Here we will do a regression analysis.

qplot(IZMIR_METRO, KARSIYAKA_TRAMVAYI, data = gerceklesen_sefer_sayilari)

with(gerceklesen_sefer_sayilari, cor(IZMIR_METRO, AY)) 
ibb.lm <- lm(IZMIR_METRO ~ ., data = gerceklesen_sefer_sayilari)
# Summary of the linear regression model
ibb.lm
# List all attributes of the linear model
attributes(ibb.lm)
# Coefficients
ibb.lm$coef
# Pull coefficients element from summary(lm) object
round(summary(ibb.lm)$coef, 3)
# Plotting the lm object
plot(ibb.lm)





