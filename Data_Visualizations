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
