library(plyr)
library(carData)
library(car)
library(fBasics)
library(timeData)
library(timeSeries)

arythmia <- read.csv("D:data.csv")
age <- arythmia$age
n = sum(count(age)$freq)

#Правило Стьорджеса
#s = 1 + log2(n)
hist(age, breaks = "Sturges", probability = TRUE)
lines(density(age))

#Правило Скотта
#h1 = 3.5 * sd(age) * n^(-1/3)
hist(age, breaks = "Scott", probability = TRUE)
#x <- dnorm(n, mean = mean(age), sd = sd(age))
lines(density(age))

#Правило Фрідмана-Дьяконіса

#h2 = 2 * IQR(age) * n^(-1/3)
hist(age, breaks = "Freedman-Diaconis", probability = TRUE)
lines(density(age))

# Q-Q plots
qqnorm(age)
qqline(age)
qqPlot(age)

#P-P plots
x <- rnorm(n,0,1)
plot(ecdf(x),pnorm(age, mean(age),sd(age)), main = "P-P plot",
     xlab = "Observed probability", ylab = "Expected probability")

#ЛілієФорса
lillieTest(age)