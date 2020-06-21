
# Project I - Obesity and Poverty
# Name: Luma, Alberto

# Import libraries
library(lattice)
library(ggplot2)
library(ppcor)

# Import the week-7-housing
project1_data<- read.delim(file.choose(), header=T, sep=",")
# Attach the data
attach(project1_data)

project1_data

summary(project1_data)
summary(Percent.in.Obesity)
summary(Percent.in.poverty)



ggplot(project1_data, aes(x = State, y = Percent.in.Obesity)) + 
  geom_point() +
  labs(x = 'State', y = 'Percent in Obesity', title = 'Obesity by State')

ggplot(project1_data, aes(x = State, y = Percent.in.poverty)) + 
  geom_point() +
  labs(x = 'State', y = 'Percent in Poverty', title = 'Poverty by State')

ggplot(project1_data, aes(x = State, fill = Hispanic, White, Black, Asian, American.Indian)) + 
  geom_bar() +
  labs(x = 'State', y = 'Percent in Population', title = 'Population by State')

#simple regression
cov(Percent.in.Obesity, Percent.in.poverty, method = "pearson")

cor.test(Percent.in.Obesity, Percent.in.poverty, method = "pearson", alt="greater", conf.level=0.99)


t.test(Percent.in.Obesity, Percent.in.poverty, mu=0, alt="two.sided", paired=T, conf.level=0.99)

cor.test(x = Percent.in.Obesity, y = Percent.in.poverty)

pcor.test(x = Percent.in.Obesity, y = Percent.in.poverty, z = White)


var(Percent.in.Obesity)
var(Percent.in.poverty)

var(Percent.in.Obesity)/var(Percent.in.poverty)
var.test(Percent.in.Obesity,Percent.in.poverty)

project1_data1 <- lm(Percent.in.Obesity ~ Percent.in.poverty)
summary(project1_data1)

N<- length(Percent.in.Obesity)
N
a<- mean(Percent.in.Obesity)
a
b<- mean(Percent.in.poverty)
b
c<- length(Percent.in.poverty)
c

confint(project1_data1, conf.level=0.95)
plot(project1_data1)

# calculate the standardized residuals
Residuals<- residuals(project1_data1)
predict(project1_data1)
plot(Percent.in.Obesity, Residuals)

# The sum of large residuals.
sum(Residuals)
residuals(project1_data1)^2
sum(residuals(project1_data1)^2)


#Simple linear Regression
plot(Percent.in.Obesity ~ White, data = project1_data)
plot(Percent.in.poverty ~ White, data = project1_data)
plot(Percent.in.poverty ~ Hispanic, data = project1_data)
plot(Percent.in.Obesity ~ Hispanic, data = project1_data)
plot(Percent.in.poverty ~ Black, data = project1_data)
plot(Percent.in.Obesity ~ Black, data = project1_data)
plot(Percent.in.Obesity ~ Percent.in.poverty, data = project1_data)

# Ordinary Least Squares Regression
olsreg<-lm(Percent.in.Obesity~Hispanic+Black+White)
summary(olsreg)

plot(olsreg)

olsregi<-lm(Percent.in.poverty~Percent.in.poverty)
summary(olsregi)

plot(olsregi)
