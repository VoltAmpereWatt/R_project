
setwd('E:/programming/projects/R_project/')
library(readr)
train_features <- as.data.frame(read_csv('train_features.csv'))
train_salaries <- as.data.frame(read_csv('train_salaries.csv'))

train <- merge(train_features,train_salaries, by = 'jobId')
head(train)

sapply(train, function(x){sum(is.na(x))})
# sapply(c(train$jobType,train$degree,train$major,train$industry),factor)
train$jobType <- as.factor(train$jobType)
train$degree <- as.factor(train$degree)
train$major <- as.factor(train$major)
train$industry <- as.factor(train$industry)

plot(salary~major,data = train)
library(tidyverse)
install.packages('ggthemes')
library(ggthemes)
dev.new()
ggplot(train,aes(x = major, y = salary)) + geom_boxplot(fill = 'seagreen')
dev.new()
ggplot(train,aes(x = jobType, y = salary)) + geom_boxplot(fill = 'darkblue')
dev.new()
ggplot(train,aes(x = degree, y = salary)) + geom_boxplot(fill = 'red') 
dev.new()
ggplot(train,aes(x = industry, y = salary)) + geom_boxplot(fill = 'orange')  
graphics.off()

fit <- lm(salary~jobType+degree+major+industry+milesFromMetropolis, data = train)
summary(fit)
confint(fit)
pdf('reg-anal.pdf',width = 12,height = 12)
par(mfrow = c(2,2))
plot(fit)
dev.off()
library(car)
dev.new()
qqPlot(fit, id = TRUE, simulate = TRUE)


residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)


durbinWatsonTest(fit)

dev.new()
crPlots(fit)

ncvTest(fit)
spreadLevelPlot(fit)

install.packages('gvlma')


test <- read_csv('test_features.csv')
preds <- predict(fit, newdata = test)
graphics.off()

tests <- cbind(test, preds)

p <- ggplot(tests, aes())

# Hypothesis test
# H0 -> You make more money with an advanced degree.
train %>% group_by(degree) %>% summarize(Average=mean(salary)) %>% ungroup() %>% arrange(-Average)
# This shows that doctoral degrees get the highest average incomes.
# To verify that all of the degrees are equally important to how much money one ends up making
# A linear model is fitted between the degree and average salary. If any degree category is not 
# significant, the hypothesis fails. 
# If all the variables are significant, we can conclude that a degree does generally influence the money you make.
fit_deg <- lm(salary ~ degree, data = train)
summary(fit_deg)
# As seen, all variables are less than the critical value, so the null hypothesis holds true.

ggplot(data = train, mapping = aes(x = milesFromMetropolis,y = salary)) + geom_point()



# Hypothesis test
# H0 -> You make more money the closer you are to a city.
# To test this, a linear model is fitted on the distance and salary. If the coefficient for the distance
# is out of signficance range, we reject the null hypothesis.
# First, the salaries for each mile distance is grouped and averaged. Then the model is fitted.
miles_salary <- train %>% group_by(milesFromMetropolis) %>% summarize(Avg.Salary = mean(salary)) %>% ungroup()
fit_dist <- lm(Avg.Salary ~ milesFromMetropolis, data = miles_salary)
summary(fit_dist)
# We can see that the milesFromMetropolis parameter influences the salary significantly, so the null holds true.
# The average salary vs distance is now visualized.
dev.new()
ggplot(data = miles_salary, mapping = aes(x = milesFromMetropolis, y = Avg.Salary)) %+% 
  geom_line() %+% ylab("Average Salary") %+%
  xlab("Distance from Metropolis") %+%
  ggtitle("Salary vs Distance")
# From the figure, it is very evident that the further you move away from a major city, the less money you make.
