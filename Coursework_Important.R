#install.packages("car")
#install.packages("QuantPsyc")
library(car)
library(QuantPsyc)

data <- read.csv(head = TRUE, file = "C://Users//Ben//Documents//lpgaCW.csv")    

# Q1A
print(summary(data))

for(i in 1:8)
{
  plot(unlist(data[i]), unlist(data[9]), xlab = colnames(data)[i], ylab = "Prize Money", pch = 20)
  abline(lm(unlist(data[9])~unlist(data[i])), lwd = 2, col = "red")
  print(coefficients(lm(unlist(data[9])~unlist(data[i]))))
  print(cor(unlist(data[i]), unlist(data[9])))
}

# Q1B
hist(unlist(data[9]), main = "Frequency of Prize Money", xlab = "Prize Money")
xfit = seq(min(unlist(data[9])),max(unlist(data[9])),length=40) 
yfit = dnorm(xfit,mean=mean(unlist(data[9])),sd=sd(unlist(data[9]))) 
yfit = yfit*diff(unlist(data[9])[1:2])*length(unlist(data[9])) 
lines(xfit, yfit, col="red", lwd=2)
qqnorm(unlist(data[9]), pch = 20)
qqline(unlist(data[9]), col = "red")

# Q2
set.seed(616821) # ID: N0616821
n1 <- sample(1:nrow(data), 100, replace = FALSE)
training <- data[n1,]
testing <- data[-n1,]

# Q2A
model <- lm(unlist(training[9])~unlist(training[1])+unlist(training[2])+unlist(training[3])+
            unlist(training[4])+unlist(training[5])+unlist(training[6])+unlist(training[7])+
            unlist(training[8]))
# Q2B 
# Reject explanatory variables where P(0.5 > |t|)

model2 <- lm(unlist(training[9])~unlist(training[4])+unlist(training[5])+unlist(training[6])+unlist(training[7]))

print(var.test(model,model2))# Larger variance over smaller variance

ftab =  qf(0.95, df1 = 91, df2 = 95)
cat("F tabulated value = "); cat(ftab); cat("\n")

# Q2Ci
plot(fitted(model2), residuals(model2), xlab = "Fitted", ylab = "Residuals", pch = 20)
abline(h = 0, lwd = 2, col = "red")
qqnorm(residuals(model2), pch = 20)
qqline(residuals(model2), lwd =2, col = "red")
print(shapiro.test(residuals(model2)))
hist(residuals(model2), main = "Histogram of Residuals", xlab = "Residuals")
xfit = seq(min(residuals(model2)),max(residuals(model2)), length = 40) 
yfit = dnorm(xfit,mean=mean(residuals(model2)),sd=sd(residuals(model2))) 
yfit = yfit*diff(residuals(model2)[1:2])*length(residuals(model2)) 
lines(xfit, yfit, col="red", lwd=2)

trainingimpr <- log10(training)
modelimpr <- lm(unlist(trainingimpr[9])~unlist(trainingimpr[1])+unlist(trainingimpr[2])+unlist(trainingimpr[3])+
                 unlist(trainingimpr[4])+unlist(trainingimpr[5])+unlist(trainingimpr[6])+unlist(trainingimpr[7])+
                 unlist(trainingimpr[8]))

# Reject explanatory variables where P(0.5 > |t|)
model2impr <- lm(unlist(trainingimpr[9])~unlist(trainingimpr[4])+unlist(trainingimpr[7])+unlist(trainingimpr[8]))

print(var.test(modelimpr,model2impr))# Larger variance over smaller variance

ftab =  qf(0.95, df1 = 91, df2 = 95)
cat("F tabulated value = "); cat(ftab); cat("\n")

plot(fitted(model2impr), residuals(model2impr), xlab = "Fitted", ylab = "Residuals", pch =20)
abline(h = 0, lwd = 2, col = "red")
qqnorm(residuals(model2impr), pch = 20)
qqline(residuals(model2impr), lwd =2, col = "red")
print(shapiro.test(residuals(model2impr)))
hist(residuals(model2impr), main = "Histogram of Residuals", xlab = "Residuals", ylim = c(0,80))
xfit = seq(min(residuals(model2impr)),max(residuals(model2impr)), length = 40) 
yfit = dnorm(xfit,mean=mean(residuals(model2impr)),sd=sd(residuals(model2impr))) 
yfit = (yfit*diff(residuals(model2impr)[1:2])*length(residuals(model2impr))) 
lines(xfit, yfit, col="red", lwd=2)

# Q2Cii
plot(hatvalues(model2impr), rstandard(model2impr), xlab = "Leverage", ylab = "Standardised Residual", xlim = c(0,0.5), ylim = c(-3,3), pch = 20)
abline(h = c(-2,2), lty = 2, col = "red")
abline(v = 2 * 4 / 100, lty = 2, col = "red")

# 2D
print(summary(model2impr))
print(lm.beta(model2impr))
print(vif(model2impr))

# Q3
testingimpr <- log10(testing)
model2imprtesting <- lm(unlist(testingimpr[9])~unlist(testingimpr[4])+unlist(testingimpr[7])+unlist(testingimpr[8]))
plot(predict(model2imprtesting, testingimpr, interval = "prediction"), pch = 20)
plot(fitted(model2imprtesting),residuals(model2imprtesting), pch = 20)
ftab =  qf(0.95, df1 = 3, df2 = 42)
cat("F tabulated value = "); cat(ftab); cat("\n")
  
#layout(matrix(c(1,2,3,4),2,2))
#plot(model2)