
library(RColorBrewer)

## Box Plot of Age by Year for Female Runners
load("CBWomenTables.rda")
age = sapply(womenMat, 
             function(x) suppressWarnings(as.numeric(x[ , 'ag'])))

boxplot(age, main = "Age by Year for Female Runners", 
        ylab = "Age", xlab = "Year")

## Scatter Plot for Run Times vs. Age for Female Runners
load("cbWomen.rda")

plot(runTime ~ age, data = cbWomen, ylim = c(40, 180),
     xlab = "Age (years)", ylab = "Run Time (minutes)",
     main = "Run Times vs. Age for Female Runners")

## Fit Models to Average Performance
Purples8 = brewer.pal(9, "Purples")[8]
Purples8A = paste(Purples8, "14", sep = "")

plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbWomen, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)",
     main = "Run Times vs. Age for Female Runners")

smoothScatter(y = cbWomen$runTime, x = cbWomen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)",
              main = "Run Times vs. Age for Female Runners")

## Side-by-Side Boxplots of Female Runners’ Run Time vs. Age
cbWomenSub = cbWomen [cbWomen$runTime>30 & cbWomen$age>15,]
ageCat = cut(cbWomenSub$age, breaks=c(seq(15, 75, 10), 90))
table(ageCat)
plot(cbWomenSub$runTime~ageCat, main="Female Runners’ Run Time vs. Age",
     xlab="Age (years)", ylab="Run Time (minutes)")

## Residual Plot from Fitting a Simple Linear Model of Performance to Age
lmAge = lm(runTime ~ age, data = cbWomenSub)

cbWomenSubAge = cbWomenSub$age[1:length(lmAge$residuals)]
smoothScatter(x = cbWomenSubAge, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals",
              main = "Female Runners’ Age vs. Residuals")
abline(h = 0, col = "purple", lwd = 3)

resid.lo = loess(resids ~ age, data = data.frame(resids = residuals(lmAge), 
                                                 age = cbWomenSubAge))
age20to80 = 20:80

resid.lo.pr = predict(resid.lo, newdata = data.frame(age = age20to80))

smoothScatter(x = cbWomenSubAge, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals",
              main = "Female Runners’ Age vs. Residuals")+
lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)


## Piecewise Linear and Loess Curves Fitted to Run Time vs. Age
womenRes.lo = loess(runTime ~ age, cbWomenSub)
womenRes.lo.pr = predict(womenRes.lo, data.frame(age = age20to80))
over50 = pmax(0, cbWomenSub$age - 50)
lmOver50 = lm(runTime ~ age + over50, data = cbWomenSub)
# summary(lmOver50)

decades = seq(30, 60, by = 10)
overAge = lapply(decades, function(x) pmax(0, (cbWomenSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)

lmPiecewise = lm(runTime ~ . , data = cbind(cbWomenSub[, c("runTime", "age")], overAge))
# summary(lmPiecewise)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")
lines(x = age20to80, y = womenRes.lo.pr,
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

## Line Plot of the Number of Female Runners by Year
numRunners = with(cbWomen, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlim = c(2001,2012),
     xlab = "Years", ylab = "Number of Runners")

## Density Curves for the Age of Female Runners for 2 years 
## (smallest and largest year that you analyzed)
summary(cbWomenSub$runTime[cbWomenSub$year == 2001])
summary(cbWomenSub$runTime[cbWomenSub$year == 2012])

age2001 = cbWomenSub[ cbWomenSub$year == 2001, "age" ]
age2012 = cbWomenSub[ cbWomenSub$year == 2012, "age" ]
plot(density(age2001, na.rm = TRUE),
     ylim = c(0, 0.07), col = "purple",
     lwd = 3, xlab = "Age (years)", main = "")
lines(density(age2012, na.rm = TRUE),
      lwd = 3, lty = 2, col="green")
legend("topright", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("2001", "2012"), bty = "n")


## Loess Curves Fit to Performance for 2 years (smallest and largest year that you analyzed) Female Runners
qqplot(age2001, age2012, pch = 19, cex = 0.5,
       ylim = c(10,90), xlim = c(10,90),
       xlab = "Age in 2001 Race",
       ylab = "Age in 2012 Race",
       main = "Quantile-quantile plot of male runner's age") 
abline(a = 0, b = 1, col="red", lwd = 2)

mR.lo01 = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 2001,])
mR.lo.pr01 = predict(mR.lo01, data.frame(age = age20to80))
mR.lo12 = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))
plot(mR.lo.pr01 ~ age20to80, ylim = c(90,120), xlim = c(20,80),
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)",
     main = "Female runners' Age vs. fitted Run Time")
lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("2001", "2012"), bty = "n")

## Difference between Loess Curves of the predicted run time for 2 years 
## (smallest and largest year that you analyzed)
gap11 = mR.lo.pr12 - mR.lo.pr01
plot(gap11 ~ age20to80, type = "l" , xlab = "Age (years)",
     ylab = "Difference in Fitted Curves (minutes)",
     main = "Difference between predicted run time for 2001 and 2012", lwd = 2)
