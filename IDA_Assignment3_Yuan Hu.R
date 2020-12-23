# IDA - Assignment 3
# Yuan Hu s2031638

## Q1 ######
# (a)
require(mice)
nhanes
nrow(nhanes)
md.pattern(nhanes)

# (b)
imps = mice(nhanes, printFlag = FALSE, seed = 1)
fits = with(imps, lm(bmi ~ age + hyp + chl))
ests = pool(fits)
ests

# (c)
# Imputed values using default M=5 and changing the seed 
ests_seed2 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 2), lm(bmi ~ age + hyp + chl)))
ests_seed2
ests_seed3 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 3), lm(bmi ~ age + hyp + chl)))
ests_seed3
ests_seed4 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 4), lm(bmi ~ age + hyp + chl)))
ests_seed4
ests_seed5 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 5), lm(bmi ~ age + hyp + chl)))
ests_seed5
ests_seed6 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 6), lm(bmi ~ age + hyp + chl)))
ests_seed6

# (d)
# Imputed values using M=100 and changing the seed
ests_seed1_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 1, m = 100), 
                            lm(bmi ~ age + hyp + chl)))
ests_seed1_100
ests_seed2_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 2, m = 100), 
                            lm(bmi ~ age + hyp + chl)))
ests_seed2_100
ests_seed3_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 3, m = 100), 
                            lm(bmi ~ age + hyp + chl)))
ests_seed3_100
ests_seed4_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 4, m = 100), 
                            lm(bmi ~ age + hyp + chl)))
ests_seed4_100
ests_seed5_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 5, m = 100), 
                            lm(bmi ~ age + hyp + chl)))
ests_seed5_100
ests_seed6_100 <- pool(with(mice(nhanes, printFlag = FALSE, seed = 6, m = 100), 
                            lm(bmi ~ age + hyp + chl)))
ests_seed6_100

summary(ests_seed1_100, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]
summary(ests_seed2_100, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]
summary(ests_seed3_100, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]
summary(ests_seed4_100, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]
summary(ests_seed5_100, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]
summary(ests_seed6_100, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]



## Q2 ######
#load("dataex2.Rdata")
load("~/Desktop/Semester 1/Incomplete Data Analysis (IDA)/Assignment3/dataex2.Rdata")

# Stochastic regression imputation method
n_sri=0
for (i in 1:100){
  data = dataex2[,,i]
  imp_sri <- mice(data, m = 20, seed = 1, method = "norm.nob", printFlag = FALSE)
  fits_sri <- with(imp_sri, lm(Y ~ X))
  ests_sri <- pool(fits_sri)
  summary = summary(ests_sri, conf.int = TRUE)
  a = summary[2, 7]
  b = summary[2, 8]
  if(a<=3 & 3<=b){
    n_sri = n_sri+1
  }
  else{
    n_sri = n_sri
  }
}
prob_sri = n_sri/100
prob_sri

# Bootstrap method
n_normb=0
for (i in 1:100){
  data = dataex2[,,i]
  imp_normb <- mice(data, m = 20, seed = 1, method = "norm.boot", printFlag = FALSE)
  fits_normb <- with(imp_normb, lm(Y ~ X))
  ests_normb <- pool(fits_normb)
  summary = summary(ests_normb, conf.int = TRUE)
  a = summary[2, 7]
  b = summary[2, 8]
  if(a<=3 & 3<=b){
    n_normb = n_normb+1
  }
  else{
    n_normb = n_normb
  }
}
prob_normb = n_normb/100
prob_normb



## Q4 ######
# (a)
#load("dataex4.Rdata")
load("~/Desktop/Semester 1/Incomplete Data Analysis (IDA)/Assignment3/dataex4.Rdata")
summary(dataex4)

imps <- mice(dataex4, m = 50, seed = 1, printFlag = FALSE)
fits = with(imps, lm(y ~ x1 + x2 + (x1*x2)))
ests = pool(fits)
summary(ests, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]


# (b)
# Calculate and append interaction variable as a variable to the dataset
dataex4$x1x2 = dataex4$x1*dataex4$x2
# Passive imputation
imp0 <- mice(dataex4, maxit = 0)
meth <- imp0$method
meth["x1x2"] <- "~I(x1*x2)"
pred <- imp0$predictorMatrix
pred[c("x1", "x2"), "x1x2"] = 0
pred["x1x2", "y"] = 0
imp <- mice(dataex4, method = meth, predictorMatrix = pred, m = 50, seed = 1, printFlag = FALSE)
fit = with(imp, lm(y ~ x1 + x2 + x1x2))
pooled_ests = pool(fit)
summary(pooled_ests, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]


# (c)
dataex4$x1x2 = dataex4$x1*dataex4$x2
# Impute interaction variable as it was just another variable
imp2 <- mice(dataex4, m = 50, seed = 1, printFlag = FALSE)
fit2 = with(imp2, lm(y ~ x1 + x2 + x1x2))
pooled_ests2 = pool(fit2)
summary(pooled_ests2, conf.int = TRUE)[, c(1, 2, 3, 6, 7, 8)]



## Q5 ######
#load("NHANES2.Rdata")
load("~/Desktop/Semester 1/Incomplete Data Analysis (IDA)/Assignment3/NHANES2.Rdata")
dim(NHANES2)
str(NHANES2)
summary(NHANES2)

require(JointAI)
require(ggplot2)
mdp <- md_pattern(NHANES2, pattern = TRUE, color = c('#34111b', '#e30f41'))
mdp$plot

par(mar = c(3, 3, 2, 1), mgp = c(2, 0.6, 0))
plot_all(NHANES2, breaks = 30, ncol = 4)

imp0 <- mice(data = NHANES2, maxit = 0)
imp0$method
imp0$predictorMatrix

meth <- imp0$method
meth["hgt"] <- "norm"
meth
post <- imp0$post
post["hgt"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(0, 2))"

imp <- mice(NHANES2, method = meth, maxit = 20, m = 30, seed = 1, printFlag = FALSE)
imp$loggedEvents

plot(imp, layout = c(4, 4))

densityplot(imp)

require(devtools)
require(reshape2)
require(RColorBrewer)
require(ggplot2)
source_url("https://gist.githubusercontent.com/NErler/0d00375da460dd33839b98faeee2fdab/raw/c6f537ecf80eddcefd94992ec7926aa57d454536/propplot.R")
propplot(imp)

fit <- with(imp, lm(wgt ~ gender + age + hgt + WC))

summary(fit$analyses[[1]])

comp1 <- complete(imp, 1)
plot(fit$analyses[[1]]$fitted.values, residuals(fit$analyses[[1]]),
     xlab = "Fitted values", ylab = "Residuals")

plot(comp1$wgt ~ comp1$age, xlab = "Age", ylab = "Weight")
plot(comp1$wgt ~ comp1$hgt, xlab = "Height", ylab = "Weight")
plot(comp1$wgt ~ comp1$WC, xlab = "Waist circumference", ylab = "Weight")
boxplot(comp1$wgt ~ comp1$gender, xlab = "Gender", ylab = "Weight")

qqnorm(rstandard(fit$analyses[[1]]))
qqline(rstandard(fit$analyses[[1]]), col = 2)

pooled_ests <- pool(fit)
pooled_ests
summary <- summary(pooled_ests, conf.int = TRUE)
summary

pool.r.squared(pooled_ests, adjusted = TRUE)

df <- data.frame("Estimate" = summary[,2], 
                 "lq" = summary[,7],
                 "uq" = summary[,8]
)
rownames(df) <- c("$\\beta_0$", "$\\beta_1$","$\\beta_2$", "$\\beta_3$", "$\\beta_4$")
colnames(df) <- c("Estimate", "2.5% quantile", "97.5% quantile")
knitr::kable(df, escape = FALSE, digits = 3,
             caption = "Regression coefficient estimates and corresponding 95% CI")


