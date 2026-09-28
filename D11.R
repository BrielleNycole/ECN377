## ECN 377 - Day 11 STARTER  |  OLS properties 1-3;  SST = SSE + SSR, R^2

library(wooldridge)
data("wage1")
reg <- lm(wage ~ educ, data = wage1)
resids <- reg$residuals
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]
## ---- OLS properties 1-3  (hold on ANY sample) ----
sum(resids)               # 1) the residuals sum to 0
cov(resids, wage1$educ)   # 2) x & residuals are uncorrelated
xbar <- b0+b1*(mean(wage1$educ)) #3)(xbar, ybar) is ON the line
ybar <- mean(wage1$wage)

## ---- SST = SSE + SSR, and R^2   (bwght ~ cigs) ----
data("bwght")
reg2 <-          #regress bwght on cigs
SST <- ______    # total variation:   squared deviations of bwght from its mean, summed
SSR <- ______    # unexplained:       squared residuals of reg2, summed
SSE <- ______    # explained:         SST - SSR
R2  <- ______    # R^2 = SSE / SST    (~ 0.02: low is normal)

## ================= YOUR TURN =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- ______   # (a) explained sum of squares
R20  <- ______   # (b) R^2
