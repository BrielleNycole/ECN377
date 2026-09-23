## ECN 377 - Day 10 STARTER  |  lm(), fitted values, residuals
## ------------------------------------------------------------------
## lm() is the shortcut for the OLS line we derived by hand on Day 9.
## Fill each ______ as we go, then COMMIT + PUSH.
## ------------------------------------------------------------------

library(wooldridge)
data("wage1")

## ---- Fit the regression:  wage on educ ----
reg <- lm(wage ~ educ, data = wage1)        # fit the OLS line:  lm(wage ~ educ, data = wage1)
reg$coefficients     # look: the two estimates  (-0.90 and 0.54, same as Day 9)

## ---- Pull the coefficients out of reg ----
b0 <- reg$coefficients[1]         # intercept
b1 <- reg$coefficients[2]         # slope

## ---- Fitted values (predictions) and residuals (misses) ----
yhat <- reg$fitted.values       # fitted values:  reg$fitted.values   (yhat = b0 + b1*educ)
uhat <- reg$residuals       # residuals:      reg$residuals        (uhat = wage - yhat)
SSR  <- sum(uhat^2)       # sum of squared residuals:  sum(uhat^2)

## ================= YOUR TURN =========================
## Estimated line:  wage-hat = -0.90 + 0.54*educ.   A person: educ = 12, actual wage = 9.0
b0 <- -0.90
b1 <- 0.54
yhat <- ______   # (a) fitted value at educ = 12
uhat <- ______   # (b) residual = actual wage (9.0) - fitted
## (c) Over- or under-predicted? (comment)  ANSWER: