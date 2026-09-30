## ECN 377 - Day 12 STARTER  |  R^2;  units of measurement

library(wooldridge)
?bwght

## SST = SSE + SSR, and R^2 (bwght ~ cigs)
data("bwght")
reg2 <- lm(bwght ~ cigs, data = bwght) 
b0 <- reg2$coefficients[1]
b1 <- reg2$coefficients[2]
SST <- (nrow(bwght)-1) * var(bwght$bwght)
SSR <- (nrow(bwght)-1) * var(reg2$residuals)
SSE <- (nrow(bwght)-1) * var(reg2$fitted.values)
R2  <- SSE/SST  

## ---- Units of measurement   (Example 2.3: salary on roe, salary in $1000s) ----
data("ceosal1")
reg3 <- ______        # regress salary on roe                         (963.19 and 18.50)
ceosal1$salarydol <- ______   # salary in DOLLARS:  Y x 1000          (hint: 1000 * the salary column)
______                # regress salarydol on roe: both estimates x 1000?  (look at $coefficients)
ceosal1$roedec <- ______      # roe as a DECIMAL:   X x 1/100         (hint: the roe column / 100)
______                # regress salary on roedec: slope x 100, intercept unchanged?
______                # R^2 of reg3 -- same for all three regressions  (hint: summary(...)$r.squared)

## ================= PROBLEMS (your turn) =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- ______   # (a) explained sum of squares
R20  <- ______   # (b) R^2
unex <- ______   # (c) fraction of the variation UNEXPLAINED