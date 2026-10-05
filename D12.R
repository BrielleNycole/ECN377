## ECN 377 - Day 12 STARTER  |  R^2;  units of measurement

library(wooldridge)
?bwght

## SST = SSE + SSR, and R^2 (bwght ~ cigs)
data("bwght")
reg2 <- lm(bwght ~ cigs, data = bwght) 
b0 <- reg2$coefficients[1]
b1 <- reg2$coefficients[2]
SST <- (nrow(bwght)-1) * var(bwght$bwght)        #(n-1)*var(y)
SSR <- (nrow(bwght)-1) * var(reg2$residuals)     #(n-1)*var(residuals)
SSE <- (nrow(bwght)-1) * var(reg2$fitted.values) #(n-1)*var(fitted.values)
R2  <- SSE/SST  
R2 <- summary(reg2)$r.squared
summary(reg2)


## ================= PROBLEMS (your turn) =========================
## A regression has SST = 200 and SSR = 150.
SST0 <- 200
SSR0 <- 150
SSE0 <- ______   # (a) explained sum of squares
R20  <- ______   # (b) R^2
unex <- ______   # (c) fraction of the variation UNEXPLAINED