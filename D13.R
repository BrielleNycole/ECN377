## ECN 377 - Day 13 STARTER  |  Units of measurement;  logs & functional form

## Logs: wrap a variable in log() inside the formula; read the slope with Table 2.3.


library(wooldridge)

## ---- Units of measurement   (Example 2.3: salary on roe, salary in $1000s) ----
data("ceosal1")
reg3 <- lm(salary ~ roe, data = ceosal1)        # regress salary on roe                         (963.19 and 18.50)
ceosal1$salarydol <- ceosal1$salary * 1000   # salary in DOLLARS:  Y x 1000          (hint: 1000 * the salary column)
reg4 <- lm(salarydol ~ roe, data =ceosal1)                # regress salarydol on roe: both estimates x 1000?  (look at $coefficients)
ceosal1$roedec <- ceosal1$roe / 100      # roe as a DECIMAL:   X x 1/100         (hint: the roe column / 100)
reg5 <- lm(salary ~ roedec, data = ceosal1)                # regress salary on roedec: slope x 100, intercept unchanged?
summary(reg3)$r.squared                # R^2 of reg3 -- same for all three regressions  (hint: summary(...)$r.squared)

## ---- Demo: log-level (Example 2.10) ----
## What you're learning: log(y) on x  ->  slope is a PERCENT change in y.
data("wage1")
reg <- lm(log(wage) ~ educ, data = wage1)      # 0.584, 0.083 (~8.3% per year)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]

## ---- Demo: log-log / constant elasticity (Example 2.11) ----
## What you're learning: log(y) on log(x)  ->  slope is an ELASTICITY.
lm(log(salary) ~ log(sales), data = ceosal1)$coefficients   # 4.822, 0.257

## ================= PROBLEMS (your turn) =========================
## log-level model:  log(wage)-hat = 0.58 + 0.08*educ
b1 <- 0.08
pct_1yr <- ______   # (a) % change in wage from +1 year of school  (hint: 100*b1)
pct_4yr <- ______   # (b) % change from +4 years                   (hint: 100*b1*4)
## (c) In a log-log model, the slope is called a(n) ______  (comment)