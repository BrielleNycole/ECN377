## ECN 377 - Day 19 STARTER  |  Example 3.1;  omitted-variable bias (3.23)

library(wooldridge)
data(gpa1)
## ---- Example 3.1 (gpa1) ----
## SLR: colGPA on ACT (save the ACT slope)
regSLR <- lm(colGPA ~ ACT, data=gpa1)
b1 <- regSLR$coefficients[2]

## MLR: add hsGPA; change in colGPA-hat from +10 ACT, hsGPA fixed
regMLR <- lm(colGPA ~ ACT + hsGPA, data=gpa1)
b1hat <- regMLR$coefficients[2] #ACT slope
b2hat <- regMLR$coefficients[3] #hsGPA slope

## the regression plane (first time only, in the Console: install.packages(c("car", "rgl")); Mac: rgl also needs XQuartz)
library(car)
scatter3d(colGPA ~ ACT + hsGPA, data = gpa1, fit = "linear")   # drag to rotate: flat in ACT, steep in hsGPA

## ---- Omitted-variable bias (Example 3.3, gpa1) ----
## delta1: regress hsGPA (omitted) on ACT (included)
reg_delta <- lm(hsGPA ~ ACT, data=gpa1)
d1 <- reg_delta$coefficients[2]
b1hat <- regMLR$coefficients[2] #ACT slope
b2hat <- regMLR$coefficients[3] #hsGPA slope
b1tilde <- regSLR$coefficients[2]

## check: SLR slope = MLR slope + (hsGPA slope) * delta1
            b1tilde = b1hat     + b2hat         * d1

## ================= PROBLEMS (your turn) =========================
## (a) colGPA-hat = 1.29 + 0.0094*ACT + 0.453*hsGPA
pred_a <- ______    # (a) colGPA at ACT = 30, hsGPA = 3.0   (hint: 1.29 + 0.0094*30 + 0.453*3.0)
## (b) b2 > 0 and delta1 > 0: SLR slope biased up or down?   ANSWER:
## (c) b2 > 0 and delta1 < 0: which way?                     ANSWER:
## (d) When is there NO omitted-variable bias?               ANSWER:

