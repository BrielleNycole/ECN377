## ECN 377 - Day 25 STARTER  |  Two-sided tests; testing beta = a
## ------------------------------------------------------------------
## H1: beta != 0  ->  reject H0 if |t| > c,  c = qt(1 - alpha/2, df)   (alpha/2 in EACH tail)
## H0: beta = a   ->  t = (beta-hat - a) / se, then the same steps (one- or two-sided).
## R's "t value" column always tests beta = 0.
## We start with the two-sided DEMO at the end of D24_starter.R.
## Fill each ______ and ANSWER, then upload to GitHub.
## ------------------------------------------------------------------

library(wooldridge)

##Example 4.3 Two Sided t-test
reg <- lm(colGPA ~ hsGPA + ACT + skipped, data=gpa1)
s <- summary(reg)$coefficients
s
t_skipped <- s[4,3]
c_05 <- qt(1-0.05/2, df= nrow(gpa1)-3-1)
c_01 <- qt(1-0.01/2, df=nrow(gpa1)-3-1)
abs(t_skipped)>c_05 #true so reject

## ---- Example 4.4 (one-sided): does crime rise more than one-for-one with enrollment? ----
## H0: beta = 1 vs H1: beta > 1 for log(enroll)
data("campus")
reg <- lm(log(crime) ~ log(enroll), data = campus)
s <- summary(reg)$coefficients
s
t_enroll <- (s[2,1]-1)/s[2,2] #H0: beta = a -> t = (bhat-a)/se
c_right <- qt(1-0.05, df=nrow(campus)-1-1)     # 5% critical value
## Reject H0 at 5%?                           
t_enroll>c_right #true so reject

## ---- Example 4.5 (two-sided): is the price elasticity of nox equal to -1? ----
## H0: beta = -1 vs H1: beta != -1 for log(nox)
data("hprice2")
reg <- lm(log(price) ~ log(nox) + log(dist) + rooms + stratio, data = hprice2)
s <- summary(reg)$coefficients
s
t_nox <- ______       # t for H0: beta = -1
c_two <- ______       # 5% critical value   (hint: how much in each tail?)
## Reject H0 at 5%?                           ANSWER:

## ================= PROBLEMS (your turn) =========================
## H0: beta = 0 vs H1: beta != 0, alpha = 0.05, df = 40
c3 <- ______   # (a) critical value   (hint: qt(1 - alpha/2, df = 40))
## (a) Is c3 bigger or smaller than the one-sided qt(0.95, df = 40) = 1.68? Why?   ANSWER:

## H0: beta = 0 vs H1: beta != 0, alpha = 0.05, c = 1.98
## (b) t = -3.2 : reject? (use |t|)           ANSWER:
##     t = 1.5 : reject?                      ANSWER:

## beta-hat = 0.8, se = 0.25. H0: beta = 1 vs H1: beta != 1, alpha = 0.05, use c = 2
t_c <- ______   # (c) t-statistic   (hint: (beta-hat - 1) / se)
## (c) Reject?                                ANSWER:

## ---- If time: p-values, a preview of Day 26 ----
## Column 4, Pr(>|t|), is the two-sided p-value for H0: beta = 0. Reject at level alpha if p < alpha.
data("gpa1")
reg <- lm(colGPA ~ hsGPA + ACT + skipped, data = gpa1)
summary(reg)$coefficients          # skipped: p about 0.002 (reject at 5%); ACT: p about 0.17 (fail to reject)
2 * (1 - pt(2.10, df = 18))        # Day 24 demo: t = 2.10 at n = 20 has p = 0.05