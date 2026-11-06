#Q1 precent change in y 
given <- 2.5 + 0.049*x1 - 0.052*x2
b1 <- 0.049
b2 <- -0.052
x1 <- 2
x2 <- 3
100*(b1*x1+b2*x2)
#Q2 Data
data(wage1)
reg<- lm(log(wage) ~ educ + exper + tenure, data=wage1)
b <- reg$coefficients
#Q2 Percent change in predicted wage with exper rise by 2 and tenure rise by 1
100*(b[3]*2 + b[4]*1) 
#Q3 Predict wage in dollar undoing log
exp(b[1]+b[2]*12+b[3]*10+b[4]*3) 

#Q5 Data
data(gpa1)
reg <- lm(colGPA~ACT+hsGPA, data=gpa1)
x1 <- mean(gpa1$ACT)
x2 <- mean(gpa1$hsGPA)
b <- reg$coefficients
#Q5 compute predicted colGPA at ACT and hsGPA
b[1]+b[2]*(x1)+b[3]*(x2) #or below
mean(gpa1$colGPA)
#Q6 Find sample mean y
given <- 4.2 + 2.4*x1 - 2.5*x2
b0 <- 4.2
b1 <- 2.4
b2 <- (-2.5)
x1 <- 2.6
x2 <- 3.6
yhat <- b0+b1*x1+b2*x2
#Q7 
#Which of these regressions violate MLR3 (no perfect collinearity), so that OLS cannot estimate them? 
#1 wage on income in dollars and income in thousands of dollars 
#2 voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB
#3 a regression with 5 independent variables, estimated on 4 observations 


#Q8 Data
data(vote1)
vote1$totexpend <- vote1$expendA + vote1$expendB
lm(voteA ~ expendA + expendB + totexpend, data=vote1)

#Q11 Find MLR slope
btilde <- b1hat + b2hat*dtilde
btilde <- 0.73
b2hat <- 1.23
dtilde <- (-0.33)
b1hat <- btilde - b2hat*dtilde

#Q12 Data
data(bwght)
reg <- lm(bwght ~ cigs, data=bwght)
summary(reg)
reg$coefficients
#Q13
reg2 <- lm(bwght~cigs+faminc, data=bwght)
reg2$coefficients
#Q14
