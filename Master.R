## ECN 377 - R RECIPE CARD  |  question type -> the R lines that answer it

library(wooldridge)

xj <- c(1,4,5,7)
yj <- c(3,6,5,2)          
pj <- c(0.3,0.2,0.1,0.4)  
EX  <- sum(xj*pj)            
EY  <- sum(yj*pj) # E[Y | X = x]          
EXY <- sum(xj*yj*pj)        
Cov <- EXY - EX*EY  
EX2 <- sum((xj^2)*pj)
EY2 <- sum((yj^2)*pj)
VarX <- EX2 - EX^2           
VarY <- EY2 - EY^2 # Var(Y | X = x)          
sdX <- sqrt(VarX)            
sdY <- sqrt(VarY)            
Cor <- Cov / (sdX*sdY)
Cov/VarX #population regression slope beta1

#5 
VarX <- 9
VarY <- 8
Cov <- -4.3
(3^2)*(VarX) + ((-2)^2)*(VarY) + 2*(3)*(-2)*(Cov)

#Var(aX+b) = a^2(VarX)
#Var(aX+bY) = (a^2)(VarX) + (b^2)(VarY) + 2ab(Cov(x,y))
#sd(aX + b) = |a| sd(x)
#E[aX + bY] = a*E[X] + b*E[Y] 
#E[aX + b] = a*E[X] + b

#Cov(a1*X + b1, a2*Y + b2) = a1*a2*Cov


#Y = b0 + b1*X + U
#E[Y|X=x]= b0 + b1*x

#==============================OLS by hand================================
x <- c(2,4,7,8,9)
y <- c(3,4,6,9,2)
b1    <- cov(x,y) / var(x)        #OLS slope
b0    <- mean(y) - b1 * mean(x)   #OLS intercept
yhat <- b0 + b1 * x               #fitted values
uhat = y - yhat                   #residuals from fitted values
uhat[4]                           #residual at observation 4

#===============================OLS data==============================
library(wooldridge)
data("wage1")
reg <- lm(wage ~ tenure, data=wage1)
reg$coeffients
b0 <- reg$coefficients[[1]]             
b1 <- reg$coefficients[[2]]  
reg$residuals[9] #residual at observation 9

nrow(wage1)                             # sample size
mean(wage1$wage)                        # a column with $
reg1 <- lm(wage ~ tenure, data = wage1) # y ~ x, data = the dataset
reg1$coefficients
reg1$residuals[2]                       # residual of the worker in row 2
mean(wage1$wage[wage1$educ == 12])      # average wage among workers with educ = 12

yhat <- b0 + b1 * x # predicted y at x = 6  (fitted value)
fitted_vals <- yhat
b1 * 4      # predicted CHANGE in y when x rises by 4
reg$fitted.values[9]      # fitted value of observation 3
reg$residuals[9]          # residual of observation 3 = actual - fitted
y[3] - (b0 + b1 * x[3])   # the same residual, by hand
uhat = y - yhat #find residual from fitted value
uhat <- resids
-sum(c(2.7,-2.3,-2.1,-2.5)) # missing residual = minus the sum of the others
SSR <- sum(uhat^2) #find SSR
SSR <- sum(resids^2) 


#OLS PROPERTIES 
sum(reg$residuals) = 0                      
sum(x * reg$residuals) = 0                 
b0 + b1 * mean(x) = mean(y) #(xbar, ybar) is always on the OLS line

## ================= 9. SST, SSE, SSR, R^2  (uses y, reg from section 6) =================
SST <- sum((y - mean(y))^2)             # total variation
SSR <- sum(reg$residuals^2)             # unexplained
SSE <- SST - SSR                        # explained
SSE / SST                               # R^2 = 1 - SSR / SST
summary(reg)$r.squared                  # check
100 * summary(reg)$r.squared            # "what percentage is explained?" -> a percent
SSE <- 187
R2 <- 0.39
SST<-SSE/R2
SST-SSE
#R2=SSE/SST
#R2=1-(SSR/SST)
#SST=SSE+SSR
#sum(uhat)=0
#delta_yhat = b1 * delta_x

n <- 25
s2y <- 16
(n - 1) * s2y                           # SST = (n - 1) * sample variance of y
SSE0 <- 90
R20 <- 0.3
SSE0 / R20 - SSE0                       # SSR, from SSE and R^2  (SST = SSE / R^2)



data("bwght")
reg2 <- lm(bwght ~ cigs, data = bwght) 
b0 <- reg2$coefficients[1]
b1 <- reg2$coefficients[2]
SST <- (nrow(bwght)-1) * var(bwght$bwght)        #(n-1)*var(y)
SSR <- (nrow(bwght)-1) * var(reg2$residuals)     #(n-1)*var(resids)
SSE <- (nrow(bwght)-1) * var(reg2$fitted.values) #(n-1)*var(fitted_vals)
R2  <- SSE/SST  
R2 <- summary(reg2)$r.squared
summary(reg2)

library(wooldridge)
data("ceosal1")
reg <- lm(salary ~ roe, data = ceosal1)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]
mean <- mean(ceosal1$roe)
cor <- cor(ceosal1$salary,ceosal1$roe)
data("wage1")
reg2 <- lm(wage ~ tenure, data=wage1)
b1 <- reg2$coefficients[2]
b0 <- reg2$coefficients[1]
yhat <- b0 + b1 * 9
fitted_vals <- b1 + b0*(wage1$wage)
resids <- (wage1$tenure) - fitted_vals
data("bwght")
reg2 <- lm(bwght ~ faminc, data = bwght) 
b0 <- reg2$coefficients[1]
b1 <- reg2$coefficients[2]
SST <- (nrow(bwght)-1) * var(bwght$bwght)        #(n-1)*var(y)
SSR <- (nrow(bwght)-1) * var(reg2$residuals)     #(n-1)*var(resids)
SSE <- (nrow(bwght)-1) * var(reg2$fitted.values) #(n-1)*var(fitted_vals)
R2  <- SSE/SST  
R2 <- summary(reg2)$r.squared
b1*10

(19-1)*27
mean(wage1$wage)
sd(wage1$educ)
reg <- lm(wage ~ educ, data = wage1)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]

mean(wage1$wage[wage1$educ == 16])

SSE <- 52
R2 <- 0.77
SST <- SSE/R2
SSR <- SST - SSE
SSR
(1-R2)*SST

#========================Table2.3 Regression Models=====================
#level-level
changeY=b1*changeX
#level-log
changeY=(b1/100)*perchangeX
#log-level
perchangeY=100*b1*changeX
#log-log
perchangeY=b1*perchangeX
##   level-level:  change in y   = b1 * (change in x)
##   log-level:    % change in y = 100 * b1 * (change in x)
100*0.016*(-3)
##   level-log:    change in y   = (b1 / 100) * (% change in x)
##   log-log:      % change in y = b1 * (% change in x)

#log-level data set
lm(log(salary)~roe,data=ceosal1)
b1 <- lm(log(salary)~roe,data=ceosal1)$coefficients[2]
100*b1*2
#level-log regression
reg <- lm(salary~log(sales),data=ceosal1)
b1 <- reg$coefficients[2]
(b1/100)*10
#log-log data set
library(wooldridge)
data('bwght')
reg4 <- lm(log(bwght) ~ log(faminc), data = bwght)   
b1 <- reg4$coefficients[[2]] # elasticity: % change in salary from +1% sales
b1*15


#PS9
#14 find sd(b1)
sigma <- 7
x <- c(2,3,4,8,9)
SSTx <- sum((x-mean(x))^2)
sdb1 <- sigma/sqrt(SSTx)
sdb1
#15 find sigma2
n <- 48
SSR <- 183
sigma2 <- SSR/(n-2)
sigma^2
#16 seb1 (standard error of b1)
n <- 13
SSR <- 53
SSTx <- 21
SSTx <- (n-1)*(0.9^2) #(n-1)*(Sx^2)
SSTx <- sum((c(2,3,5,6,11)-mean(c(2,3,5,6,11)))^2)
sigma <- sqrt(SSR/(n-2))
sigma <- 7.9
seb1 <- sigma/sqrt(SSTx)
seb1
#find standard error of the regression (seb1)
library(wooldridge)
data('ceosal1')
reg <- lm(salary  ~ roe, data=ceosal1)
SSR <- sum(reg$residuals^2)
sigma <- sqrt(SSR/(nrow(ceosal1)-2))
summary(reg)
SSTx <- sum((ceosal1$roe - mean(ceosal1$roe))^2)
seb1 <- sigma/sqrt(SSTx)
#PS8
#Q14
#when x is mulitplied by c, b1 us mulitplied by 1/c and b0 does not change
#dividing by k is multiplying by c=1/k

#15
#In a regression of y on x, every value x is mulitplied by 10 so the slope is divided by 10, the intercept does not change, and R2 does not change
.76*1000
#17 
#log-level regression 

#===================Multiple Linear Regression========================
#Find predicted value y
8 + 1.8(x1) + 1.3(x2)
b0 <- 2
b1 <- 2.2
x1 <- 6
b2 <- -0.6
x2 <- 4
y <- b0 + b1*x1 + b2*x2
#Find change in y
changeY <- b1*x1 + b2*x2
#Using data to find predicted y
data('wage1')
reg <- lm(wage ~ educ + exper + tenure, data = wage1)
b0 <- reg$coefficients[1] #intercept
b1 <- reg$coefficients[2] #educ
b2 <- reg$coefficients[3] #exper
b3 <- reg$coefficients[4] #tenure
y <- b0 + b1*16 + b2*5 + b3*2
p <- b2*10
#Using data to find percent change (log-level)
data(ceosal1)
reg <- lm(log(salary) ~ log(sales) + roe, data=ceosal1)
b0 <- reg$coefficients[1] #intercept
b1 <- reg$coefficients[2] #log sales
b2 <- reg$coefficients[3] #roe
#find perchange when change in roe is +10
perchange <- 100*b2*10 
#find predicted y with log function
exp(b0 + b1*log(5000) + b2*15)


#compute b2*d2
reg1 <- lm(wage ~ educ + exper, data=wage1)
reg2 <- lm(exper ~ educ, data=wage1)
b2 <- coefficients(reg1)['exper']
d1 <- coefficients(reg2)['educ']
b2*d1

#Suppose you could draw 1,000 different random samples from the same population and compute 
#b1 in each one. If OLS is unbiased (SLR1–SLR4 hold), which statement is correct?
     #The average of the 1,000 estimates is close to b1