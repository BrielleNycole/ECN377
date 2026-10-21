#Master Script

xj <- c(1,2,4,5)
yj <- c(2,6,3,7)          
pj <- c(0.2,0.3,0.4,0.1)  
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


#Y = b0 + b1*X + U
#E[Y|X=x]= b0 + b1*x
x <- c(1,2,5,10,11,15)
y <- c(5,8,6,10,8,14)
b1    <- cov(x,y) / var(x)   #OLS slope
b0    <- mean(y) - b1 * mean(x)   #OLS intercept
c(b0 = b0, b1 = b1)
yhat <- b0 + b1 * x #prediction
uhat = y - yhat #find residual from observation from last line
SSR <- sum((uhat)^2) #find SSR

#Var(aX+b) = a^2(VarX)

#Var(aX+bY) = (a^2)(VarX) - (b^2)(VarY) + 2ab(Cov(x,y))

#E[aX + bY] = a*E[X] + b*E[Y] 
#E[aX + b] = a*E[X] + b

#Cov(a1*X + b1, a2*Y + b2) = a1*a2*Cov


#uhat=yi-yhat
#R2=SSE/SST
#R2=1-(SSR/SST)
#SST=SSE+SSR
#sum(uhat)=0
#delta_yhat = b1 * delta_x

x <- c(5,8,3)
y <- c(15,12,1)
b1 <- #  
  b0 <- #   
  fitted_vals <- b1 + b0*x
resids <- y - fitted_vals
SSR <- sum(resids^2) #sum of squared residuals

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
install.packages('wooldridge')
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
yhat <- b0 + b1 * 16
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

-0.7+2.3+2.3+1.8+
  mean(sum(-0.7, 2.3, 2.3, 1.8, -5.7))



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

#Table2.3 Regression Models
#level-level
changeY=b1*changeX
#level-log
changeY=(b1/100)*perchangeX
#log-level
perchangeY=100*b1*changeX
#log-log
perchangeY=b1*perchangeX


#log-level data set
lm(log(salary)~roe,data=ceosal1)
b1 <- lm(log(salary)~roe,data=ceosal1)$coefficients[2]
100*b1*2
#level-log regression
reg <- lm(salary~log(sales),data=ceosal1)
b1 <- reg$coefficients[2]
(b1/100)*10

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
n <- 8
SSR <- 462
SSTx <- 21
sigma <- sqrt(SSR/(n-2))
seb1 <- sigma/sqrt(SSTx)
seb1
#find standard error of the regression (sigma)
data('wage1')
reg <- lm(wage  ~ educ, data=wage1)
sqrt(sum(reg$residuals^2)/(nrow(wage1)-2))
summary(reg)$sigma
sum((wage1$educ - mean(wage1$educ))^2)

#PS8
#Q14
#when x is mulitplied by c, b1 us mulitplied by 1/c and b0 does not change
#dividing by k is multiplying by c=1/k

#15
#In a regression of y on x, every value x is mulitplied by 10 so the slope is divided by 10, the intercept does not change, and R2 does not change
.76*1000
#17 
#log-level regression 

100*0.059*3
0.82*(-9)
(33.3/100)*19
