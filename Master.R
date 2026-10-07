#Master Script

xj <- c(0, 0, 1, 1)         
yj <- c(0, 1, 0, 1)          
pj <- c(0.4, 0.1, 0.1, 0.4)  
EX  <- sum(xj*pj)            
EY  <- sum(yj*pj)           
EXY <- sum(xj*yj*pj)        
Cov <- EXY - EX*EY          
EX2 <- sum((xj^2)*pj)
EY2 <- sum((yj^2)*pj)
VarX <- EX2 - EX^2           
VarY <- EY2 - EY^2           
sdX <- sqrt(VarX)            
sdY <- sqrt(VarY)            
Cor <- Cov / (sdX*sdY)

#Y = b0 + b1*X + U
#E[Y|X=x]= b0 + b1*x
x <- c(2,5,2)
y <- c(12,7,9)
b1    <- cov(x,y) / var(x)   #OLS slope
b0    <- mean(y) - b1 * mean(x)   #OLS intercept
c(b0 = b0, b1 = b1)
yi <- b0 + b1 * xi #prediction
uhat = yi - yhat

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

mean(wage1$wage)
sd(wage1$educ)
reg <- lm(wage ~ educ, data = wage1)
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]

mean(wage1$wage[wage1$educ == 16])
