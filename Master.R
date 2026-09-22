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
x <- c(1, 2, 3)
y <- c(2, 2, 5)
b1    <- cov(x,y) / var(x)   #OLS slope
b0    <- mean(y) - b1 * mean(x)   #OLS intercept
c(b0 = b0, b1 = b1)
pred4 <- b0 + b1 * 4    # (c) predicted y at x = 4

#Var(aX+b) = a^2(VarX)

#Var(aX+bY) = (a^2)(VarX) - (b^2)(VarY) + 2ab(Cov(x,y))

#E[aX + bY] = a*E[X] + b*E[Y] 
#E[aX + b] = a*E[X] + b

#Cov(a1*X + b1, a2*Y + b2) = a1*a2*Cov