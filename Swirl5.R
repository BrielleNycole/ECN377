#Swirl Lesson 5 Missing Values Brielle Hamel
swirl()
BrielleNycole
x <- c(44,NA,5,NA)
x*3 #=132,NA,15,NA
y <- rnorm(1000) #contains 1000 draws from a standard normal distribution
z <- rep(NA,1000) #1000 NAs 
my_data <- sample(c(y,z),100) #select 100 elements at random from y and z
my_na <- is.na(my_data) #tf for if each element is NA
my_na
my_data == NA
sum(my_na) #counts the total number of TRUEs in my_na
my_data
0/0 #NaN (not a number)
Inf-Inf #=NaN
