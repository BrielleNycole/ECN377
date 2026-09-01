#Swirl Lesson 3 Brielle Hamel
library("swirl")
swirl()
BrielleNycole
3
#Sequence of numbers 
1:20 #integer between 1 and 20
pi:10 #sequence of real numbers starting with pi and increasing in increments of 1 with an upper limit of 10
15:1 #count backwards from 15 to 1 
?":"
seq(1,20) #integers from 1 to 20
seq(0,10, by=0.5) #counting from 1 to 10 incremented by 0.5
seq(5,10, length=30) #sequence of 30 numbers between 5 and 10
my_seq <- seq(5,10, length=30)
length(my_seq)
1:length(my_seq) #creates a sequence of integers from 1 to N, beingg the length of my_seq
seq(along.with=my_seq) #same as last line
seq_along(my_seq) #also same as last line
rep(0,times=40) #vector of 40 zeros
rep(c(0,1,2),times=10) #vector (0,1,2) repeated 10 times
rep(c(0,1,2),each=10) #vector of 10 zeros, 10 ones, and 10 twos
