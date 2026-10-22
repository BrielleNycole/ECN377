#Swirl Lesson 9 Functions Brielle Hamel
library(swirl)
swirl()
BrielleNycole
9

Sys.Date()
mean(c(2,4,5))
submit() #boring_function <- function(x) {x}
boring_function('My first function!')
boring_function
submit() #my_mean <- function(my_vector) {sum(my_vector)/length(my_vector)}
my_mean(c(4,5,10))
submit() #remainder <- function(num, divisor=2) {num %% divisor}
remainder(5)
remainder(11,5)
remainder(divisor = 11, num = 5)
remainder(4,div=2)
args(remainder) #shows arguments for the funtion
submit() #evaluate <- function(func, dat) {func(dat)}
evaluate(sd,c(1.4,3.6,7.9,8.8))
evaluate(function(x){x+1},6)
evaluate(function(x){x[1]},c(8,4,0))
evaluate(function(x){x[length(x)]},c(8,4,0))
?paste
paste("Programming","is","fun!")
submit() #telegram <- function(...) {paste("START",...,"STOP")}
telegram("Will","this","work?")
submit() #mad_libs <- function(...){args <- list(...), place <- args[["place"]], adjective <- args[["adjective"]], noun <- args[["noun"]], paste("News from", place, "today where", adjective, "students took to the streets in protest of the new", noun, "being installed on campus.")}
mad_libs("school","funny","curtain")
submit() #"%p%" <- function(left,right){paste(left,right,sep = " ")}
'I'%p%'love'%p%'R!'
2
