#Swirl Lesson 7 Matrices and Data Frames Brielle Hamel
library('swirl')
swirl()
BrielleNycole
7
my_vector <- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4,5) #gave my_vector 4 rows and 5 columns
dim(my_vector)
attributes(my_vector)
my_vector
class(my_vector) #proves my_vector is now a matrix array
my_matrix <- my_vector
?matrix
my_matrix2 <- matrix(data=1:20,nrow=4,ncol=5) #creating a matrix
identical(my_matrix, my_matrix2)
patients <- c("Bill","Gina","Kelly","Sean")
cbind(patients, my_matrix) #doesn't work for two different classes of data
my_data <- data.frame(patients, my_matrix) #allows two classes of data to be stored together
my_data
class(my_data) #shows it is a data.frame
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames #assign names to columns
my_data
1
