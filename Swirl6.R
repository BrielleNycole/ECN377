#Swirl Lesson 6 Subsetting Vectors Brielle Hamel
6
x #sample list of random numbers and NAs
x[1:10] #select certain subset of elements from the vector like the first 10 elements
4
x[is.na(x)] #shows a list of all NAs
y <- x[!is.na(x)] #shows a list of all not NAs
y
2
y[y>0] #list of all positive elements of y
x[x>0]
x[!is.na(x) & x>0] #list of all not NAs and are positive
x[c(3,5,7)] #subset of 3rd,5th, and 7th elements
x[0] #numeric(0) the 0 element doesn't exist
x[3000] #NA since there are not 3000 elements
x[c(-2,-10)] #subset of all elements but the 2nd and 10th
x[-c(2,10)] #also subset of all elements but the 2nd and 10th
vect <- c(foo = 11, bar = 2, norf = NA)
vect
names(vect)
vect2 <- c(11,2,NA)
names(vect2) <- c("foo","bar","norf")
identical(vect,vect2) #gives you tf if the vectors are equal
1
vect["bar"] #gives second element of vect
vect[c("foo","bar")]
