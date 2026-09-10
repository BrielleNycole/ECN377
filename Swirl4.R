#Swirl Lesson 4 Brielle Hamel
#Vectors
#1 Logical Vectors: contain True, False, and NA
num_vect <- c(0.5,55,-10,6)
tf <- num_vect<1
2 #tf shows which numbers are less than 1
tf #TRUE FALSE TRUE FALSE
num_vect>=6 #FALSE TRUE FALSE TRUE
# A|B means at least one is true aka union
# A&B means both are true aka intersection
1
1
2
#2 Character Vectors: contain letters/words
my_char <- c('My','name','is')
my_char
paste(my_char, collapse = " ") #putting " " means separating with one space
my_name <- c(my_char, "Brielle")
my_name
paste(my_name, collapse = " ") #"My name is Brielle"
paste("Hello", "world!", sep = " ") #"Hello world!"
paste(1:3, c("X", "Y", "Z"), sep = "") #"1X" "2Y" "3Z"
paste(LETTERS, 1:4, sep = "-") #"A-1" "B-2" "C-3" "D-4" "E-1" "F-2" "G-3" "H-4" "I-1" J-2" "K-3" "L-4" "M-1" "N-2" "O-3" "P-4" "Q-1" "R-2""S-3" "T-4" "U-1" "V-2" "W-3" "X-4" "Y-1" "Z-2"
2
