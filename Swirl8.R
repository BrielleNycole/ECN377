#Swirl 8 Logic Brielle Hamel
TRUE == TRUE #== is the equality operator
(FALSE == TRUE) == FALSE #true
6 == 7 #false
6 < 7 #true
10 <= 10 #true
1
2
# != means not equal
5 != 7 #true
!5 == 7 #opposite of whether 5 is equal to 7
1  # !(0 >= -1) is False
2  # (TRUE != FALSE) == !(6 == 7) is True
FALSE & FALSE # false
TRUE & c(TRUE,FALSE,FALSE) # true false false
TRUE | c(TRUE,FALSE,FALSE) # true true true
TRUE || c(TRUE,FALSE,FALSE) #true
5 > 8 || 6 != 8 && 4 > 3.9 #true
    #First: 6 does not equal 8 and 4 is greater than 3.9 so TRUE
    #Second: 5 isn't greater than 8 so FALSE 
    #Third: FALSE || TRUE so everything is TRUE
2 #TRUE && FALSE || 9 >= 4 && 3 < 6 is TRUE
2 #FALSE && 6 >= 6 || 7 >= 8 || 50 <= 49.5 is FALSE
isTRUE(6>4) #true
1 #!isTRUE(4 < 3) is TRUE
identical('twins','twins')
1 #identical(5 > 4, 3 < 3.1) is TRUE
xor(5==6,!FALSE) #true, xor acts as exclusive OR
4 #xor(4 >= 9, 8 != 8.0) is FALSE
ints <- sample(10)
ints
ints > 5 #tells us in order which are true and false
which(ints>7) #tells us which are greater than 7
4
any(ints<0) #False, none less than 0
all(ints>0) #true
3 #any(ints == 10) is true
1
