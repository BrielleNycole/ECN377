#Swirl Lesson 10 lapply and sapply
swirl()
BrielleNycole
10
head(flags) #view head/ first few lines of dataset
dim(flags) #view dimensions of dataset
class(flags) #tells what format the variables have been stored
cls_list <- lapply(flags, class) #view format of variables per column
cls_list
class(cls_list)
as.character(cls_list)
cls_vect <- sapply(flags,class) #automate the lapply process
class(cls_vect)
sum(flags$orange) #number of countries with orange in their flag
flag_colors <- flags[, 11:17]
head(flag_colors)
lapply(flag_colors, sum) #number of flags with each color list format
sapply(flag_colors, sum) #number of flags with each color simplified 
sapply(flag_colors, mean)
flag_shapes <- flags[, 19:23]
lapply(flag_shapes, range) #range of times a certain shape appears
shape_mat <- sapply(flag_shapes, range) 
shape_mat
class(shape_mat)
unique(c(3,4,5,5,5,6,6)) #returns vector without duplicates
unique_vals <- lapply(flags, unique)
unique_vals
sapply(unique_vals, length)
sapply(flags, unique)     
lapply(unique_vals, function(elem) elem[2])
2
