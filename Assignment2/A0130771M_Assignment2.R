#Q1. Suppose we have a matrix of 1s and 0s. 
# We want to create a vector as follows: for each row of the matrix, 
# the corresponding element of the vector will be either 1 or 0, 
# depending on whether the majority of the first c elements in the row is 1 or 0. 
# Here c will be a parameter which we want to control. Create a function to perform this task

democraticDimReduce <- function(matr, c) {
  return (apply(matr,1,function(x) (x[1:c])[which.max(tabulate(match(x[1:c], unique(x[1:c]))))]))
}

#for testing
#matrix(c(1,0,1,1,0,0,0,1,1,1,0,0,0,1,1,0,1,1,1,0),nrow=4)