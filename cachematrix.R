## Put comments here that give an overall description of what your
## functions do

# the first function can make a list of four functions:
# 1. input a matrix data
# 2. return the matrix data
# 3. input a inverse data
# 4. return the inverse data
# we can command "a <- <the function>", then a has the list of the four functions.

# the second funtion can compute or return the value of the inverse data of a given variable,
# depending on whether the inverse value was calculated already or not (i.e., null)




## Write a short comment describing this function

# this function makes a list of four functions: input/return matrix data, and input/return inverse data

makeCacheMatrix <- function(matrix_data = matrix()) {           # a matrix has two information:
                                                                # (1) matrix_data and (2) inverse_data
        inverse_data <- NULL                                    # inverse_data is initially set NULL.
        
        input_matrix <- function(x) {  
                matrix_data <<- x                               # we can input a matrix data by using input_matrix function
                inverse_data <<- NULL                           # when the matrix data is given, inverse_data is set NULL
        }
        
        return_matrix <- function() matrix_data                 # we can return the matrix data by using return_matrix function
        
        input_inverse <- function(y) inverse_data <<- y         # we can input a inverse data by input_inverse function
        
        return_inverse <- function() inverse_data               # we can return the inverse data by useing return_inverse function
        
        list(input_matrix = input_matrix, return_matrix = return_matrix,           # make a list of these four functions
             input_inverse = input_inverse, return_inverse = return_inverse)
}




## Write a short comment describing this function

# this function can compute or return the inverse data of a matrix, contained in a list

cacheSolve <- function(matrix_data = matrix(), ...) {           # a variable for this function can be a list, defined above
        if(!is.null(matrix_data$return_inverse())) {            # check whether the returned inverse data of the variable
                message("getting cache data")                   # is NULL or not: if not,
                return(matrix_data$return_inverse())            # return the inverse value by return_inverse function
        } else {
                inverse_data <- solve(matrix_data$return_matrix(), ...) # if yes, compute the inverse data of the variable,
                                                                        # by returning the matrix data
                matrix_data$input_inverse(inverse_data)                 # caching the inverse data by input_inverse function                         
                return(inverse_data)                                    # return the inverse value
        }                                                                
}