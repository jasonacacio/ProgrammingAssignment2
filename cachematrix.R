## Put comments here that give an overall description of what your functions do:
        ## Below are two functions, taken from the assignment sample code, which
        ## retrieves a cached solution to a matrix inversion instead of the
        ## recalculation of each instance of the same matrix when invoked.

## Write a short comment describing this function:
        ## This function contains a list with functions nested inside  
        ## that sets (caches) and retrieves the values of the matrix 
        ## and its inversion. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function:
        ## cacheSolve is a function that checks if a matrix from makeCacheMatrix
        ## has been solved before. If so, it prints that cached output instead of 
        ## solving the matrix (using solve()) once again.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("Loading Cached Data... Done!")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## NB: The function will give an error message when the matrix inputted is
## singular. (e.g. a 2x2 matrix with all 1's as values; determinant = 0)
## This is due to the laws of mathematics, and is NOT a programming bug.

## ==========

## Sample of usage through the command line interface after declaring the above:
## DO NOT RUN anything per assignment instructions:

## matrix1 <- makeCacheMatrix(matrix(c(1:4),2,2)) ## makes a 2x2 matrix with the numbers 1 to 4

## matrix2 <- makeCacheMatrix(matrix(rep(1:1),2,2)) ## makes a 2x2 matrix containing 1 on all values

## cacheSolve(matrix1) ## this outputs the inverse of matrix1

## cacheSolve(matrix1) ## will display a message and load the previous solution to matrix1

## cacheSolve(matrix2) ## will display an error message due to matrix singularity