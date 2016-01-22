## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "vector", which is really a list containing a function to 
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverseMatrix <<- solve
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse matrix of the special "vector" 
## created with the above function. However, it first checks to see if the inverse matrix
## has already been calculated. If so, it gets the inverse matrix from the cache and skips
## the computation. Otherwise, it calculates the inverse matrix of the data and sets the
## value of the inverse matrix in the cache via the cacheSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
