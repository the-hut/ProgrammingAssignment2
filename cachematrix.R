## The functions in this file allow you to create an object from a matrix
## stored as a list. You can then calculate the inverse of the matrix
## and return the inverse value from cache.

## function makeCacheMatrix
## creates a special "matrix" object that can cache its inverse.
##
## x is a parameter of type matrix

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     
     ## setter to allow the matrix to be reset.
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     
     ## getter that returns the matrix.
     get <- function() x
     
     ## setter to allow the inverse of the matrix to be set.
     setInverse <- function(i) inverse <<- i
     
     ## getter that returns the inverse of the matrix.
     getInverse <- function() inverse
     
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## function cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix.
##
## x is a parameter of type list that needs to be constructed via makeCacheMatrix.

cacheSolve <- function(x, ...) {        
     ## get the value currently stored as the inverse of 'x'.
     inverse <- x$getInverse()
     
     ## if the inverse value exists return it from cache.
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     
     ## if the inverse value does not exist
     
     ## get the value of 'x'
     data <- x$get()
     
     ## get the inverse of 'x'
     inverse <- solve(data, ...)
     
     ## store the inverse of 'x' to cache
     x$setInverse(inverse)
     
     ## return the inverse of 'x'
     inverse
}
