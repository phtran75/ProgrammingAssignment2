## Put comments here that give an overall description of what your
## functions do
## Assignment: Caching the Inverse of a Matrix
## Our assignment is to write a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## "inv" is used to store the inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()

        ## In this case inverse has NOT already been calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## In this case inverse has already been calculated
        message("do NOT getting cached data")
        data <- x$get()

        ## We assume that the matrix supplied is always invertible.
        ## if X is a square invertible matrix, then solve(X) returns its inverse
        inv <- solve(data)
        
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
