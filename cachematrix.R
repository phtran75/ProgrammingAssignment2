##
## Assignment: Caching the Inverse of a Matrix
## Our assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## "inv" is used to store the inverse
        inv <- NULL

        ## sets the value of the matrix
        ## the <<- operator  can be used to assign a value to an object 
        ## in an environment that is different from the current environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        ## gets the value of the matrix
        get <- function() x
        
        ## sets the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        ## gets the value of the inverse
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()

        ## In this case inverse has already been calculated (inv <> NULL)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## In this case inverse has NOT already been calculated (inv == NULL)
        message("do NOT get cached data")
        data <- x$get()

        ## We assume that the matrix supplied is always invertible.
        ## if X is a square invertible matrix, then solve(X) returns its inverse
        inv <- solve(data)
        
        ## uses setinverse
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
