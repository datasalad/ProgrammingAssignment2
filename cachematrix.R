## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. Below is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() x
    set <- function(data) {
        x <<- data
        inv <<- NULL
    }
    
    getInverse <- function() inv
    setInverse <- function(i) inv <<- i
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(is.null(inv) == FALSE) {
        message("Getting data from the cache...")
        return(inv)
    }
    
    mtrx <- x$get()
    
    ## Computing the inverse of a square matrix
    inv <- solve(mtrx, ...)
    x$setInverse(inv)
    inv
}