## Matrix inversion is often a costly computation. Thus, caching the inverse 
## is sometimes beneficial to avoid computing it repeatedly. The following two
## functions can perform the task of caching the inverse of a matrix. For this
## assignment we will assume that the supplied matrix is always invertible.

## This function creates a special "matrix" object that can cache 
## its inverse, m

makeCacheMatrix <- function(x = matrix()) {
    # cached inverse of matrix
    m <- NULL
    
    # get/set methods of the matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get/set methods for matrix inverse
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    #list of functions for matrix
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function computes the inverse of the "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # If inverse has already been computed, return cached inverse
    m <- x$getInverse()
    if(!is.null(m)) {
        print("inverse is cached:")
        return(m)
    }
    
    # If NOT computed, compute and cache
    m <- solve(x$get(),...)
    x$setInverse(m)
    return(m)
}

