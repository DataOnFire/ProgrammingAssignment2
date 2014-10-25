## Coursera R Programming
## Programming Assignment 2: Lexical Scoping
##
## When using this set of functions you will be able to compute the 
## inverse of a matrix and cache it for future reference. Computing
## the inverse of a matrix is a costly computation and using this set of 
## functions to cache this result may provide some benefit instead of
## repeatedly performing this calculation.

## The makeCacheMatrix function creates a special matrix object that 
## will cache its inverse. This function is used by cacheSolve.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## The cacheSolve function will return the inverse of the 
## matrix returned by makeCacheMatrix. It will check to see if the 
## inverse has already been calculated and has not changed. If this
## is the case, the cacheSolve function will retrieve the inverse 
## from the cache. If not it will compute it, and then cache it for
## future use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
