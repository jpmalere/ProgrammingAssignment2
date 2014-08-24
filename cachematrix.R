## These functions invert a matrix one time and cache the result
## if the calculation has already been done before.

## The function "makeCacheMatrix" creates a special "vector" which 
## is a list containing functions to:
## get the value of the matrix ("get")
## set the value of the inverted matrix ("setsolve")
## get the value of the inverted matrix ("getsolve")

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## gets the value of the original (non-inverted) matrix
    get <- function() x
    ## sets the value of the variable m which stores the inverted matrix
    setsolve <- function(solvedMatrix) m <<- solvedMatrix
    ## gets the value of the variable m 
    getsolve <- function() m
    list(get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The function "cacheSolve" calculates the inverse of a matrix.
## It checks if the inverse has already been calculated.
## If so, it gets the inverted matrix from the cache and skips the
## calculation.
## Otherwise, it calculates the inverted matrix of the data.

cacheSolve <- function(x, ...) {
    ## gets the value of the inverted matrix
    m <- x$getsolve()
    ## if the inverted matrix already exists, gets from the cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## otherwise, gets the original (non-inverted) matrix
    data <- x$get()
    ## calculates the inverted matrix (solve function)
    m <- solve(data, ...)
    ## set the value of variable m, where the inverted matrix is stored
    x$setsolve(m)
    m    
}
