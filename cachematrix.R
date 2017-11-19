## Script contains functions to cache matrix inversion

## Creates wrapper object for matrix, which contains 4 functions:
## get() - get original matrix
## set() - set new matrix
## setSolve() - set inversed matrix
## getSolve() - get stored inversed matrix
## 
## Args:
##   x: Matrix to store. Default is empty matrix with single NA value.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Calculates inverted matrix or resurns previously calculated (cached) value
## 
## Args:
##   x: Matrix object, created using method makeCacheMatrix().

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setSolve(s)
    s
}
