## Put comments here that give an overall description of what your
## functions do

## this function returns a object through which we can access a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# this function receives as parameter an object (create with makeCacheMatrix)
# and returns its inverse if exists. Otherwise, this function calculates and
# returns the inverse of matrix, through solve command.   

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
