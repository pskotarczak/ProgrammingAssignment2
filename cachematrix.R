## Those two functions create a special object that stores a
## matrix and cache's its inverse.

## The function makeCacheMatrix creates a special "matrix" that
## is a list containing a function to set the value of the
## matrix, get the value of the matrix, set the value of the
## inverse and get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <- inverse
    getinv <- function i
    list(set = set, get = get,
         setinv = setinv, geinv = getinv)
}


## This function calculates the inverse of the special "matrix"
## created with the above function. It first checks to see, if
## the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of
## the inverse in the cache via the setinv function. 

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return i
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
