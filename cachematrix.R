## Below are two functions that are used to create a special
## object that stores an invertible square matrix and
## caches its inverse.


## This first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing functions to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat, ...)
    x$setInverse(m)
    m
}
