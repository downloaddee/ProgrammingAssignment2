## The two functions can be used for caching matrices with their
## inverses. makeCacheMatrix can be used to create such a cached
## matrix and cacheSolve can then be applied to that matrix
## to calculate the inverse and cache it or return it from the cache
## if already present.

## Creates a cache matrix with methods for getting and setting
## the original matrix and the inverse in the cache.
## set       sets the matrix to a new one
## get       returns the underlying matrix
## setinv    sets the inverse of the matrix from the cache
## getinv    returns the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Calculates the inverse of a cacheMatrix created by makeCacheMatrix
## or returns the value from the cache, if present there

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
