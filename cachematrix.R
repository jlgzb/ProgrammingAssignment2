## These functions allow to create a matrix object, which can cache the Inverse
## of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- Null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(solved_val) inv <<- solved_val
    get_inv <- function() inv
    list(set = set,
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse from the cache already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
