## Given an invertible matrix, makeCacheMatrix will create an
## object to be used by cacheSolve.  cacheSolve will use the 
## object to evaluate whether the inverse matrix already exists
## in cache, and will calculate it if not

## This function creates a list for the "matrix" that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(solve) mat_inv <<- solve
    get_inv <- function() mat_inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix, then retrieves
## the inverse from the cache if available.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv(m)
    m
}

