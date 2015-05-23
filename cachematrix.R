## The following two functions create an object to store an invertible matrix
## and cache the inverse of the matrix.

## This function creates the object to store the matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(xinverse) xinv <<- xinverse
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function calculates the inverse of the matrix, but first checks to see
## if the inverse has already been cached; if it has, then this function
## retrieves it from the cache, instead of recalcuating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}