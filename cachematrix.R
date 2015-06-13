## The makeCacheMatrix and cacheSolve functions can be used to calculate the 
## inverse of a matrix. Unlike the solve function cacheSolve caches previous
## results to speed up the inversion process.

## This function wraps a matrix inside a list. The list returned 
## provides functions to store and retrieve the wrapped matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of a matrix. This function only
## works with matrices created by the makeCacheMatrix function. If the matrix
## passed as paramter contains a cached inverse matrix . The cached matrix is 
## returned otherwise the inverse matrix is caluclated, cached for future calls
## and returned.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
