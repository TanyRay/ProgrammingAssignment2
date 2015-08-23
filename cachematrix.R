## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y){
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverted <- function(inv) inverted <<- inv
    getinverted <- function() inverted
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverted <- x$getinverted()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setinverted(inverted)
    inverted
}
