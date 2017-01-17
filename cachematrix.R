## A pair of functions that allows caching a matrix and its calculated inverse.
## If a matrix whose inverse has previously been calculated is to be inverted,
## rather than recalculating the inverse of the same matrix, the previously
## cached inverse matrix is pulled returned.

## makeCacheMatrix function sets and caches a matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function calculates the inverse of a matrix that has been set up
## by the makeCachedMatrix function.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    matrix <- x$get()
    
    inv <- solve(matrix, ...)
    
    x$setinverse(inv)
    
    inv
}
