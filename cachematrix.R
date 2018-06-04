## This script contains two functions that i) create an empty matrix that can be stored in cache, 
## in this case the inverse of a matrix x, and ii) a function that checks if the inverse of a matrix
## x is stored in cache; if it is it retrieves the inverse. If it is not, it computes and returns the inverse.

## This function create an empty matrix that can be stored in cache, 
## in this case the inverse of a matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks if the inverse of a matrix
## x is stored in cache; if it is it retrieves the inverse. If it is not, it computes and returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
