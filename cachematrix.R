## The functions below create a special matrix object and cache its inverse

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
        }
    setSolve <- function(solve) {
        inverse <<- solve
        }
    getSolve <- function(){
        inverse
        }
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function computes of the inverse of the special matrix above
## However, if inverse exists, the the function retrives the inverse
## from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getSolve()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setSolve(inverse)
    inverse
}
