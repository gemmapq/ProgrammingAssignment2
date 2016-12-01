## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse was cached and has not changed, then cachesolve returns that inverse. Otherwise, it computes it.

## makeCacheMatrix creates a set of functions (get(), set(), getInv(), setInv()) and returns them to the parent environment on a list.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(i) inv <<- i
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve checks if the inverse of the input is cached on the makeCacheMatrix's environment and it computes it otherwise.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
