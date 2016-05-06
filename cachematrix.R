# makeCacheMatrix is a function that returns a list of functions
# Functions included:
#
# setMatrix      
# getMatrix      
# setInverse    
# getInverse
#
#
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# Function to generate inverse of a "special" matrix with ...
#
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    y <- x$getMatrix()
    inverse <- solve(y, ...)
    x$setInverse(inverse)
    inverse
}
