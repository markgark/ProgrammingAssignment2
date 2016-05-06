# makeCacheMatrix is a function that returns a list of functions
# Functions included:
#
# setMatrix      
# getMatrix      
# setInverseM    
# getInverseM
#
#
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    getMatrix <- function() x
    setInverseM <- function(inv) i <<- inv
    getInverseM <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseM = setInverseM, getInverseM = getInverseM)
}

# Function to generate inverse of a "special" matrix with ...
#
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data...")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
