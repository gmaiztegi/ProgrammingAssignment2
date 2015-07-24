## Theese functions allow the programmer to calculate and cache
## the inverse of a square matrix. Computing the inverse is a costly
## operation and this allows to save some precious time

## makeCacheMatrix encapsulates a matrix and (eventually) its
## inverse. The matrix itself can be retrieved with get() and
## the inverse should be calculated using cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    # Inverse is initialized to null
    i <- NULL
    
    # Sets the matrix and resets the inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Gets the matrix
    get <- function() x
    
    # Sets the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    # Returns the inverse
    getinverse <- function() i
    
    # Finally, return a list with all the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix. It caches the result
## in its first run, so subsequent calls return inmediatelly.
cacheSolve <- function(x, ...) {
    # Get the currently stored value
    m <- x$getinverse()
    
    # Check if the inverse is already computed and, in that case,
    # return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Get the matrix, compute its inverse, and save it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    # Finally, return
    i
}
