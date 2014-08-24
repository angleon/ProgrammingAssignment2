## This assignment shows how to use a cache, in order to save calculation
## time

## Name: makeCacheMatrix
## This function :set the value of the matrix
##                get the value of the matrix
##                set the value of the matrix's inverse
##                get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  
        inverseMatrix <- NULL
        set <- function(y) {
          x <<- y
          inverseMatrix <<- NULL
        }
        
        get <- function() matrix
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Name: cacheSolve
## This function get the inverse of a matrix. It checks if the value was
## calculated before and if it so, retrieve the value from the cache, if not
## the calculation is performed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
          message("getting cached data")
          return (inverse)
        }
        
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
