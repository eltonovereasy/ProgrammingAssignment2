#Establishes a cached matrix with inputs and outputs for both matrix parameters, as well as those for storing and acquiring the matrix's inversion.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) { m <<- solve }
		getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)			 
}

## Checks for a cached inversion of the given function, and returns it, otherwise calculating from the cached matrix.

cacheSolve <- function(x, ...) {
		##Check for previously determined inversion m, returning current value if available.
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##Otherwise, inversion is determined from the cached matrix x.
		data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
