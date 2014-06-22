## The two functions work in combination to calculate the inverse of the matrix and cache its value.
## In case the inverse is already calculated it does not perform the calculation again, but retrieves its value.

## This function creates a list of functions for setting and getting the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function calculates the inverse of the matrix, unless the inverse is already cached, in the latter case it directly returns the inverse

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m       ## Return a matrix that is the inverse of 'x'
}
