## The goal of these functions is to be able to quickly calculate
## the inverse of a matrix (a computationally expensive process).
## This will be done by storing a special matrix in Cache and 
## anytime the inverse is calculated it is either retrieved 
## from the cache (if unchanged) or recalculated and stored.

## Make a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<-inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the special matrix. If it has been 
## previously calculated it is retrieved from the cache. If not
## previously calculated, it is now calculated.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
        ## matrix returned that is the inverse of 'x'
}
