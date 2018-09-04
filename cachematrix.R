## Creating an object to cache the inverse matrix, including the set/get 
## allowing us to set en get the inversed matrix.

## store the original matrix 'x' in the cache
## calcute its inverted matrix 'm' as well

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <-  function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list (set = set, 
              get = get,
              setsolve = setsolve,
              getsolve = getsolve)
        
}

cacheSolve <- function(x, ...) {
        ## extract the data from cache, and save it into a variable 'm'
        m <-  x$getsolve()
        
        ## if m contains values (ie inversed matrix): return 'm' 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## else: calculate m^-1, store it
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
