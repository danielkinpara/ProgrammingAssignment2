## These are two functions to cache and calculate the inverse of a 
## given matrix.
## The passed argument must be an square and invertible matrix.


## This first function implements a list of caching procedures:
## 1. To set the matrix;
## 2. To get the matrix;
## 3. To set the inverse matrix;
## 4. To get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## This second function checks if the solved matrix is cached.
## If yes, it returns the cached result.
## If not, it solves the matrix and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}