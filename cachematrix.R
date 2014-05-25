## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Cached inverse of matrix
    s <- NULL    
    
    # Sets new matrix
    set <- function(y) {
        x <<- y
        # invalidate the cached data on matrix changing
        s <<- NULL
    }
    
    # Returns matrix
    get <- function() x
    
    # Caches inversion of matrix
    setsolve <- function(value) s <<- value
    
    # Returns cached inverse of matrix
    getsolve <- function() s
    
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # get cached data
    s <- x$getsolve()
    # if cached data is not null - return it
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # calculate inverse of the matrix, cache the result and return it
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}


