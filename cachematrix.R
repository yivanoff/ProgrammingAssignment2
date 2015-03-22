# Creates the special matrix which actually is list of functions
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# Calculates or gets from cache inversion matrix to x which should be a makeCacheMatrix class
cacheSolve <- function(x, ...) {
    
    # Check x parameter for having "getsolve()" method
    lapply (names (makeCacheMatrix()),
        function (i) if (! i %in% names (x)) 
            stop (paste ("Wrong argument in cacheSolve() function. It must have ", i, "() method", ""))
    )
    
    # Check if x has cached inversion matrix
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # If x doesn't have cached value then calculate it and save
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
