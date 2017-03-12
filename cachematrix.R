##
## The functions in this script implement a new matrix-like object that is able to
##  cache the results of a matrix inversion operation.
## 

##
## The 'makeCacheMatrix' function creates a list of functions that are bound to
##  lexically scoped storage for the matrix and the matrix inverse.  This list may be
##  used to allow caching of inverse calculation results.
##
## Example:
##  m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))    -- Create a cache matrix object
##  m$set(matrix(rnorm(9), 3, 3))                   -- Set a new matrix
##  m$get()                                         -- Get the current matrix
##
makeCacheMatrix <- function(x = matrix()) {
    # Create lexically scoped storage for the matrix inverse
    inv <- NULL
  
    # Create and return a list which contains the access closures 
    list(
        set = function(y) { x <<- y; inv <<- NULL },
        get = function() x, 
        setinv = function(y) {inv<<-y}, 
        getinv = function() inv
    )
}

##
## The 'cacheSolve' function uses lists such as those returned from the 
##  'makeCacheMatrix' function to cache calculations of the matrix inverse so long
##  as the matrix is not reset.
##
## Use (with 'm' created by 'makeCacheMatrix()'):
##  inv <- cacheSolve(m)                            -- Calculate a matrix inverse
##  m$set(z)                                        -- Set a new matrix, z
##  system.time(inv <-cacheSolve(m))                -- Time the first inversion
##  system.time(inv <-cacheSolve(m))                -- Time the second inversion
##  inv %*% m$get()                                 -- Check the inverse is valid
##
cacheSolve <- function(x, ...) {
    # Check if the inverse is valid and simply return the cached value if it is.
    inv <- x$getinv()
    if(!is.null(inv)) return(inv)
    
    # If the cached value was not valid, a new inverse needs to be calculated.
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
