##
## The functions in this script implement a new matrix-like object that is able to
##  cache the results of a matrix inversion operation.
## 

##
## The 'makeCacheMatrix' function creates a list of functions that are bound to
##  lexically scoped storage for the matrix and matrix inverse.  This list may be
##  used to allow caching of inverse calculations.
##
## Example:
##  m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))    -- Create a cache matrix object
##  m$set(matrix(rnorm(9), 3, 3))                   -- Set a new matrix
##  m$get()                                         -- Get the current matrix
##
makeCacheMatrix <- function(x = matrix()) {
    # Define lexically scoped storage of the matrix and of the inverse
    mat <- x
    inv <- NULL
  
    # Implement operations on the scoped data
    set <- function(x) {
      mat <<- x
      inv <<- NULL # Always clear the inverse when the matrix is reset
    }
    get <- function() mat
    setinv <- function(x) inv <<- x
    getinv <- function() inv
  
    # Create and return a list which contains the access functions 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##
## The 'cacheSolve' function uses lists such as those returned from the 
##  'makeCacheMatrix' function to cache calculations of the matrix inverse so long
##  as the matrix is not reset.
##
## Use (with 'm' created by 'makeCacheMatrix()'):
##  inv <- cacheSolve(m)                            -- Calculate the inverse
##  inv %*% m$get()                                 -- Check the inverse is valid
##
cacheSolve <- function(x, ...) {
    # Check if the inverse has is valid and simply return the cached value if it is.
    inv <- x$getinv()
    if(!is.null(inv)) return(inv)
    
    # If the cached value was NULL, a new inverse needs to be calculated
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
