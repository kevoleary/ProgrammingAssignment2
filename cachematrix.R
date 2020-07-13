## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes in a matrix and creates a special matrix that can cache
# its inverse. It creates a list of functions that allow the caller to set
# or retrieve the matrix's values (using set() or get() respectively) as well as
# functions to set or retrieve the matrix's inverse (using setInv() or getInv()
# respectively). All of these values are saved to the cache and a list 
# containing these functions is returned.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(givenInv) inv <<- givenInv
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

# This code first checks to see if an inverse has already been saved to the
# cache and whether the matrix has changed. If the inverse is saved and the
# matrix is unchanged, it returns the cached inverse. Otherwise, it uses the 
# get() function from makeCacheMatrix() to retrieve the data, the solve()
# function to find the inverse, and the setInv() function from makeCacheMatrix()
# to cache this data. Finally, it returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv) & x == x$get()) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x)
    x$setInv(inv)
    inv
}
