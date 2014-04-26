## Set of functions for a class of matrix that can cache its inverse.
##
## Caching the inverse can be beneficial in situations when the inverse of the
## matrix needs to be referenced frequently, the computational burden for
## calculating the inverse is large, and there is sufficient memory to store
## the inverse.

## makeCacheMatrix returns a list that behaves as a CacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    
    # Return a list containing all functions.
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## cacheSolve returns the inverse of a CacheMatrix. The solve() function is
## used only the first time that cacheSolve is called on a particular
## CacheMatrix. After that, the cached value of the inverse is used.

cacheSolve <- function(x, ...) {
    # Check to see if the inverse has already been calculated
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Since no inverse is chached, calculate the inverse with solve()
    data <- x$get()
    inv <- solve(data, ...)
    # Cache the result and return
    x$set_inverse(inv)
    inv
}
