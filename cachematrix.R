# These functions cache the inverse of a square invertible matrix

# This function accepts a square invertible matrix and
# returns a list of functions that:
# 1) Sets it's value
# 2) Gets it's value
# 3) Sets the inverse value of the matrix
# 4) Gets the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function returns the inverse of a square invertible matrix.
# In the case where the inverse has already been calculated, 
# the function returns a cached value. It accepts a list created with 
# the "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}