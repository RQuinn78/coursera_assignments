## This script contains 2 functions the first creates a matrix that can cache its inverse
## and the second retrieves the cached inverse matrix

## This function creates a list of functions which set the value of the matrix, get the
## value of the matrix, set the inverse value of the matrix and get the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x 
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list (set=set, get=get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function checks whether the inverse of the matrix exists as a cached 
## value. If so it looks up the cached matrix and returns it, if not it computes and
## returns the inverse matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
        message ("Getting Cached Data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse (inv)
    inv
}
