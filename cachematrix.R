## This code implements a special object that stores an invertible matrix
## and caches its inverse.


## Function makeCacheMatrix is a container for a matrix (assumed to be
## invertible) that can cache the inverse of the matrix when it is computed.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve takes as input an object of the type makeCacheMatrix
## produces and returns the inverse of the input matrix. If the inverse was
## already stored in the input object then the stored value is returned,
## otherwise the inverse is computed and then stored and returned.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached inverse data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
