## These functions will store the potentially expensive 
## result of the solve() calculation on a given matrix.


## Creates a list object containing methods for getting 
## and setting the matrix and the matrix inverse

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


## Solves for the inverse matrix on a 'CacheMatrix' if
## the inverse is not cached.  Otherwise, returns cached
## inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    data = x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
