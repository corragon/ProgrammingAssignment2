## These functions will store the potentially expensive 
## result of the solve() calculation on a given matrix.


## Creates a list object containing methods for getting 
## and setting the matrix and the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Setter/Getter for matrix
    set <- function(y) {
        ## Set inverse to NULL any time the matrix is set
        x <<- y
        i <<- NULL
    }
    get <- function() x
    ## Setter/Getter for inverse
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    ## Return the new "CacheMatrix" object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves for the inverse matrix on a 'CacheMatrix' if
## the inverse is not cached.  Otherwise, returns cached
## inverse matrix

cacheSolve <- function(x, ...) {
    ## Check if the inverse of x is cached
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    ## Calculate and set the matrix inverse
    data = x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ## Return inverse
    i
}
