## These functions initialize a list of operations which will be applied on an 
## input matrix then compute its inverse and cache it to avoid the repetitive
## action of computing the inverse

## Initialize the object matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

    matrixInv <- NULL
    set <- function(y) {
            x <<- y
            matrixInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrixInv <<- inverse
    getinverse <- function() matrixInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## Fetch the cached inverse matrix or computes it if it has not been computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInv <- x$getinverse()
        if(!is.null(matrixInv)) {
                message("getting cached data")
                return(matrixInv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}