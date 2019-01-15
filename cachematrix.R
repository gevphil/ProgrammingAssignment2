
## This function creates a cached matrix and provides the ability
## to set or get a matrix, previous matrix, or inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    matrix <- x
    matrixInverse <- NULL
    prevMat <- NULL
    set <- function(x) {
              matrix <<- x
    }
    get <- function() matrix
    setPrevMat <- function(z) {
              prevMat <<- z
    }
    getPrevMat <- function() prevMat
    setInverse <- function(y) {
              matrixInverse <<- y
    }
    getInverse <- function() matrixInverse
    
    list(set = set, get = get, setPrevMat = setPrevMat, getPrevMat = getPrevMat, setInverse = setInverse, getInverse = getInverse)
}


## Returns a matrix that is the inverse of x but checks to see if
## the inverse matrix is already cached. If the inverse matrix is not
## cached the inverse matrix is calculated then stored in the cache.

cacheSolve <- function(x, ...) {
    matrix <- x$get()
    matrixInverse <- x$getInverse()
    PrevMat <- x$getPrevMat()
    if(is.null(matrixInverse) == FALSE & all.equal(PrevMat,matrix) == TRUE) {
        message("getting cached inverse matrix")
        return(matrixInverse)
    }
    x$setPrevMat(matrix)
    matrixInverse <- solve(matrix)
    x$setInverse(matrixInverse)
    matrixInverse
}
