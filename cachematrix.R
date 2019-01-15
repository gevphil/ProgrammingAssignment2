## Put comments here that give an overall description of what your
## functions do

## This function creates a cached matrix

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


## Returns a matrix that is the inverse of x

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
