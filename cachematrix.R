## Coursera R Programming
## Assignment: Caching the Inverse of a Matrix

## Function Name - makeCacheMatrix
## Function Description - creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # set the value of the "matrix"
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # get the value of the "matrix"
    get <- function() x

    # set the value of the inverse of the "matrix"
    setInverse <- function(matrixInverse) inverse <<- matrixInverse

    # get the value of the inverse of the "matrix"
    getInverse <- function() inverse

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Function Name - cacheSolve
## Function Description - Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    
    # invoke getInverse
    inverse <- x$getInverse()

    # check if getInverse returned cached data
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # get the "matrix" whose inverse has to be computed
    mat <- x$get()
    
    # compute the inverse of the "matrix"
    inverse <- solve(mat, ...)

    # set the inverse of the "matrix"
    x$setInverse(inverse)
    inverse
}