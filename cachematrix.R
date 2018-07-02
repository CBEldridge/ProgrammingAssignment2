## The first two functions work together to (1: makeCacheMatrix) create a
# Matrix object that can store the inverse once it has been calculated one
## time, and (2: cacheSolve) calculate and store the inverse in the
## previously created objejct.

## This function creates a special "matrix" object that can cache its inverse
## by creating a list of functions to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set (cache) the inverse of the matrix
## 4) get the cached inverse of the matrix
makeCacheMatrix <- function(mtx = matrix()) {

    ## set the stored inverse to NULL when created
    inv <- NULL

    ## If the matrix is changed, set the inverse to NULL again
    set <- function(mty) {
        mtx <<- mty
        inv <<- NULL
    }

    ## Get the matrix representation
    get <- function() mtx

    ## used by cacheSolve to set the stored inverse
    setInverse <- function(solution) inv <<- solution

    ## used by cacheSolve to get the stored inverse
    getInverse <- function() inv

    ## declare the list of functions that makes up the matrix object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve retrieves
## the inverse from the cache rather than calculating it.
cacheSolve <- function(mtx, ...) {

    ## get the stored inverse
    inv <- mtx$getInverse()

    ## if the stored inverse is not null, return the inverse
    if(!is.null(inv)) {
        ## print a message to point out using cached data
        message("getting cached data")
        ## return the inverse
        return(inv)
    }

    ## otherwise, get the matrix
    mdata <- mtx$get()

    ## solve for the inverse
    inv <- solve(mdata, ...)

    ## cache the inverse solution
    mtx$setInverse(inv)

    ## return the inverse
    inv

}
