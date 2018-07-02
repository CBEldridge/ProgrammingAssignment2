## The first two functions work together to (1: makeCacheMatrix) create a
# Matrix object that can store the inverse once it has been calculated one
## time, and (2: cacheSolve) calculate and store the inverse in the
## previously created objejct.

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inv <<- Inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- inverse(data, ...)
    x$setInverse(inv)
    m
}

## makeVector creates a special "vector", which is really a list containing
## a function to:
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the mean
## 4) get the value of the mean
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## The following function calculates the mean of the special "vector" created
## with the above function. However, it first checks to see if the mean has
## already been calculated. If so, it gets the mean from the cache and skips
## the computation. Otherwise, it calculates the mean of the data and sets the
## value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
