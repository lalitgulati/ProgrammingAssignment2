## Creates functions that helps save time, by pulling info from
## cache memory (if the info exists), otherwise it generates the info that is
## in cache, until next time. Goal is to have the inverse of matrix in cache

## Primarily there are two main functions, makeCacheMatrix and cacheSolve

## makeCacheMatrix after the initialization, sets up the details, both data/argument
## and the inverse.



makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
      }
    get <- function() x
       setinv <- function(inversx) xInv <<- inversx
       getinv <- function() xInv
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## cacheSolve when called provides the inverse matrix, from cache if it is there
## or solves and provides

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getinv()
        if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
        }
        data <- x$get()
        xInv <- solve(data, ...)
        x$setinv(xInv)
        xInv
        }
