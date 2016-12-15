## These two functions serve to calculate the inverse of a matrix
## and cache them to the memory. This is used to avoid running the
## same time-consuming calculation multiple times

## makeCacheMatrix creates a matrix object that contains four functions:
##
## set: stores the original matrix
## get: recovers the original matrix
## setInverse: stores the inverted matrix
## getInverse: recovers the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve first tries to recover the inverted matrix from memory
## if it finds the inverse, then it returns it with a message saying
## it got it from memory.
## If the function is unable to find the stored inverse then it calculates
## the inverse of the matrix and stores it calling setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}