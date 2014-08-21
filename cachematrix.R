#!/usr/bin/Rscript
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  This function creates an matrix object with capability
#  to cache the inverse of the matrix.
#  Prerequisite is that the matrix is invertible. This is not checked.
#  Instance methods are:
#   set: assign a matrix value to the object
#   get: get the value (as matrix)
#   setinv: set the inverse matrix
#   getinv: get the inverse matrix
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#  This function updates the cache is it is outdated
#  and returns the inverse matrix.
#  Prereqisite is that an invertible matrix is assigned the the parameter
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    message("calculating inverse")
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}

# try it
# check the messages "calculating inverse" and "getting cached data"
m <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(m)
cacheSolve(m)

m$set(matrix(6:9,2,2))
cacheSolve(m)
cacheSolve(m)

