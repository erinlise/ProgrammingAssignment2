## These two functions allow you to cache a square matrix and its inverse into
## a list object, for easy retrieval later.

## makeCacheMatrix() creates a list object that includes a cached square matrix,
## and an empty cache for its inverse.
## The function cacheSolve() must be run in order to calculate and cache the
## inverse of the matrix into the list object.
## makeCacheMatrix() takes one argument, x, which is used as the data with which
## to construct a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        mdim<-sqrt(length(x))
        dim(x)<-c(mdim,mdim)
        inv <- NULL
        setmatrix <- function(y) {
                mdim<-sqrt(length(y))
                dim(y)<-c(mdim,mdim)
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() takes a matrix that has been cached using makeCacheMatrix(),
## and gives its inverse.
## The first time cacheSolve() is run on an object made with make CacheMatrix(),
## it will generate the inverse matrix, and will store it in the cache.
## On subsequent iterations, cacheSolve() will retrieve the inverse matrix
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
