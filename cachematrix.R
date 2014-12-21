## These two functions allow you to cache a square matrix and its inverse into
## a list object, for easy retrieval later.

## makeCacheMatrix() caches a square matrix and creates an empty cache for 
## storing its inverse. These are stored in a list object.
## 
## makeCacheMatrix() takes as its argument the data to populate a square matrix,
## or, an existing square matrix object.
## 
## The function cacheSolve() must be run separately in order to calculate the
## inverse of the matrix, and store it in the cache.

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


## cacheSolve() gives the inverse of a matrix that has been cached using
## makeCacheMatrix().
## 
## cacheSolve() takes as its argument a list object containing a chached matrix,
## created by makeCacheMatrix().
## 
## The first time cacheSolve() is run it will generate the inverse of the 
## cached matrix, and will store it in the inverse matrix cache.
## 
## On subsequent iterations, cacheSolve() will retrieve the inverse matrix
## previously stored in the cache.

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
