## Two functions that allow you to compute the inverse of a matrix and 
## cache the inverse to be used later without extra computation

## makeCacheMatrix() is a matrix object. It stores the matrix and the cache of
## the inverse of that matrix once it has been calculated. It sets the matrix
## and the inverse matrix in the parent environment with <<-

makeCacheMatrix <- function(x = matrix()) {
    MInv <- NULL
    set <- function(y) {
        x <<- y
        MInv <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) MInv <<- Inv
    getInv <- function() MInv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve() checks if the inverse matrix has already been calculated and if
## so returns it from the cache. If it has not been calculated, the inverse
## matrix is calculated, stored in the cache in makeCacheMatrix() and returns
## the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    MInv <- x$getInv()
    if(!is.null(MInv)) {
        message("getting cached data")
        return(MInv)
    }
    data <- x$get()
    MInv <- solve(data)
    x$setInv(MInv)
    MInv
}
