## These two functions should be used to decrease the cost of computation
## of matrices inversion. 
## The main idea is to cache previously calculated matrix inversion.

## This function creates a matrix object with "methods" - 
## set and get sets and gets the matix
## setinverse and getinverse sets and gets the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x

    setinverse <- function(solve) m <<- solve
    getinverse <- function() m

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function returns the inversion of matrix x (a matrix created with
## makeCacheMatrix function).
## If the result was previously calculated, 
## then cached value should be returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
