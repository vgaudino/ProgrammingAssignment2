## Put comments here that give an overall description of what your
## functions do

##this function creates the vector that containg a cache of the matrix

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) z <<- inverse 
    getinv <- function() z
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##Now with this function I can calculte with Solve function the inverse of the data vector from makeCacheMatrix

cacheSolve <- function(x, ...) {
    z <- x$getinv()
    if(!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinv(z)
    ## Return a matrix that is the inverse of 'x'
    z
}

