## The Assignment to make Cache matrix and check
## it for cache

## Make a Cache matrix

makeCacheMatrix <- function(m = matrix()) {
    mi = NULL
    set = function(n){
        m <<- n
        mi <<- NULL
    }
    get <- function() m
    setinv <- function(mean) mi <<- solve(m)
    getinv <- function() mi
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## To check for the Cache and populate it

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- m$getinv()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- m$get()
    mi <- solve(data, ...)
    m$setinv(mi)
    mi
}
