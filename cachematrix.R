## Assignment: caching the inverse of a matrix
## This function will create a cache of a supplied matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(inv) im <<- inv
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache, and display a message 'getting cached data'.

cacheSolve <- function(x, ...) {
        ## Solving m (the matrix cached in function above), provides the inverse of the matrix
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
}
