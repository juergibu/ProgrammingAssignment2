## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## setup a matrix caching its inverse
        ## inverse apriori unknown
        inverse.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse.matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse.matrix <<- i
        getinverse <- function() inverse.matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## is there a cached inverse?
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        ## cache inverse
        x$setinverse(i)
        i
}
