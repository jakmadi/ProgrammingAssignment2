## My first function and my second function work in unison to 1) cache the inverse of a "matrix" and 2) compute the inversion of the matrix. Has the matrix already been inverted, then cachesolve will retrieve the inverse from the cache 
## 'makeCacheMatrix' caches the inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 'cacheSolve' computes an inversion if the matrix wasen't aready cached inverted before by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
        ## Return a matrix that is the inverse of 'x'

