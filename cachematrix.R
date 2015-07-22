## The function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse, which is really a list containing functions to:
##    1. set the values of the matrix
##    2. get the values of the matrix
##    3. set the values of the inverse of the matrix
##    4. get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
