## This function will cache and inverse a given matrix

## The makeCacheMatrix function sets and gets the values of the given matrix  
## and will cache the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The cacheSolve function will check if the inverse matrix has been calculated 
## already and give the cached data. If not, it will calculate the inverse of 
## the given matrix.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
        
}
