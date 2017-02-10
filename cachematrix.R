## Put comments here that give an overall description of what your
## functions do
## This program returns the inverse of the provided matrix from the cache if it was previously calculated. Otherwise, it calculates the inverse and returns the result

## Write a short comment describing this function
## This function returns a list consisting of 4 functions:
## to set the data
## to get the data
## to set the inverse
## to get the inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function checks whether the inverse of the given matrix is available in cache
## If available, it returns value from cache with message
## else it calculates the inverse and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

