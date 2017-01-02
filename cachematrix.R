## Caching the Inverse of a Matrix - utilizing lexical scoping.
## Matrix inversion is usually a costly computation. To save time we are going
## to utilize already inverted matrix if available.

## This function creates a custom object with matrix and associated
## getter functions. Inverse of matrix is cached as well.
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invM <<- inverse
        getInverse <- function() invM
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function returns the inverse of matrix created by 
## makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, then then function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
