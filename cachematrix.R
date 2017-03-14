## A pair of functions that cache the inverse of a matrix - makeCacheMatrix and cacheSolve

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
}
        get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                        getinverse <- function() inv
                                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                 
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
}
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
                        
## Sample Run :
## > x = rbind(c(1,-2),c(-2,1))
## > x
##     [,1] [,2]
##[1,]    1   -2
##[2,]   -2    1
##> m$get()
##     [,1] [,2] [,3] [,4]
##[1,]    1    5    9   13
##[2,]    2    6   10   14
##[3,]    3    7   11   15
##[4,]    4    8   12   16
##> m <- makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1   -2
##[2,]   -2    1
##> cacheSolve(m)
##           [,1]       [,2]
##[1,] -0.3333333 -0.6666667
##[2,] -0.6666667 -0.3333333
##> cacheSolve(m)
##getting cached data
##           [,1]       [,2]
##[1,] -0.3333333 -0.6666667
##[2,] -0.6666667 -0.3333333
