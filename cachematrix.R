## Matrix inversion can be computationally intensive.  An approach to reduce
##  the time required is to cache any matrix inversions that were previously
##  calculated  <paraphrased from assignment from "r programming" - rdpeng - JHU>

# Example:
#  invertable:
#         [,1] [,2] [,3]
#   [1,]    1    2    3
#   [2,]    0    1    4
#   [3,]    5    6    0
# y <- makeCacheMatrix(invertable)
# > cacheSolve(y)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > cacheSolve(y)
# getting cached data
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

## makeCacheMatrix: This function creates a special "matrix" object
##  that can cache its inverse.
##  This function should be called first in order to create the "matrix"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix"
##  returned by makeCacheMatrix above. If the inverse has already been
##  calculated (and the matrix has not changed), then cacheSolve should
##  retrieve the inverse from the cache.
## This function is called second to return the cached or not cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
