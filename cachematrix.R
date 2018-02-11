## Coursera - R programming - week 3 - programming assignment.
## Author: Xavier.
## Date: 11/02/18

## makeCacheMatrix function.
## creates and returns a list of functions: get, set, getInverse, SetInverse.
## The purpose is to inverse a matrix in the cache memory.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function.
## If the inverted matrix does not exist, the procedure invert x and sent it back.
## Otherwise the stored inverted matrix is sent back.
## The stored inverted matrix is stored in cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
    }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

##exemple of the execution of the code
# x = matrix(c(2,3,3,2),2,2)
#  m = makeCacheMatrix(x)
#  m$get()
#      [,1] [,2]
# [1,]    2    3
# [2,]    3    2
#
# cacheSolve(m)
#     [,1] [,2]
# [1,] -0.4  0.6
# [2,]  0.6 -0.4


