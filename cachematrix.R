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


## Write a short comment describing this function

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


