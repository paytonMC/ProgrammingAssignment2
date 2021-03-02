## These two functions create a special matrix which stores it's own inverse.
## These functions are intended to be used with large matrices, where the 
## inverse operation is very costly.

## This function creates a special "matrix" that can cache its inverse.  The 
## special matrix is really a list of functions allowing the user to get the
## matrix, set the matrix, get the inverse, and set the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversevalue) matrix_inverse <- inversevalue
  getinverse <- function() matrix_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of a matrix and adds it to the
## cache matrix list create by the function above.  If the inverse has already
## been calculated, then the function uses the cached value instead of
## performing the inverse operation.

cacheSolve <- function(x, ...) {
  
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  matrix <- x$get()
  matrix_inverse <- solve(matrix)
  x$setinverse(matrix_inverse)
  matrix_inverse

}
