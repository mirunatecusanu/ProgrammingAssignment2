## The following two functions cache the inverse of a matrix

## 'makeCacheMatrix' is a function that creates a special "matrix"
## object that can cache its inverse
## its methods are:
## 1. setMatrix: method to set the matrix 
## 2. getMatrix: method to get the matrix
## 3. setInverse: method to set the inverse of the matrix
## 4. getInverse: method to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  matrix(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## cacheSolve is a function that computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has been calculated (and the matrix hasn't changed)
## then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  matrixData <- x$getMatrix()
  ## compute the inverse of the matrix
  i <- solve(x, ...)
  x$setInverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
