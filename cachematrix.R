#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly
#The following two functions cache the inverse of a matrix.

#The first function is called 'makeCacheMatrix': This function creates 
#a special "matrix" object that can cache its inverse.
# It creates a list containing a function to
#   1.set the value of the matrix
#   2.get the value of the matrix
#   3.set the value of inverse of the matrix
#   4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function returns the inverse of the matrix. However, it first
#checks to see if the inverse has already been calculated. 
#If so, it gets the inverse of the matrix from the cache and skips 
#the computation. Otherwise, it calculates the inverse of the matrix and sets
#the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
