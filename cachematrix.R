
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly Below is a pair of functions thta cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## 1.set the value of the Matrix
## 2.get the value of the Matrix
## 3.set the value of the Inverse
## 4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
 

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ## Return a matrix that is the inverse of 'x' 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

  
 
