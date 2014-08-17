## Put comments here that give an overall description of what your
## functions do

## These functions implement caching of the inverse of a matrix.
## If the inverse cache does not exist, the functions compute and 
## return the inverse and cache it for future use. If the inverse 
## cache already exists, the functions simply return it.

## Write a short comment describing this function

## This function creates a matrix that cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
      	x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
      	setinverse = setinverse,
            getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of a matrix.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
		message("getting cached inverse data")
            return(inv)
      }
      data <- x$get()
	inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}

