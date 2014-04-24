## The purpose of these two functions is to cache the inverse of matrix and
## retrieve the value of cache when needed.
     	
## The first function makeCacheMatrix creates a special "matrix", which is a
## list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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

## The following function calculates the inverse of the special "matrix"
## calculated with the above function.  However, it first checks to see if
## the inverse has already been calculated.  If so, it gets the inverse
## from the cache and skips the computation.  Otherwise, it calculates the
## inverse of the matrix and sets the value in the cache via the "setsolve"
## function.
	
cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     
     ## Return a matrix that is the inverse of 'x'
     if (!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}

