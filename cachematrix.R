## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Below are two functions that are used to create a special object 
## that stores a matrix and cache's the matrix inverse after it has been computed once.
## Subsequent calls to inverse the matrix (using cacheSolve()) will return the cached inverse insted
## of repeating the clculation

## The 'makeCacheMatrix' function takes a numeric matrix (assumed to be invertable)
## and creates a special object, which adds to the matrix a list of functions to
## 1.  set the value of the object
## 2.  get the value of the object
## 3.  set the value of the matrix inverse 
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}



## This function computes the inverse of the special
## "matrix object" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            s <- x$getsolve()
            if(!is.null(s)) {
                  message("getting cached data")
                  return(s)
            }
            data <- x$get()
            s <- solve(data, ...)
            x$setsolve(s)
            s
 
}
