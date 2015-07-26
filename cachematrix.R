## Matrix inversion is a costly operation. Purpose of these package is to
## cache the inverion matrix after the first call and utilize the cache for
## all subsequent inversion calls

## The first function, `makeCacheMatrix` creates a special "vector" 
## that is which is a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inversion (solve)
## 4.  get the value of the matrix inversion (solve)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function computes inverse of special matrix
## created with the above function. 
## The cacheSolve function first checks if the inversion matrix available
## in the cache. If so it skips the computation and returns the inversion matrix
## directly from cache. Otherwise it computes and stores in cache for the first run

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data for matrix inversion")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

