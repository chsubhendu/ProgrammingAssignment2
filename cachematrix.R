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
  invert_matrix <- NULL
  set <- function(y) {
    x <<- y
    invert_matrix <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) invert_matrix <<- solve
  getinvert <- function() invert_matrix
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## The following function computes inverse of special matrix
## created with the above function. 
## The cacheSolve function first checks if the inversion matrix available
## in the cache. If so it skips the computation and returns the inversion matrix
## directly from cache. Otherwise it computes and stores in cache for the first run

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert_matrix <- x$getinvert()
  if(!is.null(invert_matrix)) {
    message("getting cached data for matrix inversion")
    return(invert_matrix)
  }
  data <- x$get()
  invert_matrix <- solve(data, ...)
  x$setinvert(invert_matrix)
  invert_matrix
}

