## The function creates a special "matrix" object that can cache its inverse
## The cached object can be accessed faster than recomputing the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function computes efficiently the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## x is a list as returned by makeCachaMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  message("computing inverse")
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  
}
