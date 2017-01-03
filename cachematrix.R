## first take in a matrix.  first time inverse is set, save it to cache
## if invers is called again, use saved invers from cache, else calculate.

## accepts matrix as input and gets or sets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}



## sets the invers if it hasn't been set already
## calls invers if was already set in previous step

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
