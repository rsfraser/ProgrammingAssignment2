
## makeCacheMatrix and cacheSolve are two functions for caching the
## inverse of a matrix to reduce processing time by not requiring
## it be re-calculated where it has been previously solved

## makeCacheMatrix function stores a list of functions for
## for creating / manipulating a matrix and caching inverse once 
## solved by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {  ## used to set a new and clear cache
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix. If inverse of matrix 
## already exists (m not null) then it returns the cached inverse, 
## else solves matrix and caches via makeCacheMatrix setinverse()

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
      if(!is.null(m)) {
         message("getting cached data")
         return(m)
      }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}
