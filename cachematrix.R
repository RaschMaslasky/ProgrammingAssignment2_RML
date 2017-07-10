## In some cases matrix inversion is a slowly computation.
## There may be to caching the inverse of a matrix rather than compute it. 

## The following function create a list, which is containig 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #get the value
  get <- function() x
  #set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  #get the value of the inverse
  getinverse <- function() m
  #result list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  #checks if the inverse has already been computed
  if(!is.null(m)) { 
    message("getting cached data ...") 
    return(m)
  }
  #if not, to compute the inverse
  else {
    message("no cached data ...")
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
  }
}
