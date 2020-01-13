## A pair of functions that cache inverse of matrix

## makeCacheMatrix creates special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get matrix
  get <- function() x
  
  #set inverse
  setInv <- function(Inv) 
    inv <<- Inv
  
  #retrieve inverse
  getInv <- function() inv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve computes inverse returned by makeCacheMatrix;
## if already calculated & matrix is unchanged, retrieves inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
