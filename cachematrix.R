## Functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes inverse returned by makeCacheMatrix
## If inverse already calculated (and original matrix still unchanged), then cachesolve should retrieve inverse from cache (and thus save time)

cacheSolve <- function(x, ...) {
        ## Returns inverse matrix
  i <- x$getinverse()
  if(!is.null(i))  {
    message("getting cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
