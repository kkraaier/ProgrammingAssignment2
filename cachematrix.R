##makeCacheMatrix creates a list containing a function to
# 1. set value of matrix
# 2. get value of matrix
# 3. set value inverse matrix
# 4. get value inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) { 
    x <<- y
  inv <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get = get, setinverse = setinverse, getinverse=getinverse)
}



## CacheSolve retunrs the inverse matrix
# 1. Checks if inverse already computed
# A. if yes: get result from cache and skip calculation
# B. if no: compute, set in cach

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
      message("getting data from cache")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
