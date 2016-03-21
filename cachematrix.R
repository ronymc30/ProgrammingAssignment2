# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  #if inv is already computed, return value from cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #else, compute inverse & set inverse in cache
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)    
  ## Return a matrix that is the inverse of 'x'    
}

## Test run
a <- rbind(c(4,7),c(2,6))
b <- makeCacheMatrix(a)
b$get()
## First run 
cacheSolve(b)
# Second run
cacheSolve(b)

# Run results
# > ## Test run
#   > a <- rbind(c(4,7),c(2,6))
# > b <- makeCacheMatrix(a)
# > b$get()
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > ## First run 
#   > cacheSolve(b)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > # Second run
#   > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
