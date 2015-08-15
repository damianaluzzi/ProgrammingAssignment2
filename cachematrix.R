## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix() creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## x: invertible matrix
  ## return: a list containing functions to
    ## 1. set the matrix
    ## 2. get the matrix
    ## 3. set the inverse
    ## 4. get the inverse
  ##this list is used as the input to cacheResolve()
  
  inv <- NULL
  set = function(y) {
  ## <<-  assign a value to an object in an environment 
  ## Different from the current environment.
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setInv = function(inverse) inv <<- inverse
  getInv = function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}
## Inverse of the matrix created by the function makeCacheMatrix. 
## When the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## Return: inverse of the original matrix input to makeCacheMatrix()


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'# return a matrix inverse of x; 
  ## Inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("get cache data")
    return(inv)
  }
## Otherwise
  mat <- x$get()
  inv <- solve(mat, ...)
## Sets value	
  x$setInv(inv)
  return(inv)
}


## testing the function makeChacheMatrix
testing = function(mat){
  ## mat is invertible matrix
  dataTemp = makeCacheMatrix(mat)
  start.time = Sys.time()
  cacheSolve(dataTemp)
  duration = Sys.time() - start.time
  print(duration)
  
  start.time = Sys.time()
  cacheSolve(dataTemp)
  duration = Sys.time() - start.time
  print(duration)
}

# Can you try the testing on a matrix by 1500 rows and 1500 columns with random numbers:

set.seed(1110201) 
r = rnorm(2250000) 
mat1 = matrix(r, nrow=1500, ncol=1500) 
testing(mat1)
