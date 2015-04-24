## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  {
  inv_x = NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get = function() x 
  setinverse = function(inverse) inv_x <<-inverse
  getinverse = function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv_x = x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } 
    mat.data = x$get()
    inv_x <- solve(mat.data, ...)
    x$setinverse(inv_x)
    return(inv_x)
  



test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
