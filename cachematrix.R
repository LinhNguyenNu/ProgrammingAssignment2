## Put comments here that give an overall description of what your
## functions do
# Functions to calculate inversed matrix and cache it and retrieve inversed matrix from cache (if calculated)

## function creating a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x # non-var function: get input matrix x
  setSolve <- function(solve) sol <<- solve
  getSolve <- function() sol
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  # output of makeCacheMatrix function: list of 4 functions: set get setSolve getSolve
}


## function computing the inverse of input matrix x returned by makeCacheMatrix above


cacheSolve <- function(x, ...) {## input of this function: output of makeCacheMatrix function
  sol <- x$getSolve()
  
  if(!is.null(sol)) {
    message("getting cached data")
    return(sol)
  }
  
  ## data: input matrix x in makeCacheMatrix function 
  ## retrieved by function "get" within makeCacheMatrix)
  data <- x$get()   
  
  ## Calculating inverse of matrix "data" 
  sol <- solve(data, ...) 

  ## Caching the inverse of matrix "data" calculated
  x$setSolve(sol)
  
  sol ## Return a matrix that is the inverse of 'x'
  
}
