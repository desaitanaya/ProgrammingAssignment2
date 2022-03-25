## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS) #is used to calculate inverse for non squared as well as sqaure matices
makeCacheMatrix <- function(x = matrix()) {
  #holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  inv <- NULL     #initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x     #function to get matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x     #function to obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the cached value
  inv <- x$getinv()
  # if a cached value exists return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$get()
  inv <- solve(data)   #calculates inverse value
  x$setinv(inv)
  
  # return the inverse
  inv
}
