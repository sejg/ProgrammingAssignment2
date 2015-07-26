## This pair of functions can be used to find the inverse of a matrix, 
## or alternatively to retrieve the cached inverse of a matrix

## makeCacheMatrix creates a list of functions to set the matrix, get the matrix, 
## set the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  ##set the value of the matrix
  set<-function(y) {
    x <<- y
    inv<<-NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of the inverse
  setinv <- function(inverse) inv <<-inverse
  ##get the value of the inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve should compute the inverse of the special matrix, 
## or retrieve the cached value for the inverse if it has already been calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinv(inv)
  inv
        
}
