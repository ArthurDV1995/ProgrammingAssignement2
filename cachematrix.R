## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  x_inver <- NULL
  set <- function(y){
    x <<- y
    x_inver <<- NULL
  }
  
  get <- function() x
  setInver <- function(inverse) x_inver <<- inverse
  getInver <- function() x_inver
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inver <- x$getInver()
  x_mat <- x$get()
  
  #checks that inverse exists and that matrix hasn't changed
  if(!is.null(x_inver) && x_mat == x) {
    message("getting cached inverse")
    return(x_inver)
  }
  #otherwise, finds inverse
  x_inv <- solve(x_mat, ...)
  x$setInver(x_inver)
  x_inver
  
}    
