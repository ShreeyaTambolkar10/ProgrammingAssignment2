CacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  set_Inv <- function(inverse) j <<- inverse
  get_Inv <- function() j 
  list(set = set, get = get, 
  set_Inv = set_Inv, 
  get_Inv = get_Inv)
}

cache <- function(x, ...) {
  j <- x$get_Inv()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$set_Inv(j)
  j
}