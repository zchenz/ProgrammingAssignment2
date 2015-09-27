## This function caches the inverse of a matrix so that the inverse does not have to be
## calculated repeatedly if the matrix is doesn't change

## This function completes the following tasks:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function completes the following tasks:
## 1) check the inverse 'i' to see if it exists and is not NULL, if true, return(i)
## 2) if not, calculate the inverse of the maxtrix and cache it using x$setinverse(i) 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
