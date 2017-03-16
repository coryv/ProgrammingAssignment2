## This is a function that will cache a non-singular matrix
## and then return the inverse of the matrix by running solve()
## on the original. Matrix must be square.

## makeCacheMatrix stores a matrix along with a set of functions that
## may be run on the matrix in conjunction with the cacheSolve 
## function.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL ##Initialize m
  set <- function(y){ ##function that will set the cached Matrix value
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve ##Set the inverse
  getinverse <- function () m ##Get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Running Solve on the cached matrix and passing the value back up
## to makeCacheMatrix to store in cache, but if the cache already
## contains a cached inverse matrix it will print the cached. 

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get() ##get the matrix from makeCacheMatrix
	m <- solve(data, ...) ##run solve() on the cached matrix
	x$setinverse(m) ##set the inverse from solve as the cached matrix
	m ##print the resulting inverse matrix that has been inverted
}
