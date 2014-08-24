## This R class has two functions, one that gets a matrix and sets the inverse
## of that matrix and the other gives you the inverted matrix as output

## This function will create an inverse
## of the matrix x and cache it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This funtion will check if the inverse 
## of a matrix is already cached, if yes
## then it will not recalculate it else
## it will calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(! is.null(m)) {
    message("getting cached inverse matrix")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

