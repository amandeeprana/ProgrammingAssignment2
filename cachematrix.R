## Defining a function makeCacheMatrix on an input matrix that will save that matrix as a special object
## This will be understood by  second function (cacheSolve) can understand
## Also will be Attaching --> 'set', 'get', 'setinverse', and 'getinverse' functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## Reset the value of i to NULL.. Clean sheet :)
  set <- function(y) {
    x <<- y
    i <<- NULL ## i becomes NULL on run of set() even if it is 
  }                   ## defined elsewhere (i.e. in another matrix)
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## If inverse of matrix x is calculated then is is returned from cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("showing cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## TESTING Code
## my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
## my_matrix$get()
## cacheSolve(my_matrix)

#> my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
#> my_matrix$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(my_matrix)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

