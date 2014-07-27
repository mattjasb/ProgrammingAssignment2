## Title:   Programming Assignment 2: Lexical Scoping
## Purpose: Two functions for creating matrices objects that
##          can cache their inverse
## Credits: User mattjasb, Coursera, R Programming 2014-07-27

## A makeCacheMatrix object, a matrix that can store its own inverse.
## Nested functions control value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## when initializing a new object, set cache of solved matrix to NULL
  solution <- NULL 
  
  ## nested function - use lexical scoping to change global variable linked to x,
  ## and reset cache to NULL because value of x has changed
  set <- function(y){ 
    x <<- y
    solution <<- NULL
    
  }

  ## nested function - return value of x
  get <- function() x
  
  ## nested function - INTERNAL USE ONLY - set value of solved matrix
  setsolution <- function(solve) solution <<- solve
  
  ## nested function - return solved matrix
  getsolution <- function() solution
  
  ##
  list(set = set, get = get, setsolution = setsolution, getsolution = getsolution)
  
}


## Return a matrix that is the inverse of 'x' and set value of "solution"
## in instance x of object makeCacheMatrix 

cacheSolve <- function(x, ...) {
  
  solution <- x$getsolution()
  
  ## test if the solution is already cached
  if(!is.null(solution)){
    message("getting cached data")
    return (solution)
  }
  
  ## compute solution
  data <- x$get()
  solution <- solve(data, ...)
  x$setsolution(solution)
  
  solution
  
}

