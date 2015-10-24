## These two functions, "makeCacheMatrix" and "cacheSolve", have been written for the 
## Programming Assignment 2: Lexical Scoping of the R Programming course of the 
## Data Science Specialization, Coursera, October, 2015.

## The assignment aims for the understanding of the lexical scoping rule used by the 
## R language and the handling of potentially time-consuming computations via 
## retriving already calculated data from the cache.

## The provided examples, "makeVector" and "cachemean" functions, have been used as the 
## basis of the new code.

## makeCacheMatrix: This function creates a special matrix object that can cache its inverse.
## It contains the matrix x and its cahed inverse, inverseMatrix. 
## It returns a list of four functions:
## get(): returns the matrix x
## set(y): updates the matrix to a new value, y
## getInverseMatrix(): returns the cached inverseMatrix
## setInverseMatrix(solution): updates the cached inverseMatrix to a new value, solution

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL ## initiated the inverseMatrix to null as it has not been set yet
  
  set <- function(y) { ## updates the matrix and sets the inverseMatrix to null  
    x <<- y 
    inverseMatrix <<- NULL
  }
  
  get <- function() x ## simply returns matrix x
  
  setInverseMatrix <- function(solution) inverseMatrix <<- solution ## updates the cached inverseMatrix 
  
  getInverseMatrix <- function() inverseMatrix ## returns the cachced inverseMatrix
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix) ## output the four functions above in a list

}

## cacheSolve: This function computes the inverse of the special matrix returned by 
## "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix 
## has not changed), then the "cacheSolve" fetches the inverse from the cache.

cacheSolve <- function(x, ...) {

  inverseMatrix <- x$getInverseMatrix() ## fetch the cached InverseMatrix

  if(!is.null(inverseMatrix)) { ## inverseMatrix has been set or updated, return cached inverseMatrix and print the message "getting cached data"
    message("getting cached data")
    return(inverseMatrix) ## this exits the function
  }
 
  ## otherwise, need to calculate the inverseMatrix and store it.
  
  data <- x$get() ## fetch the matrix part of object x and store it in the variable data
  
  inverseMatrix <- solve(data, ...) ## calualte the inverse of the matrix and store it in variable inverseMatrix
  
  x$setInverseMatrix(inverseMatrix) ## store the calculated inverseMatrix in the cache
  
  return(inverseMatrix) ## return the inverse of the matrix and exit from here instead

}
