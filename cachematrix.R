## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## Below you can find a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## Example: A = matrix(c(1,0,1,2,-1,3,1,4,2), nrow=3, ncol=3, byrow=TRUE)
## A
## [,1] [,2] [,3]
## [1,]    1    0    1
## [2,]    2   -1    3
## [3,]    1    4    2
## M <- makeCacheMatrix(A)

makeCacheMatrix <- function(x = matrix()) {
  ## Following the example explained in the assignment, this funcion consists of
  ## four encapsulated functions: set the matrix, get the matrix, set its inverse,
  ## get its inverse.
  inv <- NULL ## Initially inv is set to NULL
  ## 1st step: setting the matrix
  set <- function(y){
          x <<- y
          inv <<- NULL
  }
  ## 2nd step: getting the matrix
  get <- function()x
  ## 3rd step: setting the inverse
  setinverse <- function(inverse)
  inv <<- inverse
  ## 4th step: getting the inverse
  getinverse <- function() inv
  ## Encapsulating these functions into a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## After creating the matrix with makeCacheMatrix, we want a function cacheSolve
## that computes the inverse and cache the result. If we try to use cacheSolve again
## on the same matrix, then we obtain the pre-computer result. In this case we will 
## see an informative message in the command prompt.
## Example: A = matrix(c(1,0,1,2,-1,3,1,4,2), nrow=3, ncol=3, byrow=TRUE)
## A
## [,1] [,2] [,3]
## [1,]    1    0    1
## [2,]    2   -1    3
## [3,]    1    4    2
## M <- makeCacheMatrix(A)
## I <- cacheSolve(M)
## I should return 
## [,1] [,2] [,3]
## [1,]  2.8 -0.8 -0.2
## [2,]  0.2 -0.2  0.2
## [3,] -1.8  0.8  0.2
## I2 <- cacheSolve(M)
## This displays following message "Getting cached data"
## I2 should return 
## [,1] [,2] [,3]
## [1,]  2.8 -0.8 -0.2
## [2,]  0.2 -0.2  0.2
## [3,] -1.8  0.8  0.2

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Following the example explained in the assignment, firstly it gets
  ## the state of the inverse and sees if it has already been computed
  inv <- x$getinverse()
  ## If yes:
  if(!is.null(inv)){
    ## It returns the cmoputed inverse and displays a message
    message ("Getting cached data")
    return(inv)
    }
  ## If not, it gets the matrix itself
  data <- x$get()
  inv <- solve(data)
  ## It caches this result
  x$setinverse(inv)
  ## It returns this result
  inv
}
