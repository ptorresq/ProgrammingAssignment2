## This function has been created in order to create a matrix
## that can cache its inverse

## First you have to create a function calling a matrix 'x' 
## Then It will set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## You will need to create a matrix assigning to value to 'x'
## Then apply the function makeCacheMatrix(x)

## Once We have the first function, the cacheSolve funcition is applied
## This will calculate the inverse of the matrix created above.
## It will check if the inverse of the matrix was already calculated and
# it will print the result. This will also show a message 'Getting Cached Data'
## while the function is processing the answer.
## It is assumed that the matrix can be inversed

cacheSolve<- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting Cached Data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinv(inverse)
  inverse
}

## Running the cache solve for the first time

cacheSolve(inverse)

## Running cache solve for the second time

cacheSolve(inverse)

## It will show the 'Geting Cached Data' message
## And it will print the result
