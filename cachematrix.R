#############
### usage ###
#############

## pass in a new matrix m1
# matrixObj <- makeCacheMatrix(m1)

## get the inverse
# inv1 <- cacheSolve(matrixObj)

## when cacheSolve(matrixObj) is called again, the cached inv1 is returned

## when a new m2's inverse needs to be computed, set the new matrix and clear the cache
# matrixObj$set(m2)
# inv2 <- cacheSolve(matrixObj)



# clear global env
rm(list=ls())

# this function calculates the inverse of an invertible matrix
makeCacheMatrix <- function(x = diag(2)) {
  invX <- NULL
  # this enclosed function caches the input matrix for comparison later
  set <- function(y){
    x <<- y
    invX <<- NULL
  }
  
  # get the input matrix
  get <- function() x
  # cache the inverted matrix
  setInvX <- function(inverseX) invX <<- inverseX
  # get the inverted matrix
  getInvX <- function() invX
  list(set = set, get = get,
       setInvX = setInvX,
       getInvX = getInvX)
}

# this function gets the inverted matrix
cacheSolve <- function(x) {
  invX <- x$getInvX()
  # if invX is null, set the cached matrix equal to the input matrix and calculate invX
  if(is.null(invX)){
    # get the data
    data <- x$get()
    # solve for the inverse
    inverseX <- solve(x$get())
    # cache the inverse
    x$setInv(inverseX)
    # return the inverse
    invX <- inverseX}
  
  invX
}
