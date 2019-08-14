rm(list=ls())

makeCacheMatrix <- function(x = diag(2)) {
  invX <- NULL
  set <- function(y){
    cachedX <<- y
    invX <<- NULL
  }
  
  get <- function() x
  getCachedX <- function() cachedX
  setInvX <- function(inverseX) invX <<- inverseX
  getInvX <- function() invX
  list(set = set, get = get,
       setInvX = setInvX,
       getInvX = getInvX)
}


cacheSolve <- function(x) {
  invX <- x$getInvX()
  # if invX is null, set the cached matrix equal to the input matrix and calculate invX
  if(is.null(invX)){
    # set first cached matrix
    x$set(x$get())
    # solve for the first inverse
    inverseX=solve(x$get())
    # cache the first inverse
    x$setInv(inverseX)
    # return the first inverse
    invX=x$getInvX()}
    
  # if matrix does not change and inv is not null
  else {
    if((x$getCachedX()==x$get())) {
    message("getting cached data")
    return(invX)
    }
    # if input matrix has changed
    else{
        # cache the new matrix for subsequent comparison 
        x$set(x$get())
        # solve for the new inverse
        inverseX=solve(x$get())
        # cache the new inverse
        x$setInv(inverseX)
        # return the new inverse
        invX=x$getInvX()
    }}
  invX
}
