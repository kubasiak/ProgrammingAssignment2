makeCacheMatrix <-function(x=matrix()){
#this function creates a generelized Vector which fourth value is getting inverse of matrix x
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinv <- function(invm) im <<- invm
  getinv <- function() im
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


cacheSolve<-function(x, ...){
  #This function first checks if value of inverse msatrix is calculated and stored and then either returns it or calculates it and returns it
  im<-x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <-x$get()
  im<-solve(data)
  x$setinv(im)
}
