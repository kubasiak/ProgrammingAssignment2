makeCacheMatrix <-function(x=matrix()){
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
  im<-x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <-x$get()
  im<-solve(data)
  x$setinv(im)
}
