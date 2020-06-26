## Put comments here that give an overall description of what your
## functions do

##Creates "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##Gets the inverse of the matrix from the cache if available, if not calculates it
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matr<-x$get()
  m<-solve(matr, ...)
  x$setinverse(m)
  m
}
