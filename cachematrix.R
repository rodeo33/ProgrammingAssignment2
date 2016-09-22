## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix returns list that contains four functions:
##get(),set(),getInverse(),setInverse()
## and two objects x and m

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
    
  }
  
  get<-function() x
  
  setInverse<- function(solve) m<<-solve
  getInverse<- function() m
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## cacheSolve function takes input as object of type returned by
##makeCacheMatrix
##If value of inverse of a matrix is already cached, 
##then it returns value from cache
##else, it finds it's inverse and stores this value in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
  
}
