## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInv<-function(xInverse) m<<-xInverse
  getInv<-function() m
  # res<-list(set=set, get=get,
  #           setInv=setInv, getInv=getInv)
  list(set=set, get=get,
       setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(cachedObj, ...) {
  ## Return a matrix that is the inverse of 'cachedObj'
  # cachedObj<-makeCacheMatrix(x)
  m<-cachedObj$getInv()
  if(!is.null(m)) {
    message("Retrieving cached matrix")
    return(m)
  }
  data<-cachedObj$get()
  m<-solve(data, ...)
  cachedObj$setInv(m)
  return(m)
}