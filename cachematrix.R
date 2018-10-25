## The below functions make use of lexical scoping in R to help cache
## an otherwise computationally intensive operation: matrix inversion

## Create an environment for the matrix x for use in the global environment
## The functions are standard set+get operations in object-oriented
## programming; set() may be called to obviate repeated calls to
## makeCacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInv<-function(xInverse) m<<-xInverse
  getInv<-function() m
  return(list(set=set, get=get,
              setInv=setInv, getInv=getInv))
}

## Takes object created by makeCacheMatrix() as input, returns the inverse of
## the matrix "x" contained in cachedObj
cacheSolve <- function(cachedObj, ...) {
  ## Return a matrix that is the inverse of 'cachedObj'
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