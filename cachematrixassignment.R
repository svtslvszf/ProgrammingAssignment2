

## makecachematrix makes a cached matrix 
# the cachesolve functions calculates the inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    y<<-x
    s<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) s<<-solve
  getsolve<-function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

cachesolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


cachemean(makeVector(x=1:20))

m<-matrix(c(1,2,3,4),nrow=2,ncol = 2)
solve(m)
