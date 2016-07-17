## The code below creates a special "matrix" object and computes its inverse. when you call it next time, 
## if the input matrix is not changed and the inverse is already computed, then it retrieves the inverse from the cache
## 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  invmat<-x$get()
  inv<-solve(invmat,...)
  x$setinv(inv)
  return(inv)
}
