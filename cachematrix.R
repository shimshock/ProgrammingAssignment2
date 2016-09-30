## This function will cache the inverse of a matrix
## When trying to invert a matrix it will first check to see if thee is a cached verison
## if there is it will use it

## Creates the matrix in cache

makeCacheMatrix <- function(x = matrix()) {##defines the function
    inv<- NULL
    set<- function(y){
        x<<-y
        inv<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) inv<<-inverse
    getinverse<-function() inv
    list(set=set,get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}


## This provides the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
