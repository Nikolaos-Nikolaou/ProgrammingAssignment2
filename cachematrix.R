
## My code is an example based code with a few motifications. 

makeCacheMatrix <- function(x = matrix()) {
  
  t=NULL
  
  set <- function(y) {
    
    x <<- y
    t <<- NULL
    
  }
  
  get <- function() x
  
  setinversem <- function(inversem) t <<- inversem
  
  getinversem <- function() t
  
  list(set = set, get = get,
       setinversem = setinversem,
       getinversem = getinversem)
  
}

cacheSolve <- function(x, ...) {
  
  t <- x$getinversem()
  
  if(!is.null(t)) {
    
    message("getting cached data")
    return(t)
    
  }
  
  data <- x$get()
  t <- solve(data, ...)
  x$setinversem(t)
  t
  
}

## If you want to test my code, just run the following:
## mymatrix<-makeCacheMatrix(matrix(c(1,-1,0,1),2,2))
## solve(matrix(c(1,-1,0,1),2,2))
## cacheSolve(mymatrix)
## cacheSolve(mymatrix)