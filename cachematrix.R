## Assignement 2 - rprog-005
## Two functions, calculating the inverse of a matrix in two cases: 
## 1 - initial matrix has not been changed
## 2 - initial matrix has been changed 

makeCacheMatrix <- function(x = matrix(), ...){
  ## create a special "matrix" object that can cache its inverse
  
  ## allocate matrix
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## set get
  get <- function() x
  
  ## set matrices
  setmatrix <- function(solve) m <<- solve
  
  getmatrix <-function() m
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...){
  ## compute the inverse of the special "matrix" returned by makeCacheMatrix
  ## If the inverse has already been calculated (and the matrix has not changed)
  ## then the cachesolve retrieves the inverse from the cache.
  
  ## check if inverse is empty
  m <- x$getmatrix()
  if(!is.null(m)){
    ## is the matrix the same?  
    im <- diag(nrow(m))
    calcIm <- m %*% x$get()           
    if(isTRUE(all.equal(calcIm,im))){
      message("Calculating from cache")
      return(m)
    }
    else{
      message("Calculating again as matrix change ")
      data <- x$get()
      m = solve(data, ...)
      x$setmatrix(m)
      return(m)
    }
  }
  
  ## get matrix
  data <- x$get()
  ## calculate inverse
  m = solve(data, ...)
  ## set inverse in object x
  x$setmatrix(m)
  ## print inverse
  m
  #}
  
  
}
