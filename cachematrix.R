## This function takes a squared matrix as an input, then returns and cache the inverse of that matrix

## makeCacheMatrix has to be run before cacheSolve
## This functions creates the variables and functions used in cacheSolve
## 

makeCacheMatrix <- function(x = matrix()) {
  
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) inversem <<- inversematrix
  getinversematrix <- function() inversem
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
  
}

## This function return and cache a matrix that is the inverse of 'x'
## It first checks if the inverse matrix has already been cached for the dataset
## If yes, it will return the cached data, otherwise it will compute the inverse of the matrix and cache it

cacheSolve <- function(x, ...) {
  
  inversem <- x$getinversematrix()
  if(!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
    
  }
  
  data <- x$get()
  inversem <- solve(data, ...)
  x$setinversematrix(inversem)
  return(inversem)

}




