##Please find below the functions makeCacheMatrix and cacheSolve.
##Detailed comments on each function are made right below them.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set= set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Comments:
makeCacheMatrix <- function(x = matrix()) {
  ##creates the function containing the matrix named "x"
  i <- NULL
  ##"i" is first given a null value (i.e. "i" is empty)
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## creates the method "set" as a function of "y" 
  ## where "y" contains the matrix "x", and "i" is empty (null)
  get <- function() x
  ## creates the method "get" which returns the matrix "x"
  setinverse <- function(inverse) i <<- inverse
  ## calculates the inverse of the matrix and assigns the result value to "i"
  getinverse <- function() i
  ## returns the value of the inverted matrix
  list(set= set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## this is the true return of the function "makeCacheMatrix". 
  ## everytime a new matrix is introduced, "makeCacheMatrix" returns the four methods: "set, "get", "setinverse", "getinverse"
}
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ## calls the method "getinverse" contained in the function of the matrix "x"
  ## calculates and returns the value of the inverse of the matrix "x"
  ## assigns the value to "i"
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## if "i" IS NOT EMPTY (i.e. the inverse of the matrix has been already calculated and stored in "i")
  ## then the value of "i" is directly returned without any new calculation
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## if "i" IS EMPTY (i.e. the inverse of the matrix has not been calculated)
  ## then the matrix "x" is called and its inverse is calculated
  ## the value of the inverted matrix is assigned to "i"
}
