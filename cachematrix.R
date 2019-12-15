## This functions help calculate the inverse matrix of a given matrix

## This function creates a special list with 4 functions that relate to a given matrix

makeCacheMatrix <- function(c = matrix()){
  m <- NULL
  set <- function(y) {
    c <<- y
    m <<- NULL
  }
  get <- function() c
  setinv <- function(solve) m <<- solve # this calculates the inverse of the matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function takes the previously define list and uses it to generate the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

# Example to proof functionality
x<-matrix(3:6,2,2)
y <- makeCacheMatrix(x) # list of 4 functions
x # original matrix
cacheSolve(y) # inverse matrix
