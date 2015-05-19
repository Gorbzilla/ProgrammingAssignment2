## These two functions allow for the inverse of a matrix to be
## cached, so that it only has to be calculated once
## regardless of how many times it is required

## The function makeCacheMatrix creates 4 functions that can
## be used to set a vector, get a vector, set the mean of the
## vector, or get the mean of the vector

makeCacheMatrix <- function(x = matrix()) {
  # initiate inv to null
  inv <- NULL
  # Generate the set function - can be used to set a vector
  # to a variable that can then have the other functions
  # applied to it using $ notation
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Generate the get function which will return the vector
  # that has been 'set' using the above function, if any
  get <- function() {
    x
  }
  # Generate the set_inverse function to set the inverse of
  # a matrix
  set_inverse <- function(matrix_inverse) {
    inv <<- matrix_inverse
  }
  
  # Generate the get_inverse function to get the inverse of
  # a matrix
  get_inverse <- function() {
    inv
  }
  # The return value of the makeCacheMatrix function is a list
  # containing references to each function held within
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## The function cacheSolve reads in the cached inverse matrix
## If it null, then it calculates and stores it
## OTherwise it returns the cached version

cacheSolve <- function(x, ...) {
  ## attempts to write the cached inverse to matrix_inverse
  matrix_inverse <- x$get_inverse()
  # If matrix_inverse is not null, write a message,
  # return it and exit the function
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  # If matrix_inverse is null, calculate the inverse using
  # solve() and then set it to matrix_inverse
  data <- x$get()
  matrix_inverse <- solve(data)
  x$set_inverse(matrix_inverse)
  matrix_inverse
}

# Some test code for the above function. Try them in order one by one:
# mx1 <- matrix(c(6,3,2,3,4,4,5,6,5),3,3)
# mx2 <- makeCacheMatrix(mx1)
# mx2$get()
# cacheSolve(mx2)
# cacheSolve(mx2)
# mx2$get_inverse()

