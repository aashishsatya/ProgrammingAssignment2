## Put comments here that give an overall description of what your
## functions do

# Since computing inverse of matrices are computationally time-consuming,
# we try to optimise the process by simply caching the results.

# makeCacheMatrix makes a list which contains functions to set and get the
# values of the elements in matrix and the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # function that makes the list or data structure that enables caching
  
  # s is inverted matrix, null initially
  # changed after inverse is obtained
  s <- NULL
  
  set <- function(y) {
    
    # change value of matrix
    x <<- y
    # matrix changed, so inverse changed
    s <<- NULL
  }
  
  # simply return the data
  get <- function() x
  
  # assign the solved inverse value to variable s
  setInverse <- function(solve) s <<- solve
  
  # return the inverse
  getInverse <- function() s
  
  # return the special "vector"
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  # function that uses the data structure created above
  
  # check if inverse is already computed
  s <- x$getInverse()
  if (!is.null(s))
  {
    # inverse already computed
    # so return it
    return(s)
  }
  
  # inverse not computed, so compute it
  # get data first
  data <- x$get()  
  # find inverse of the data
  s <- solve(data)
  # cache this so that in future the inverse
  # may be simply returned
  x$setInverse(s)
  
  # return inverse
  s
}
