# The two fucntions below can be used to find the inverse of a matrix
# and cahce it's value so that it needn't be calculated again. 

# This function creates a special "matrix" object that can cache 
# its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # caches the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # allows the matrix to be retrived
  get <- function() x
  
  # finds the inverse of the matrix
  setinv <- function(inverse) i <<- inverse
  
  # allows the inverse of the matrix to be retrieved
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then this function 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # inverse of matrix cached my previous function is found
  i <- x$getinv()
  
  # checks whether the matrix has been changed
  if(!is.null(i)) {
    
    # if it has been changed return cached inverse matrix
    message("getting cached data")
    return(i)
  }
  
  # if it hasn't been changed the inverse matrix is then found
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

