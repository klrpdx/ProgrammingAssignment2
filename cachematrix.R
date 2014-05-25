## These two functions work together to provide a caching mechanism for inverted matrices.
## They allow a matrix to be stored together with its inverse. Because caluclating the 
## inverse may take time, it only needs to be calculated once and is then stored so
## it can be retrieved any time it is needed.




## makeCacheMatrix returns a list built around the supplied matrix. Having the list
## allows you to get the matrix or its inverse whenever you need it. Additionally, 
## the matrix used by this list can be set at any time. Note that when setting a new
## matrix, its inverse will need to be calculated again and stored.
##
## Four 'methods' are exposed for use:
## set(matrix) - store a matrix.
## get() - get the matrix previously stored, or an empty matrix if one has not yet been set.
## setInverse(inv) - store an inverse of a matrix
## getInverse() - get the inverse of a matrix, or NULL if it has not been set.
##
makeCacheMatrix <- function(x = matrix()) {
  
  inverted <- NULL
  
  # Store a matrix. Set inverted to NULL so we know we need a new inversion calculated.
  # The <<- operators cause the value to be stored in the existing vars in the parent environment,
  # otherwise new vars existing only in the run-time enviroment would be created, and nothing could
  # be cached.
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  
  # Get the stored matrix
  get <- function() {
    x
  }
  
  # Store the inversion of a matrix. It's assumed it is the inversion of our stored
  # matrix, but there's no guarentee
  setInverse <- function(invMatrix) {
    inverted <<- invMatrix
  }
  
  # Return the stored inverse, or NULL if it has not been calculated yet.
  getInverse <- function() {
    inverted
  }
  
  #Create and return a list containing the functions created above.
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## cacheSolve is a function that returns an inverse of a matrix. Because it uses
## our special makeCacheMatrix, cacheSolve will return the cached value of the inversion
## if it is available and save computation time.
## If the inverse is not available in the cache, this function uses the 
## solve() function to create the inverse and then stores it in makeCacheMatrix
## so it won't need to be calculated again. 
##
## The '...' arguments in the function declaration are simply passed into solve() but
## are not used by the cacheSolve function itself.

cacheSolve <- function(x, ...) {
  
  # xirtam is the matrix inverted. Get it? Just a little programming humor...
  xirtam <- x$getInverse()

  # If no inverse in the cache, announce it, create one, and store it.
  if (is.null(xirtam)) {
    message("No cached value found, inverting matrix")
    xirtam <- solve(x$get(),...)
    x$setInverse(xirtam)
  }
  #Otherwise, just announce we're using the cached version
  else {
    message("Returning cached value")
  } 
  
  xirtam
}
