## Put comments here that give an overall description of what your
## functions do
## F. Schebelle, 2017-02.19

##############################################
# Special matrix with inverted amtrix cached,
# if calcutlated previousely.
#
# x:     R object of type square matrix 
#         (e.g. matrix(c(2,7,5,2,8,4,3,7,6),
#           nrow = 3, ncol=3, byrow = TRUE))
#        MUST BE invertible det(x) != 0
#
makeCacheMatrix <- function(x = matrix()) {
  #Inverted matrix cache
  s <- NULL
  
  # set square matrix object
  set <- function(y) {
    x <<- y
    # reset cache
    s <<- NULL
  }
  # get square matrix
  get <- function() x
  # store inverse matrix into cache
  setsolve <- function(solve) s <<- solve
  # get inverse matrix from cache
  getsolve <- function() s
  
  # Object operations
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##############################################
# Calculates the inverse of a matrix and
# caches the result.
# x:      R object of type
#         makeCacheMatrix.
cacheSolve <- function(x, ...) {
    # Prepare the cache
    s <- x$getsolve()
    # If cached data, return immediately
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    # else
    # get the matrix
    data <- x$get()
    # invert the square matrix
    s <- solve(data, ...)
    # store in cache
    x$setsolve(s)
    ## Return a matrix that is the inverse of 'x'
    s
}
