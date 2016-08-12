## The following functions cache the inverse of a matrix, assuming the matrix
## provided is always invertible.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## Steps
# 1. It sets the value of the matrix
# 2. It gets the value of the matrix
# 3. It sets the value of the inverse of the matrix
# 4. It gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinvers <- function(inverse) im <<- inverse
  getinvers <- function() im
  list(set = set, get = get, setinvers = setinvers, 
       getinvers = getinvers)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  im <- x$getinvers()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...) # leave out ...?
  x$setinvers(im)
  im
}

# Example run:

# a_matrix <- makeCacheMatrix(matrix(2:5, 2, 2))
# a_matrix$get()
# a_matrix$getinvers()
# cacheSolve(a_matrix)
# a_matrix$getinvers()
