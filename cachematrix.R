## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## The following pair of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set_mat <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(solve_mat) inv_matrix <<- solve_mat
  get_inv <- function() inv_matrix
  list(set_mat = set_mat, get_mat = get_mat,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve retrieves the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_inv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get_mat()
  inv_matrix <- solve(data, ...)
  x$set_inv(inv_matrix)
  inv_matrix
}
