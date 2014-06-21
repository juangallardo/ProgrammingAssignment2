## makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the inverse of the matrix
## 4 get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv_x <- NULL
  set <- function(x){
    y <<- x
    inv_x <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(mx) inv_x <<-mx
  getInverseMatrix <- function() inv_x
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getInverseMatrix()
  if (!is.null(inv)){
    message("getting cached matrix")
    return (inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$setInverseMatrix(inv)
  inv
}
