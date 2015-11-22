## This function create a special matrix and sets its properties.it sets and read 
## the value of the matrix and its inverse. it doesn't do computation.its aim
## is to store the differents values. Mainly it store the inverse of the matrix 
## and avoid to recompute if the matrix doesn't change

## x is supposed to be invertible. so it is square and determinant(x) is not zero

makeCacheMatrix <- function(x = matrix()) {
  invma <- NULL
  set <- function(ma=matrix()) {
    ## To avoid to initialize the inverse of the matrix only because 
    ## it is renamed.
    z<-x
    x <<- ma
    if (dim(x) == dim(z) && all(x == z)){
      message("you reentered the same matrix")
      
    } else {
      
      invma <<- NULL ## initialize the inverse if the matrix change
    }
  }
  get <- function() x
  setinvma <- function(sol) invma <<- sol
  getinvma <- function() invma
  list(set = set, get = get, setinvma = setinvma,getinvma = getinvma)
}


## This function make computation and store the result in the special matix
## Mainly it compute the inverse of the matrix if the matrix change otherwise
## it read it in the cache

cacheSolve <- function(x, ...) {
        
  invma <- x$getinvma() 
  if(!is.null(invma)) {
    message("getting cached data")
    return(invma) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  invma <- solve(data)
  x$setinvma(invma)
  invma ## Return a matrix that is the inverse of 'x'
}
