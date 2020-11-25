## Put comments here that give an overall description of what your
## functions do

##For a very long vector, it may take too long to compute the its inverse, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the inverse so that when we need it again, it can be looked up in the cache rather than recomputed. this function will take advantage of the scoping rules of the R language to preserve state inside of an R object.

## Write a short comment describing this function

## The following function, creates a special "matrix" object that can cache its inverse

## set the inverse of the vector
## get the inverse of the vector

makeCacheMatrix <- function(x = matrix()) {

  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)


}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
