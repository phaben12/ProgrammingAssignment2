## Put comments here that give an overall description of what your functions do
## A pair of functions that cache the inverse of a matrix


## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL     # initialize the inverse property
  if (class(try(solve(x), silent = T)) != "try-error"){
    #print("i'm here!")
    set = function(y){
      x <<-y
      inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }else{
    print("x not an invertible matrix")
  }
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  #get x's invert from cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }else{# calculate x's invert
    mat = x$get()
    inv = solve(mat, ...)
    x$setinv(inv)
    return(inv)
  }
}


