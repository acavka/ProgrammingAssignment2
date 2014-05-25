## A set of functions to calculate the inverse of a matrix and
## then cache the value so that if it is required again we do 
## not need to recalculate its value

## Calculates the inverse of a matrix and caches it in case it is 
## required later

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list (set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## First checks to see if matrix inverse has already been 
## calculated and if so returns the cached value, otherwise 
## it calculates and returns the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  {
    m <- x$getmatrix()
    if(!is.null(m)) 
    {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setmatrix(m)
    m
  }
}