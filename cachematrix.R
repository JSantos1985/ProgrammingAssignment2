
# Test matrix!
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

## I simply repurposed the example cached mean function to use solve instead of mean.

## This function takes a matrix "x". First, it sets "m" (which is the cache) to NULL. First, it defines a function
## "set" which takes "y" as an argument and stores it in the outer scope as "x", and also resets "m"
## to NULL, clearing existing cached data.
## The second function "get" simply "returns "x". The next function "setmatrix" takes an inverted matrix obtained
## from cacheSolve, and stores it in the cache "m". The final function "getmatrix" fetches cached data from "m".
## The function returns a list of all these four functions, for usage in combination with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solved) m <<- solved
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function takes a matrix object "x" created by makeCacheMatrix. First, it calls getmatrix on the object 
## to fetche the value of "m". If it is NOT NULL, then cached data is available. It states that this is the case, 
## and returns the cached data "m". Otherwise, it calls get to fetch the data, applies solve and stores this as "m",
## and finally calls setmatrix to cache the solution on the object as "m". Returns "m" as the solution.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
