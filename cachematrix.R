## The first function takes a matrix and stores it in a list alongside a function
## to set and return its inverse. The second function returns the inverse  
## after checking if its already been cached

## This function takes a matrix mx and outputs 4 functions in a list. The functions in the list 
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(mx = matrix()) {
  inv <- NULL
  setmx <- function(y) {
    mx <<- y
    inv <<- NULL
  }
  getmx <- function() mx
  setinv <- function(mxinv) inv <<- mxinv
  getinv <- function() inv
  list(setmx=setmx, getmx=getmx, setinv=setinv, getinv=getinv)
  
}


## This function will either calcuate or recall from cache the inverse of the matrix
## which was output in the list x from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Test if the inverse matrix already exists
  ## If not create the inverse using the solve() function
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mx <- x$getmx()   ## returns the matrix
  inv <- solve(mx)  ## finds the inverse
  x$setinv(inv)     ## sets the inverse in the cache
  inv               ## returns the inverse matrix
  
  }
