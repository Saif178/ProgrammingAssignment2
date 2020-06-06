makeCacheMatrix <- function(x = matrix(seq(3,12,3),2,2)) 
{
  #this function returns a list which sets the matrix, gets the matrix
  #and also sets the inverse and gets the inverse of the matrix
  
  #the argument x takes the matrix and by default it gives a 2 by 2
  #square matrix
  m <- NULL
  set <- function(y) 
  {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  return(list(set = set, get = get,
       setinv = setinv,
       getinv = getinv))
}


#

cacheSolve <- function(x, ...) 
{
  #this function takes the special matrix from makeCacheMatrix function
  #and finds the inverse of the matrix
  
  #if the inverse has already been found it returns the inverse of the matrix
  m <- x$getinv()
  if(!is.null(m)) 
  {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  return(m)
}
