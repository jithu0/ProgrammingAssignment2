## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {         #function to create object to cache the inverse of a matrix
  inv <- NULL                                       #create inv initially as null to capture the inverse of the matrix
  set <- function(y) {                              #creating the 'set' function
    x <<- y                                         #assigning value of matrix
    inv <<- NULL                                    #setting inv to NULL if the matrix is changed.
      }

  get <- function()x                                #creating the get function
  
  setinverse <- function(inverse) inv <<- inverse   #applying the values of inverse(
  getinverse <- function() inv                      #getting the values of inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {                       #checks if the inv is null, if not then getting the cached data skipping the computation
    message("getting cached data")          #displaying the message
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv                                       #Returns inverse of the matrix
}
