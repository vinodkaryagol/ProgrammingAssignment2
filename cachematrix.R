# makeCacheMatrix function takes any sqaure matrix (invertible matrix)
# as an argument and calculates inverse of square matrix using solve function

makeCacheMatrix<-function(x=matrix()){
  matrixInverseCopy<-NULL      # Initialized matrixInverseCopy with NULL
  set<-function(y){
    x <<- y                     # Assign value of matrix i.e. 'x'
    matrixInverseCopy <<- NULL         
  }
  get <- function() x        # get value of matrix i.e. 'x'
  
  # setInverse function calcualtes inverse of matrix 'x' and assign the 
  # result to matrixInverseCopy
  setInverse <- function (solve) matrixInverseCopy <<- solve
  # getInverse function is used to get inverse of matrix 'x'
  getInverse <- function() matrixInverseCopy
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

# cacheSolve function takes matrix as an argument and checks whether matrix
# inverse is calculated by makeCacheMatrix function. If it is calculated than
# it print message by returning a inverse of matrix else calculate  
# Inverse of matrix 'x' and return it

cacheSolve<-function(x, ...){
  matrixInverse <- x$getInverse()  # Retrieve matrix inverse 
  if(!is.null(matrixInverse)){   # If matrix inverse is calculated
    message("Getting cached data: Matrix inverse is already calculated")
    return(matrixInverse) # Return a matrix that is the inverse of 'x'
  }
  data <- x$get()      #  Retrieve a matrix using get method
  matrixInverse <- solve(data, ...)  # Calculate inverse of matrix
  x$setInverse(matrixInverse) # Set value of inverse of matrix
  (matrixInverse)  # Return a matrix that is the inverse of 'x'
}
