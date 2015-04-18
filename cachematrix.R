
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- matrix(data = NA, nrow(x), ncol(x))
    set <- function(y = matrix())
    {
        x <<- y
        inverseMatrix <- matrix()
    }
    
    get <- function()
    {
        x
    }
    
    setInverseMatrix <- function(newInverseMatrix) 
    {
        inverseMatrix <<- newInverseMatrix
    }
    
    getInverseMatrix <- function()
    {
      inverseMatrix
    }
    
    list(set = set, 
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverseMatrix <- x$getInverseMatrix()
      naSum <- sum(is.na(inverseMatrix))
      if(naSum != nrow(inverseMatrix) * ncol(inverseMatrix))
      {
          message("get cached data")
          return(inverseMatrix)
      }
      
      data <- x$get()
      inverseMatrix <- solve(data)
      x$setInverseMatrix(inverseMatrix)
      inverseMatrix
}
