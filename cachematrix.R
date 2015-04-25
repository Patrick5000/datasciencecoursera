makeCacheMatrix <- function(x = matrix()) {   
  #creates a special "matrix" object that can cache its inverse
  # applied to square non-singular matrix
  # result is used in function cacheSolve
  # setting the inv to NULL as a placeholder for a future value
  inv<-NULL
  set<-function(y){
    # superassignment <<-- assings value to an object in environment
    # different from current one
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) inv<<- solve
  getmatrix<-function() inv
  # list will return a list of functions to
  # set / get the matrix & set /get inverse
  # used as input for cacheSolve()
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}

# cacheSolve computes the inverse of matrix returned by makeCacheMatrix above
cacheSolve <- function(x=matrix(), ...) {
  
  inv<-x$getmatrix()
  #  inverse already been calculated
  if(!is.null(inv)){
    message("cached data")
    return(inv)
  }
  # if not,calculates the inverse 
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setmatrix(inv)
  inv
}

