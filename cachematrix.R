# The fucntions takes a matrix as input and caches its inverse; and then returns the inverse of matrix
# If the inverse is already calculated then it just returns the cached value of the matrix.


# This function caches the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}

# This function solves the given matrix for its inverse and if the inverse has already been calculated then
# it retrieves the cached matrix

cacheSolve <- function(x=matrix(), ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}
