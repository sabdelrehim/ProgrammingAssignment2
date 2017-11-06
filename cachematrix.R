## Caching an inversed matrix and re-caching on matrix change  

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m<- NULL
  
  set <- function(y) {
    x <<- y
    invm <<- NULL

  }
  
  get <- function() x
  
  setimat <- function(invmat) invm <<- invmat
  
  getimat <- function() invm
  
  list(set = set, get = get,
       setimat = setimat,
       getimat = getimat)

}


## This function computes the inverse of the special "matrix"make

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getimat()
  if(!is.null(m)){
    m1 <- solve(x$get())
    if (identical(m, m1)) {
      message("getting inverted matrix cached data")
      return (m)
    }
  }
  data <- x$get()
  x$setimat(solve(data))
  x$getimat()
}
