
makeCacheMatrix <- function(x = matrix()){
  n <- NULL
  get <- function() x
  setinv <- function(inv) n <- inv
  getinv <- function() n
  list(get = get,
       setinv = setinv,
       getinv = getinv)
  }
  
cacheSolve <- function(x,...){
  n <- x$getinv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
 

 
##Example
t <- matrix(c(1,3,3,5,5,7,7,9,9), ncol = 3, nrow = 3)  
my_matrix <- makeCacheMatrix(t)
cacheSolve(my_matrix)
