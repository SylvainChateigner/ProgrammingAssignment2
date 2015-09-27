## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  tmp<-NULL
  set<-function(y){
  x<<-y
  tmp<<-NULL
}
get<-function() x
defmatrice<-function(solve) tmp<<- solve
recupmatrice<-function() tmp
list(set=set, get=get, defmatrice=defmatrice, recupmatrice=recupmatrice)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    tmp<-x$recupmatrice()
    if(!is.null(tmp)){
      message("getting cached data")
      return(tmp)
    }
    matrice<-x$get
    tmp<-solve(matrice, ...)
    x$defmatrice(tmp)
    tmp
}
