


#This function will create a matrix that will cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
  j<-NULL

  #The functions purpose is to change is to change the vector stored in the main matrix  
  set<-function(y){
    x<<-y
    j<<-NULL

  }
  
  #The function will return the vector stored in the main function
  get<-function() x
  
  setinverse<-function(inverse) j<<-inverse
  getinverse<-function() j
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#The function will calculate the inverse of the matrix returned by "makeCacheMatrix" function.
#If the inverse has been calcualted, the cache solve will extract the inverse from cache.
#If inverse hasn't been computed, the data will get the matrix stored with the first function (makeCacheMatrix),
#with j getting the inverse and x$setmean(j) storing it in j in the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  j<-x$getinverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  data<-x$get()
  j<-solve(data, ...)
  x$setinverse(j)
  j
}
