## Put comments here that give an overall description of what your
## functions do

## Create a special structure which store inversse matrix m for
## matrix x if was calculated
makeCacheMatrix<-function(x=matrix()){
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m <<- solve
	getinverse<- function() m
	list(set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}

## Return a matrix that is the inverse of 'x', cached the value if
## it's not already cached
cacheSolve <- function(x, ...) {
   m<-x$getinverse()
   if(!is.null(m)) {
	    message("getting cached data")
		return(m)
	}
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
