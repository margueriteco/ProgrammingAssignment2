## The following two functions serve the purpose of caching
##the inverse of a matrix so that it does not need to be 
##recomputed. If the inverse has is not in the cache it will
##compute the inverse. 

## makeCacheMatrix creates a list containing a function to...
##1. Set the value of matrix (x)
##2. get the value of matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setI<-function(inverse) m<<-inverse
	getI<-function()m
	list(set=set,get=get,setI=setI,getI=getI)
}

##cacheSolve returns the inverse of the matrix either by 
##returning the value (if it has already been computed, and
##is present in the cache) or computing it.  It then sets the
##value in the cache to prevent subsequent calculations. 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getI()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setI(m)
	m
}
