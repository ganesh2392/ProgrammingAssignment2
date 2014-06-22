makeCacheMatrix <- function(x = matrix()) {  # create a matrix
        m<-NULL
        set<-function(y){                    # set value of the matrix
             x<<-y
             m<<-NULL
       }
       get<-function() x                     # get value of the matrix 
       setmatrix<-function(solve) m<<- solve # set value of the solve
       getmatrix<-function() m               # get value of the solve
       list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)                  # list of functions
}

cacheSolve <- function(x=matrix(), ...) {    
       m<-x$getmatrix()                      
       if(!is.null(m)){                      # if inverse is already calculated cached value is returned
            message("getting cached data")
            return(m)
       }
       matrix<-x$get()                       # else inverse is calculated
       m<-solve(matrix, ...)
       x$setmatrix(m)
       m
}
