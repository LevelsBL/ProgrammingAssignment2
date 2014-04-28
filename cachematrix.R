##-- COURSE:  	COURSERA : R PROGRAMMING : Programming Assignment 2
##-- CREATED:		  2014-04-27 22:07:46 EDT
##-- UPDATED:		
##-- PROGRAMMER:		BERTRAM LEVELS (BLL)
##-- COMPANY:		
##-- APPLICATION:	R STUDIO
##-- DESCRIPTION:	
##-- SYSTEM:	      WINDOWS 7 64BIT
##--  	PLATFORM       X86_64-W64-MINGW32          
##--		ARCH           X86_64                      
##--		OS             MINGW32                     
##--		SYSTEM         X86_64, MINGW32             
##--		STATUS                                     
##--		MAJOR          3                           
##--		MINOR          0.3                         
##--		YEAR           2014                        
##--		MONTH          03                          
##--		DAY            06                          
##--		SVN REV        65126                       
##--		LANGUAGE       R                           
##--		VERSION.STRING R VERSION 3.0.3 (2014-03-06)
##--		NICKNAME       WARM PUPPY                  

##-- VERSION HISTORY --##
##--       XX.XXX  	  YYYYMMDD  Author   Description
##     ver<-"01.000"   #:XXXXXXXX (BLL)  initial version, framework

makeCacheMatrix <- function(A = matrix(sample(1:9),3,3)){    
    Z <- NULL                  ## Matrix Inverse
  set <-function(y) {          ## Store matrix
    A <<- y
    Z <<-NULL                  ## Set the matrix inverse null
  }
  get <-function() A           ## Get the matrix
  getInverse <- function() Z   ## Get the matrix inverse
  setInverse <- function(m) Z <<- solve(A)   ## Solve the matrix 
  list(set=set, get=get, getInverse=getInverse, setInverse = setInverse) 
  
}

cacheSolve <-function(x, ...) {
  m <-x$getInverse()                  ## Return the iNverse of matrix
  if(!is.null(m)) {                   ## If there is cache data
    message("Retuning cached data")
    return(m)                       ## Just return the cache data
  }
  data <- x$get()                    ## Else obtain current matrix
  m <- solve(data, ...)              ## Compute the matrix inverse
  x$setInverse(m)                    ## Set the matrix inverse
  m                                   ## Return the result
}
