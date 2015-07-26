#makeCacheMatrix
#------------------------
#The makeCasheMatrix simply takes any invertible matrix as its argument.
#It then stores the inverse of that matrix in the global environment. 

makeCacheMatrix <- function(y) {
  inv_mat <<- solve(y)         #compute inverse
}


#cacheSolve
#------------------------
#Assuming you have already run the makeCacheMatrix function (which means you would have 
#two objects in the global environment: the matrix y, and its inverse), cacheSolve
#does one of two things:
# 1)If the inputed matrix x is 
#   a).of the same dimensions as y
#   b).contains the same values as y
#   Then simply take the cached inverted value inv_mat from the makeCacheMatrix function.
# 2)If either condition a or b from step 1 above is violated, compute the inverse of 
#   matrix x and store it as the new inv_mat value. Also, store x in y. 

cacheSolve <- function(x) {
  if(sum(dim(y) - dim(x))==0){ #dimension comparison
    if(sum(x-y)!=0){           #value comparison
      inv_mat <<- solve(x)     #compute inverse
      y <<- x                  #store x in y
      inv_mat                  #print inverse
    } else {
      inv_mat                  #print inverse
    }
  } else {
    inv_mat <<- solve(x)       #compute inverse
    y <<- x                    #store x in y
    inv_mat                    #print inverse
  }
}