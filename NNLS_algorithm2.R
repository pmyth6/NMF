library(MASS)
NNLS = function(A, y, tol){
  #Initialise
  P = c() #set P to the empty set
  m = nrow(A)
  n = ncol(A)
  R = 1:n 
  x = numeric(n)
  s = numeric(n)
  w = t(A)%*%(y - A%*%x) #error vector
  wR = w[R] #wR is a sub-vector of w with indexes from R
  
  #Main loop
  while (length(R)!=0 && max(wR)>tol){
    j = R[which.max(wR)] #set j in R to be the index of the largest value in wR
    P = c(P, j) #add j to P
    R = R[R != j] #remove j from R
    AP = A[,P] #AP is a sub-matrix of A restricted to the columns of the indexes in P
    
    #sP is a sub-vector of s with indexes from P, and sR indexes from R
    sP = s[P] 
    sR = s[R] 
    sP = ginv(t(AP)%*%AP)%*%t(AP)%*%y #ginv computes the pseudo inverse
    sR[] = 0
    
    #set s to sR and sP with their respective indexing
    s[R] = sR 
    s[P] = sP
    
    #Nested loop - correct any negative values
    while (min(sP)<=0){
      xP = x[P] #xP is a sub-vector of x with indexes from P
      alpha = Inf
      for (i in 1:length(sP)){
        if (sP[i]<=0){
          alpha = min(alpha, xP[i]/(xP[i]-sP[i]))
        }
      }
      if (is.infinite(alpha)) {
        warning("No valid alpha found")
        break
      }
      x = x + alpha(s - x)
      R = P[x[P] <= 0] #move to R the indexes j in P such that x[j] <= 0
      P = setdiff(P, R) #remove those indexes j from P
      
      #reassign the sub-vectors sP and sR now that P and R have changed
      sP = s[P] 
      sR = s[R]
      
      #recalculate sP and sR
      sP = inv(t(AP)%*%AP)%*%t(AP)%*%y
      sR[] = 0
      
      #set s to sR and sP with their respective indexing
      s[R] = sR
      s[P] = sP
    }
    
    x = s #set x to s
    w = t(A)%*%(y - A%*%x) #set w to the new errors
    print(w)
    wR = w[R] #also set wR to the new errors
  }
  
  return(x)
}
