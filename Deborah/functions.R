
generate_ladder <- function(skills, K=10){
  
  n = length(skills)
  V = matrix(0,n,n) # matrices of victories: Vij=1 if j beats i 
  rdm = runif(K*n*(n-1)/2)
  k=1
  for (i in seq_len(n-1)){
    
    for (j in seq_len(K)){
      
      if (rdm[k] < (pi[i]/(pi[i] + pi[i+1]))){
        V[i,i+1] =  V[i,i+1] + 1
      } else {
        V[i+1,i] = V[i+1,i] + 1
      }
      k = k + 1
      
    }
  }
  return(V)
  
}