s<-expand.grid(r=1:4, c=1:4, a_h =c(1,0,-1), a_v=c(1,0,-1))
S_terminal =list(c(1,1), c(4,4)) #### terminal states for the grid world
pi <- s[!((s$a_h+s$a_v) %in% c(0,2,-2)),]
pi <- pi[!((pi$r+pi$c) %in% c(2,8)),]
pi <- pi[((pi$r+pi$a_v)<5 & (pi$r+pi$a_v)>0),]
pi <- pi[((pi$c+pi$a_h)<5 & (pi$c+pi$a_h)>0),]

pi$Q <- -1  #### initialize Q values
alpha = 0.15   #### RL parameters alpha(learning rate), gamma(discount rate), e= probability of random exploration
gamma = 0.8
episodes = 8000
e=0.1



choose_a <- function(pi, e, s_r, s_c){
  event <- ifelse(runif(1) < (1-e),"max","rand") #### chooses randomly if maximum or exploratory actions are done
  A <- pi[(pi$r==s_r & pi$c == s_c),]
  if(nrow(A)==0){
    return(NA)
  }
   if (event == "max"){
    
    temp <- A[(A$Q==max(A$Q)),c("a_h","a_v")]
    if (length(temp) > 1){
      a_s <- temp[ceiling(runif(1,0,nrow(temp))),]
    }
    
  } else if(event=="rand") {
    a_s<- A[ceiling(runif(1,0,nrow(A))),c("a_h","a_v")]
  } 
return(a_s)
} #### a function which chooses an action based on current state (e-greedy or e-soft)


for(i in 1:episodes){
    rn <- ceiling(runif(1, min=0, max=nrow(pi)))
    s_r <- pi$r[rn]
    s_c <- pi$c[rn]
    
    a_s <- choose_a(pi, e, s_r, s_c)
  
  while(all.equal(c(s_r,s_c), S_terminal[[1]])!=1|all.equal(c(s_r,s_c), S_terminal[[2]])!=1) {   
    
    s_r_k= s_r +a_s$a_v
    s_c_k = s_c +a_s$a_h
  
    a_s_k= choose_a(pi, e, s_r_k, s_c_k)
    if(is.na(any(a_s_k))==TRUE){
      if(all.equal(c(s_r_k,s_c_k), S_terminal[[1]])==1|all.equal(c(s_r_k,s_c_k), S_terminal[[2]])==1){
        pi[(pi$r==s_r & pi$c==s_c & pi$a_h==a_s$a_h & pi$a_v==a_s$a_v),]$Q = 0
        break
        }else{
          break
          }
    }

    Q_sa <- pi[(pi$r==s_r & pi$c==s_c & pi$a_h==a_s$a_h & pi$a_v==a_s$a_v),]$Q  #### calling Q values from Q table
    Q_sa_k <- pi[(pi$r==s_r_k & pi$c==s_c_k & pi$a_h==a_s_k$a_h & pi$a_v==a_s_k$a_v),]$Q 
    
    R  <- -1 #### Reward
    pi[(pi$r==s_r & pi$c==s_c & pi$a_h==a_s$a_h & pi$a_v==a_s$a_v),]$Q <- Q_sa + alpha*(R + gamma*Q_sa_k-Q_sa) #### SARSA update function
    
    
    s_r <- s_r_k #### updating states and actions
    s_c <- s_c_k
    a_s$a_v <-a_s_k$a_v
    a_s$a_h  <- a_s_k$a_h
  }

}

print(pi$Q)

pi$r_k <- pi$r+pi$a_v
pi$c_k <- pi$c +pi$a_h

r=4
c=2
pi[(pi$r==r & pi$c==c),]



