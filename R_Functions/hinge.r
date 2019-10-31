hinge <- function(y,sigma,p){
  
  n=length(y)
  m=length(sigma)/n
  sm=0.00000001
  
  # Do Gram-Schmidt orthogonalization of first p rows
  # but without the last ten rows
  
  if(p>0){
    q1=sigma[1,]/sqrt(sum(sigma[1,]^2))
  }
  q=matrix(q1,ncol=1,nrow=n)
  if(p>1){
    for(i in 2:p){
      q1=sigma[i,]-q%*%solve(t(q)%*%q)%*%t(q)%*%sigma[i,]
      q1=q1/sqrt(sum(q1^2))
      q=cbind(q,q1)
    }
  }
  l=p
  if(p>0){
    h=1:p
    ones=1:p*0+1
    r=diag(ones,nrow=p,ncol=p)
  }
  
  # add one edge to start
  
  check=0
  if(p==0){rhat=y}
  if(p>0){rhat=y-q%*%solve(t(q)%*%q)%*%t(q)%*%y}
  b2=sigma%*%rhat
  
  if(max(b2[(p+1):m])>sm){
    obs=(p+1):m
    i=min(obs[b2[(p+1):m]==max(b2[(p+1):m])])
    l=p+1
    if(p==0){
      q=sigma[i,]
      q=q/sqrt(sum(q^2))
      h=i
      r=matrix(1,nrow=1,ncol=1)
    }
    if(p>0){
      q1=sigma[i,]-q%*%solve(t(q)%*%q)%*%t(q)%*%sigma[i,]
      q1=q1/sqrt(sum(q1^2))
      q=cbind(q,q1)
      h[l]=i
      r=t(q)%*%t(sigma[h,])
    }
  }
  if(max(b2[(p+1):m])<(sm)){
    check=1
  }
  if(check==1){a=t(q)%*%y}
  # LOOP starts here:
  nrep=0
  while(check==0 & nrep<1000){
    nrep=nrep+1
    # Fit data to current EDGES
    
    a=t(q)%*%y
    
    # check if convex: 
    #first find the b vector:
    b=1:l*0
    if(l>1){b[l]=a[l]/r[l,l]
            for( j in (l-1):1){
              b[j]=a[j]
              for(i in (j+1):l){
                b[j]=b[j]-r[j,i]*b[i]
              }
              b[j]=b[j]/r[j,j]
            }
    }else{b[l]=a[l]/r}
    
    #check to see if b positive
    
    if(l>p){
      obs=(p+1):l
      i=obs[b[(p+1):l]==min(b[(p+1):l])]
      if(b[i]<(-sm)){
        # if not, remove hinge, make new q and r
        c1=0
        if(i>1){h=c(h[1:(i-1)],h[(i+1):l])}else{h=h[2:l]}
        l=l-1
        if(i>1){
          q=q[,1:(i-1)]
          for( j in i:l){
            qnew=sigma[h[j],]-q%*%t(q)%*%sigma[h[j],]
            qnew=qnew/sqrt(sum(qnew^2))
            q=cbind(q,qnew)
          }
          r=t(q[,1:l])%*%t(sigma[h,])
        }
        if(i==1&l>1){
          q[,1]=sigma[h[1],]/sqrt(sum(sigma[h[1],]^2))
          for(j in 2:l){
            qnew=sigma[h[j],]-q[,1:(j-1)]%*%t(q[,1:(j-1)])%*%sigma[h[j],]
            qnew=qnew/sqrt(sum(qnew^2))
            q=cbind(q[,1:(j-1)],qnew)
          }
          r=t(q[,1:l])%*%t(sigma[h,])
        }	
        if(i==1&l==1){
          q[,1]=sigma[h[1],]/sqrt(sum(sigma[h[1],]^2))
          r=matrix(1,nrow=1,ncol=1)
        }	
        q=q[,1:l]
      }
    }
    if(b[i]>(-sm)) {
      c1=1
      #
      # now see if we need to add another hinge
      #
      theta=q%*%t(q)%*%y
      rhat=y-theta
      b2=sigma%*%rhat
      
      # check to see if b2 negative
      
      obs=(p+1):m
      i=min(obs[b2[(p+1):m]==max(b2[(p+1):m])])
      if(l<m&l>0){if(b2[i]>sm){
        l=l+1
        qnew=sigma[i,]-q%*%t(q)%*%sigma[i,]
        qnew=qnew/sqrt(sum(qnew^2))
        q=cbind(q,qnew)
        h=c(h,i)
        r=t(q)%*%t(sigma[h,])
        c2=0
      }}
      if(b2[i]<sm){c2=1}
      check=c1*c2
      h
    }
  }
  # find coefficient vector
  b=1:l*0
  b[l]=a[l]/r[l,l]
  if(l>1){
    for( j in (l-1):1){
      b[j]=a[j]
      for(i in (j+1):l){
        b[j]=b[j]-r[j,i]*b[i]
      }
      b[j]=b[j]/r[j,j]
    }
  }
  coef=1:m*0
  coef[h]=b
  coef
}
########################################
#       MAKE THE EDGE VECTORS          #
########################################
