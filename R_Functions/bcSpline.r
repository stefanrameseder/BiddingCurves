

bcSpline <- function(x,m){
 ##############################################################
# cubic b-splines
# returns basis functions and 2nd derivative of basis functions
# at the knots
##############################################################
  tk=0:(m-1)/(m-1)*(max(x)-min(x))+min(x)
  k=4
  t=1:(m+2*k-2)*0
  t[1:(k-1)]=min(x);t[(m+k):(m+2*k-2)]=max(x)
  t[k:(m+k-1)]=tk
  n=length(x)
  sm=1e-8
  
  m0=matrix(1:(n*(m+k+1))*0,nrow=n)
  for( j in 4:(m+k-1) ){
    if(t[j]<t[j+1]-sm){
      index=x<=t[j+1]&x>t[j]
      m0[index,j]=1
    }
  }
  m0[1,k]=1
  
  m1=matrix(1:(n*(m+k))*0,nrow=n)
  for( j in 3:(m+k-1) ){
    index=x>t[j]&x<=t[j+2]
    if(t[j+1]>t[j]+sm){
      p1=(x[index]-t[j])/(t[j+1]-t[j])*m0[index,j]
    }else{p1=0}
    if(t[j+2]>t[j+1]+sm){
      p2= (t[j+2]-x[index])/(t[j+2]-t[j+1])*m0[index,j+1]
    }else{p2=0}
    m1[index,j]=p1+p2
  }
  imin=x==min(x)
  m1[imin,k-1]=1
  
  
  m2=matrix(1:(n*(m+k-1))*0,nrow=n)
  for( j in 2:(m+k-1) ){
    index=x>t[j]&x<=t[j+3]
    if(t[j+2]>t[j]+sm){
      p1=(x[index]-t[j])/(t[j+2]-t[j])*m1[index,j]
    }else{p1=0}
    if(t[j+3]>t[j+1]+sm){
      p2=(t[j+3]-x[index])/(t[j+3]-t[j+1])*m1[index,j+1]
    }else{p2=0}
    m2[index,j]=p1+p2
  }
  m2[imin,k-2]=1
  
  
  m3=matrix(1:(n*(m+k-2))*0,nrow=n)
  for( j in 1:(m+k-2) ){
    index=x>=t[j]&x<=t[j+4]
    if(t[j+3]>t[j]+sm){
      p1=(x[index]-t[j])/(t[j+3]-t[j])*m2[index,j]
    }else{p1=0}
    if(t[j+4]>t[j+1]+sm){
      p2=(t[j+4]-x[index])/(t[j+4]-t[j+1])*m2[index,j+1]
    }else{p2=0}
    m3[index,j]=p1+p2
  }
  
  #  plotting splines
  
  np=1000
  xpl=0:(np-1)/(np-1)*(max(x)-min(x))+min(x)
  m0pl=matrix(1:(np*(m+k+1))*0,nrow=np)
  for( j in 4:(m+k-1) ){
    if(t[j]<t[j+1]-sm){
      index=xpl<=t[j+1]&xpl>t[j]
      m0pl[index,j]=1
    }
  }
  m0pl[1,k]=1
  m1pl=matrix(1:(np*(m+k))*0,nrow=np)
  for( j in 3:(m+k-1) ){
    index=xpl>t[j]&xpl<=t[j+2]
    if(t[j+1]>t[j]+sm){
      p1=(xpl[index]-t[j])/(t[j+1]-t[j])*m0pl[index,j]
    }else{p1=0}
    if(t[j+2]>t[j+1]+sm){
      p2= (t[j+2]-xpl[index])/(t[j+2]-t[j+1])*m0pl[index,j+1]
    }else{p2=0}
    m1pl[index,j]=p1+p2
  }
  m1pl[1,k-1]=1
  m2pl=matrix(1:(np*(m+k-1))*0,nrow=np)
  for( j in 2:(m+k-1) ){
    index=xpl>t[j]&xpl<=t[j+3]
    if(t[j+2]>t[j]+sm){
      p1=(xpl[index]-t[j])/(t[j+2]-t[j])*m1pl[index,j]
    }else{p1=0}
    if(t[j+3]>t[j+1]+sm){
      p2=(t[j+3]-xpl[index])/(t[j+3]-t[j+1])*m1pl[index,j+1]
    }else{p2=0}
    m2pl[index,j]=p1+p2
  }
  m2pl[1,k-2]=1
  m3pl=matrix(1:(np*(m+k-2))*0,nrow=np)
  for( j in 1:(m+k-2) ){
    index=xpl>=t[j]&xpl<=t[j+4]
    if(t[j+3]>t[j]+sm){
      p1=(xpl[index]-t[j])/(t[j+3]-t[j])*m2pl[index,j]
    }else{p1=0}
    if(t[j+4]>t[j+1]+sm){
      p2=(t[j+4]-xpl[index])/(t[j+4]-t[j+1])*m2pl[index,j+1]
    }else{p2=0}
    m3pl[index,j]=p1+p2
  }
  
  # matrix of second derivatives
  
  secder=matrix(0,ncol=m,nrow=m+k-2)
  secder[1,1]=6
  secder[2,1]=-9
  secder[2,2]=3/2
  secder[3,1]=3
  secder[3,2]=-5/2
  secder[3,3]=1
  if(m>4){
    for(j in 4:(m-1)){
      secder[j,j-2]=1;secder[j,j-1]=-2;secder[j,j]=1
    }
  }
  secder[m,m-2]=1
  secder[m,m-1]=-5/2
  secder[m,m]=3
  secder[m+1,m-1]=3/2
  secder[m+1,m]=-9
  secder[m+2,m]=6
  
  ans=new.env()
  ans$bpl=m3pl
  ans$xpl=xpl
  ans$edges=m3
  ans$d2=secder
  ans$knots=tk
  ans
}

