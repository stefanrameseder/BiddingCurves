bqSpline <- function(x,m){
  tk=0:(m-1)/(m-1)*(max(x)-min(x))+min(x)
  k=3
  t=1:(m+2*k-2)*0
  t[1:(k-1)]=min(x);t[(m+k):(m+2*k-2)]=max(x)
  t[k:(m+k-1)]=tk
  n=length(x)
  sm=1e-8
  h=t[4]-t[3]
  
  bmat=matrix(1:(n*(m+k-2))*0,nrow=n)
  index=x>=t[3]&x<=t[4]
  bmat[index,1]=(t[4]-x[index])^2
  bmat[index,2]=2*(x[index]-t[2])*(t[4]-x[index])+(t[5]-x[index])*(x[index]-t[3])
  index=x>=t[4]&x<=t[5]
  bmat[index,2]=(t[5]-x[index])^2
  for( j in 3:(m-1) ){
    index=x>=t[j]&x<=t[j+1]
    bmat[index,j]=(x[index]-t[j])^2
    index=x>=t[j+1]&x<=t[j+2]
    bmat[index,j]=(x[index]-t[j])*(t[j+2]-x[index])+(x[index]-t[j+1])*(t[j+3]-x[index])
    index=x>=t[j+2]&x<=t[j+3]
    bmat[index,j]=(t[j+3]-x[index])^2
  }
  index=x>=t[m]&x<=t[m+1]
  bmat[index,m]=(x[index]-t[m])^2
  index=x>=t[m+1]&x<=t[m+2]
  bmat[index,m]=(x[index]-t[m])*(t[m+2]-x[index])+2*(x[index]-t[m+1])*(t[m+3]-x[index])
  index=x>=t[m+1]&x<=t[m+2]
  bmat[index,m+1]=(x[index]-t[m+1])^2
  
  #################################################
  # plotting splines
  
  xpl=0:1000/1000*(max(x)-min(x))+min(x)
  bpl=matrix(1:(1001*(m+k-2))*0,nrow=1001)
  index=xpl>=t[3]&xpl<=t[4]
  bpl[index,1]=(t[4]-xpl[index])^2
  bpl[index,2]=2*(xpl[index]-t[2])*(t[4]-xpl[index])+(t[5]-xpl[index])*(xpl[index]-t[3])
  index=xpl>=t[4]&xpl<=t[5]
  bpl[index,2]=(t[5]-xpl[index])^2
  for( j in 3:(m-1) ){
    index=xpl>=t[j]&xpl<=t[j+1]
    bpl[index,j]=(xpl[index]-t[j])^2
    index=xpl>=t[j+1]&xpl<=t[j+2]
    bpl[index,j]=(xpl[index]-t[j])*(t[j+2]-xpl[index])+(xpl[index]-t[j+1])*(t[j+3]-xpl[index])
    index=xpl>=t[j+2]&xpl<=t[j+3]
    bpl[index,j]=(t[j+3]-xpl[index])^2
  }
  index=xpl>=t[m]&xpl<=t[m+1]
  bpl[index,m]=(xpl[index]-t[m])^2
  index=xpl>=t[m+1]&xpl<=t[m+2]
  bpl[index,m]=(xpl[index]-t[m])*(t[m+2]-xpl[index])+2*(xpl[index]-t[m+1])*(t[m+3]-xpl[index])
  index=xpl>=t[m+1]&xpl<=t[m+2]
  bpl[index,m+1]=(xpl[index]-t[m+1])^2
  
  
  #################################################
  
  slopes=matrix(0,ncol=m,nrow=m+k-2)
  slopes[1,1]=-2*h
  slopes[m+k-2,m]=2*h
  slopes[2,1]=4*h
  slopes[2,2]=-2*h
  if(m==4){slopes[3,2]=2*h;slopes[3,3]=-2*h}
  slopes[m+k-3,m]=-4*h
  slopes[m+k-3,m-1]=2*h
  if(m>4){
    for(j in 3:(m+k-4)){
      slopes[j,j-1]=2*h
      slopes[j,j]=-2*h
    }
  }
  
  bmat[,1]=bmat[,1]*2
  bmat[,m+1]=bmat[,m+1]*2
  slopes[1,]=slopes[1,]*2
  slopes[m+1,]=slopes[m+1,]*2
  bpl[,1]=bpl[,1]*2
  bpl[,m+1]=bpl[,m+1]*2
  mb=max(bpl)
  slopes=slopes/mb
  bpl=bpl/mb
  bmat=bmat/mb
  
  
  ans=new.env()
  ans$edges=bmat
  ans$slopes=slopes
  ans$knots=tk
  ans$xpl=xpl
  ans$bpl=bpl
  ans
}
