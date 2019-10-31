


penSpline <- function(type, x, y, k, q, pen, sm = 1e-8){
  #set.seed(123)
  #q = 2
  #x=rnorm(100, mean=5)
  #y=sort(abs(rnorm(100, mean=3)))
  #k=27
  #pen=1
  #sm = 1e-8
  #type=1
  #################################################################################
  # Code for penalized spline with inputs
  # q = q th order difference penalties, q=0 to 3 for function, 1st derivative, ... 
  # x = vector of x values
  # y = vector of y values (together a scatterplot x_i, y_i)
  # k = number of total knots (i.e. k-2 interior knots), 
  # pen = penalty parameter
  # type (constrain type):  
  # - 1=monotone increasing 
  # - 2=monotone decreasing
  # - 3=convex
  # - 4=concave
  # - 5=convex increasing
  # - 6=convex decreasing
  # - 7=concave increasing
  # - 8=concave decreasing
  # sm = 
  #
  # output: 
  # - cfit = constrained fit
  # - ucfit = unconstrained fit
  # - cgcv = constrained GCV
  # - ucgcv = unconstrained GCV
  # - edfc = effective degrees of freedom for constrained fit
  # - edfu = effective degrees of freedom for unconstrained fit
  # - knots
  # - xpl = grid of points for plotting smooth fits
  # - cpl = constrained fit values at xpl
  # - ucpl = unconstrained fit values at xpl
  #################################################################################
  
  
  n <- length(y)
  
  if(type<=2){
    ans <- bqspline(x,k)
  }else{
    ans=bcspline(x,k)
  }
  m=length(ans$edges)/n
  
  delta=t(ans$edges)
  dmat=matrix(0,nrow=m-q,ncol=m)
  #  third-order
  if(q==3){
    for(i in 4:m){
      dmat[i-3,i-3]=1
      dmat[i-3,i-2]=-3
      dmat[i-3,i-1]=3
      dmat[i-3,i]=-1
    }
  }
  # second order
  if(q==2){
    for(i in 3:m){
      dmat[i-2,i-2]=1;dmat[i-2,i-1]=-2;dmat[i-2,i]=1
    }
  }
  # first order
  if(q==1){
    for(i in 2:m){
      dmat[i-1,i-1]=1;dmat[i-1,i]=-1
    }
  }
  # zero order
  if(q==0){
    for(i in 1:m){
      dmat[i,i]=1
    }
  }
  # if q is anything else, no penalty
  qmat=delta%*%t(delta)+pen*t(dmat)%*%dmat
  umat=chol(qmat)
  if(type<=2){
    smat=ans$slopes
  }else{smat0=ans$d2}
  if(type==2){smat=-smat}
  if(type==3){smat=smat0}
  if(type==4){smat=-smat0}
  if(type==5){
    smat=matrix(0,ncol=m-1,nrow=m)
    smat[,1:(m-2)]=smat0
    smat[1,m-1]=-1;smat[2,m-1]=1
  }
  if(type==6){
    smat=matrix(0,ncol=m-1,nrow=m)
    smat[,1:(m-2)]=smat0
    smat[1,m-1]=1;smat[2,m-1]=-1
  }
  if(type==7){
    smat=matrix(0,ncol=m-1,nrow=m)
    smat[,1:(m-2)]=-smat0
    smat[m-1,m-1]=-1;smat[m,m-1]=1
  }
  if(type==8){
    smat=matrix(0,ncol=m-1,nrow=m)
    smat[,1:(m-2)]=-smat0
    smat[m-1,m-1]=1;smat[m,m-1]=-1
  }
  xpl=ans$xpl
  bpl=ans$bpl
  knots=ans$knots
  uinv=solve(umat)
  # make cone edges
  bmata=t(smat)%*%uinv
  bmat=matrix(0,ncol=m,nrow=m)
  if(type==3|type==4){
    perpmat=matrix(nrow=2,ncol=m)
    uvec=matrix(runif(2*m),nrow=m,ncol=2)
    perpmat=uvec-t(bmata)%*%solve(bmata%*%t(bmata))%*%bmata%*%uvec
    bmat[1:2,]=t(perpmat)
    bmat[3:m,]=bmata
    
  }else{
    uvec=runif(m)
    perpvec=uvec-t(bmata)%*%solve(bmata%*%t(bmata))%*%bmata%*%uvec
    bmat[1,]=perpvec
    bmat[2:m,]=bmata
  }
  edges=t(solve(bmat))
  ysend=t(uinv)%*%delta%*%y
  np=1;if(type==3|type==4){np=2}
  coef=coneB(ysend,edges[(np+1):m,],matrix(t(edges[1:np,]),ncol=np))
  beta=uinv%*%t(edges)%*%coef$coefs
  yhat=t(delta)%*%beta
  qinv=solve(qmat)
  pmat=t(delta)%*%qinv%*%delta
  ytil=pmat%*%y
  edfu=sum(diag(pmat))
  cv=sum((y-ytil)^2)/(1-edfu/n)^2
  # Compute cv for constrained fit
  index=coef$coefs>sm
  index[1]=TRUE
  gmat=edges[index,]
  if(length(gmat)/m==1){gmat=matrix(gmat,ncol=m)}
  pcmat=t(delta)%*%uinv%*%t(gmat)%*%solve(gmat%*%t(gmat))%*%gmat%*%t(uinv)%*%delta
  edfc=sum(diag(pcmat))
  cvc=sum((y-yhat)^2)/(1-edfc/n)^2
  #cvc
  #ans=new.env()
  
  k
  
  ans$cfit=yhat #constrained fit
  ans$ucfit=ytil # unconstrained fit
  ans$ucgcv=cv # unconstrained cross validation
  ans$cgcv=cvc # constrained cross validation
  ans$edfu=edfu # unconstrained edf
  ans$edfc=edfc # constrained edf
  ans$xpl=xpl # x values
  ans$beta=beta
  ans$cpl=bpl%*%beta # constrained y values 
  ans$ucpl=bpl%*%qinv%*%delta%*%y # unconstrained y values
  ans$knots=knots # knots
  return(ans)
}
