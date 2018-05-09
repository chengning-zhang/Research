##############################################NB w(x)
w0=numeric(755)
for(i in 201:955){
  cache=prediction.g(P450.simulate[i,],P450.simulate)
  w0[i-200]=(1-c.hat.NB)/c.hat.NB*cache*
    (1-cache)
}
w=c(rep(1,200),w0)
####w is 0-1 , is it just coincidence?
#########################################treat obs in U as weighted w(x)=P(y=1|x,s=0) to be 1,1-w(x)
 
prediction.w<-function(instance,data,output=FALSE){ #data is training data, instance is dataframe one obs
  classvalues <-
    sort(unique(data[, ncol(data)]),decreasing = T)
  ###have to make sure first classvalues is "1" Then  p1 is p(y=1|x)
  nl=sum(data[,ncol(data)]==classvalues[1])
  nu=sum(data[,ncol(data)]==classvalues[2])
  p1<- (nl+max(w0)*nu+1)/(nl+nu+2) #try to use p(y) to adjust w(x) later and mean(w) later
  p2<-nu*(1-max(w0)+1)/(nl+nu+2)
  #####1-8 columns don't have the factor (0,1)(1,2) problem. 
  for(l in 1:(ncol(data)-1) ){
    featurevalue <- unique(data[, l])
    k <- length(unique(data[, l]))
    p1<-p1*(1+(sum(data[,l]==as.character(instance[l])& data[,ncol(data)]==classvalues[1])+
      sum(w*as.numeric(data[,l]==as.character(instance[l])& data[,ncol(data)]==classvalues[2]))))/
      (nl+max(w0)*nu+k)    
    
    p2<-p2*(1+sum(as.numeric(data[,l]==as.character(instance[l])&data[,ncol(data)]==classvalues[2])*(1-w)))/
      (nu*(1-max(w0))+k)

  }
  p<-p1+p2;p1<-p1/p;p2<-p2/p
  if(output==T)  cat("p(y=1|x)=",p1,"\n")
  return(p1)
}
#######accuracy of NB w(x)
p.y.1_NB=apply(P450.simulate,1,prediction.w,P450.simulate,F)
sum((p.y.1_NB[201:955]>0.5)==c(rep(1,424),rep(0,331)))/755 ##0.471523178808 without laplace smoothing
#better than idea1 though,0.438410596026 with laplace smoothing, frustrating!!!
#plot(apply(P450.simulate,1,prediction.w,P450.simulate,F))

###if I use true p1=624/955 p2 , the accuracy is 0.545695364238
####so next step: max(w0) and laplace smoothing


