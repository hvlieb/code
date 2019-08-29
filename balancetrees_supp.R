library("igraph")

#combination of two split size sequence lists
comba<-function(a,b)
{#print("/");print(a);print(b)#;print(c)
  n<-dim(a)
 m<-dim(b)
#print(matrix(rep(t(a),each=m[1]*p[1]),q,n[2],byrow=TRUE))
#print(matrix(rep(t(b),each=p[1]),q,m[2],byrow=TRUE))
rr<-cbind(a[rep(1:(n[1]),each=m[1]),,drop=FALSE],b[rep(1:(m[1]),n[1]),,drop=FALSE])
#print(rr)
return(rr)
}
 
#split sizes for subtrees
splitpattern.tree<-function(k)
{if (k>=2) de<-do.call(rbind,lapply(1:(k%/%2),function(u)comba(matrix(k,1,1),comba(splitpattern.tree(k-u),splitpattern.tree(u)))))
 return(switch(min(k,2),matrix(1,1,1),de)) }

#list of subtree patterns
splitpattern.trees<-function(k)
{li<-list(matrix(1,1,1))
# print(li)
if (k==1) return(li) else
 {for (j in 2:k)
  li[[j]]<-do.call(rbind,lapply(1:(j%/%2),function(u) comba(matrix(j,1,1),comba(li[[j-u]],li[[u]]))))
  return(li)
 }}

#all split size combinations at central node 
splitpattern<-function(n)
{
  dd<-comba(matrix(1:(n%/%2),nc=1),comba(matrix(1:(n%/%2),nc=1),matrix(1:(n%/%2),nc=1)))
  #print(dd)
  ee<-which((dd[,1]>=dd[,2])&(dd[,2]>=dd[,3])&((dd[,1]+dd[,2]+dd[,3])==n))
  #print(ee)
  ff<-dd[ee,,drop=FALSE]
  #print(ff)
  uu<-lapply(1:(dim(ff)[1]),function(i)comba(splitpattern.tree(ff[i,1]),comba(splitpattern.tree(ff[i,2]),splitpattern.tree(ff[i,3]))))
  #print(uu)
  uu<-do.call(rbind,uu)
  #print(uu)
  uu<-unique(t(apply(uu,FUN=sort.int,M=1,dec=TRUE)))
  #print(uu)
  uu<-uu[,1:(n-3)]
  #print(uu)
  uu1<-lapply(1:(dim(uu)[2]),FUN=function(i)uu[,i])
  #print(uu1)
  ww<-do.call(order,uu1)
  #print(ww)
  return(uu[ww,])
}  
#all patterns
splitpatterns<-function(n)
{li<-splitpattern.trees(n)
  dd<-comba(matrix(1:(n%/%2),nc=1),comba(matrix(1:(n%/%2),nc=1),matrix(1:(n%/%2),nc=1)))
  #print(dd)
  ee<-which((dd[,1]>=dd[,2])&(dd[,2]>=dd[,3])&((dd[,1]+dd[,2]+dd[,3])==n))
  #print(ee)
  ff<-dd[ee,,drop=FALSE]
  #print(ff)
  uu<-lapply(1:(dim(ff)[1]),function(i)comba(li[[ff[i,1]]],comba(li[[ff[i,2]]],li[[ff[i,3]]])))
  #print(uu)
  uu<-do.call(rbind,uu)
  #print(uu)
  uu<-unique(t(apply(uu,FUN=sort.int,M=1,dec=TRUE)))
  #print(uu)
  uu<-uu[,1:(n-3)]
  #print(uu)
  uu1<-lapply(1:(dim(uu)[2]),FUN=function(i)uu[,i])
  #print(uu1)
  ww<-do.call(order,uu1)
  #print(ww)
  return(uu[ww,])
}  


#Pareto minima
minsn<-function(n)
{a0<-splitpatterns(n)
b0<-sapply(1:(dim(a0)[1]),function(i) apply(a0,FUN=function(v) all(v>=a0[i,]),M=1))
c0<-apply(b0,sum,M=1)
a0[which(c0==1),]}  
#examples
for (i in 6:15)
{  print(a<-minsn(i));print(dim(a)[1])}


edt<-sapply(8:22,FUN = function(n) dim(minsn(n))[1])
edta<-sapply(8:23,FUN = function(n) dim(minsn(n))[1])
edtb<-sapply(8:24,FUN = function(n) dim(minsn(n))[1])

#output graphics
postscript("NoSn.eps")
plot(4:22,c(rep(1,4),edt),xlab=expression(n),ylab=expression(abs(S[n])),main="size of set of Pareto minima")
dev.off()
