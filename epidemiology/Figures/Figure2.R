setwd("epidemiology")
library(plotrix)

# Run models

# source('Models/run_size_model.R')
# source('Models/run_size_size_model.R')
# source('Models/run_size_size_model_2.R')


plot_posteriors_size <- function(){
  par(mar = c(7.1, 4.1, 2.1, 2.1))
 
  plot(0,ylim=c(-.7,1),xlim=c(1,14),cex=0,axes=F,xlab = "",ylab = "",cex.lab=1)
  
  mtext('Effect on relative size', side=2, line=2.7, at=0.15,cex=5/5)
  
  axis(2,cex.axis=1,tck=-0.01)
  
  for (i in seq(5.5,18.5,2)){
    rect(i,-3,i+1,3,col="grey98",border=NA)
  }
  
  for (i in seq(0.5,4.5,4/1.5)){
    rect(i,-3,i+4/3,3,col="grey98",border=NA)
    
  }
  axis(1,lwd=1,at = seq(5,14),padj=.7,las=1,tck=-0.01,labels = c("2010/2011","2011/2012","2012/2013","2013/2014","2014/2015","2015/2016","2016/2017","2017/2018","2018/2019","2019/2020"),cex.axis=1)
  box(bty="l")
  axis(1,lwd=1,padj=c(rep(.8,3),rep(.3,10)),at = c(5/4,5/4*2,5/4*3),tck=-0.01,cex.axis=1,labels = c("Seasons since\nsubstantial\ncirculation\n(per season)","Previous\nseason size\n(per unit\nsize)","Sum two\nprevious\nseasons size\n(per unit\nsize)"))

  eps = 0.3/8
  lwd_1 =1.5
  lwd_2 = 0.5
  idx = 7
  
  cex.cube = 1*1.5
  cex.diamond = 1.4*1.5
  cex.circle = 0.9*1.5
  
  for (i in c(seq(1,8))){
    pts = quantile(as.matrix(size_size_model_2_b)[,i],c(2.5,25,50,75,97.5)/100)
    points(idx+0.4,pts[3],pch=0,col="#AA4499",lend=1,cex=cex.cube,lwd=0.8)
    segments(idx+0.4,pts[1],idx+0.4,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
    segments(idx+0.4,pts[2],idx+0.4,pts[4],idx+0.4,lwd=lwd_1,col="#AA4499",lend=1,lty=1)
    
    pts = quantile(as.matrix(size_size_model_2_h1pdm)[,i],c(2.5,25,50,75,97.5)/100)
    points(idx+0.1,pts[3],pch=0,col="#DDCC77",lend=1,cex=cex.cube,lwd=0.8)
    segments(idx+0.1,pts[1],idx+0.1,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
    segments(idx+0.1,pts[2],idx+0.1,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
    
    pts = quantile(as.matrix(size_size_model_2_h3)[,i],c(2.5,25,50,75,97.5)/100)
    points(idx-0.2,pts[3],pch=0,col="#44AA99",lend=1,cex=cex.cube,lwd=0.8)
    segments(idx-0.2,pts[1],idx-0.2,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
    segments(idx-0.2,pts[2],idx-0.2,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
    
    idx = idx + 1
    
  }
  
  
  idx = 6
  for (i in c(seq(1,9))){
    pts = quantile(as.matrix(size_size_model_b)[,i],c(2.5,25,50,75,97.5)/100)
    segments(idx+0.3,pts[1],idx+0.3,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
    segments(idx+0.3,pts[2],idx+0.3,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
    points(idx+0.3,pts[3],pch=5,col="#AA4499",lend=1,cex=cex.diamond/1.7,lwd=0.8)
    
    pts = quantile(as.matrix(size_size_model_h1pdm)[,i],c(2.5,25,50,75,97.5)/100)
    segments(idx,pts[1],idx,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
    segments(idx,pts[2],idx,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
    points(idx,pts[3],pch=5,col="#DDCC77",lend=1,cex=cex.diamond/1.7,lwd=0.8)
    
    pts = quantile(as.matrix(size_size_model_h3)[,i],c(2.5,25,50,75,97.5)/100)
    segments(idx-0.3,pts[1],idx-0.3,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
    segments(idx-0.3,pts[2],idx-0.3,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
    points(idx-0.3,pts[3],pch=5,col="#44AA99",lend=1,cex=cex.diamond/1.7,lwd=0.8)
    
    idx = idx + 1
  }
  
  idx = 5
  for (i in c(seq(1,10))){
    pts = quantile(as.matrix(size_model_b)[,i],c(2.5,25,50,75,97.5)/100)
    segments(idx+0.2,pts[1],idx+0.2,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
    segments(idx+0.2,pts[2],idx+0.2,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
    points(idx+0.2,pts[3],pch=1,col="#AA4499",lend=1,cex=cex.circle,lwd=0.8)

    pts = quantile(as.matrix(size_model_h1pdm)[,i],c(2.5,25,50,75,97.5)/100)
    segments(idx-0.1,pts[1],idx-0.1,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
    segments(idx-0.1,pts[2],idx-0.1,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
    points(idx-0.1,pts[3],pch=1,col="#DDCC77",lend=1,cex=cex.circle,lwd=0.8)
    
    pts = quantile(as.matrix(size_model_h3)[,i],c(2.5,25,50,75,97.5)/100)
    segments(idx-0.4,pts[1],idx-0.4,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
    segments(idx-0.4,pts[2],idx-0.4,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
    points(idx-0.4,pts[3],pch=1,col="#44AA99",lend=1,cex=cex.circle,lwd=0.8)
    
    idx = idx + 1
    
  }
  
  eps = 0.5/5
  
  idx = 5/4
  pts = quantile(as.matrix(size_model_b_noseason)[,2],c(2.5,25,50,75,97.5)/100)
  segments(idx+0.4,pts[1],idx+0.4,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
  segments(idx+0.4,pts[2],idx+0.4,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
  points(idx+0.4,pts[3],pch=19,col="#AA4499",lend=1,cex=cex.circle,lwd=0.8)
  
  pts = quantile(as.matrix(size_model_h1pdm_noseason)[,2],c(2.5,25,50,75,97.5)/100)
  segments(idx+0.08,pts[1],idx+0.08,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
  segments(idx+0.08,pts[2],idx+0.08,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
  points(idx+0.08,pts[3],pch=19,col="#DDCC77",lend=1,cex=cex.circle,lwd=0.8)
  
  pts = quantile(as.matrix(size_model_h3_noseason)[,2],c(2.5,25,50,75,97.5)/100)
  segments(idx-0.24,pts[1],idx-0.24,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
  segments(idx-0.24,pts[2],idx-0.24,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
  points(idx-0.24,pts[3],pch=19,col="#44AA99",lend=1,cex=cex.circle,lwd=0.8)
  
  pts = quantile(as.matrix(size_model_b)[,11],c(2.5,25,50,75,97.5)/100)
  segments(idx+0.24,pts[1],idx+0.24,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
  segments(idx+0.24,pts[2],idx+0.24,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
  points(idx+0.24,pts[3],pch=1,col="#AA4499",lend=1,cex=cex.circle,lwd=0.8)
  
  pts = quantile(as.matrix(size_model_h1pdm)[,11],c(2.5,25,50,75,97.5)/100)
  segments(idx-0.08,pts[1],idx-0.08,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
  segments(idx-0.08,pts[2],idx-0.08,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
  points(idx-0.08,pts[3],pch=1,col="#DDCC77",lend=1,cex=cex.circle,lwd=0.8)
  
  pts = quantile(as.matrix(size_model_h3)[,11],c(2.5,25,50,75,97.5)/100)
  segments(idx-0.4,pts[1],idx-0.4,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
  segments(idx-0.4,pts[2],idx-0.4,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
  points(idx-0.4,pts[3],pch=1,col="#44AA99",lend=1,cex=cex.circle,lwd=0.8)
  
  
  
  
  idx = 5/4*2
  meansize_h3 = mean(as.matrix(size_size_model_h3)[,13])
  meansize_h1pdm = mean(as.matrix(size_size_model_h1pdm)[,13])
  meansize_b = mean(as.matrix(size_size_model_b)[,13])
  
  
  pts = quantile(as.matrix(size_size_model_b_noseason)[,2],c(2.5,25,50,75,97.5)/100)#*meansize_h3
  segments(idx+0.4,pts[1],idx+0.4,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
  segments(idx+0.4,pts[2],idx+0.4,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
  points(idx+0.4,pts[3],pch=18,col="#AA4499",lend=1,cex=cex.diamond)
  
  pts = quantile(as.matrix(size_size_model_h1pdm_noseason)[,2],c(2.5,25,50,75,97.5)/100)#*meansize_h1pdm
  segments(idx+0.08,pts[1],idx+0.08,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
  segments(idx+0.08,pts[2],idx+0.08,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
  points(idx+0.08,pts[3],pch=18,col="#DDCC77",lend=1,cex=cex.diamond)
  
  pts = quantile(as.matrix(size_size_model_h3_noseason)[,2],c(2.5,25,50,75,97.5)/100)#*meansize_b
  segments(idx-0.24,pts[1],idx-0.24,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
  segments(idx-0.24,pts[2],idx-0.24,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
  points(idx-0.24,pts[3],pch=18,col="#44AA99",lend=1,cex=cex.diamond)
  
  pts = quantile(as.matrix(size_size_model_b)[,10],c(2.5,25,50,75,97.5)/100)#*meansize_h3
  segments(idx+0.24,pts[1],idx+0.24,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
  segments(idx+0.24,pts[2],idx+0.24,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
  points(idx+0.24,pts[3],pch=5,col="#AA4499",lend=1,cex=cex.diamond/1.7,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_h1pdm)[,10],c(2.5,25,50,75,97.5)/100)#*meansize_h1pdm
  segments(idx-0.08,pts[1],idx-0.08,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
  segments(idx-0.08,pts[2],idx-0.08,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
  points(idx-0.08,pts[3],pch=5,col="#DDCC77",lend=1,cex=cex.diamond/1.7,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_h3)[,10],c(2.5,25,50,75,97.5)/100)#*meansize_b
  segments(idx-0.4,pts[1],idx-0.4,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
  segments(idx-0.4,pts[2],idx-0.4,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
  points(idx-0.4,pts[3],pch=5,col="#44AA99",lend=1,cex=cex.diamond/1.7,lwd=0.8)
  
  
  meansize_h3 = mean(as.matrix(size_size_model_2_h3)[,12])
  meansize_h1pdm = mean(as.matrix(size_size_model_2_h1pdm)[,12])
  meansize_b = mean(as.matrix(size_size_model_2_b)[,12])
  
  
  idx = 5/4*3
  pts = quantile(as.matrix(size_size_model_2_b_noseason)[,2],c(2.5,25,50,75,97.5)/100)
  segments(idx+0.4,pts[1],idx+0.4,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
  segments(idx+0.4,pts[2],idx+0.4,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
  points(idx+0.4,pts[3],pch=15,col="#AA4499",lend=1,cex=cex.cube,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_2_h1pdm_noseason)[,2],c(2.5,25,50,75,97.5)/100)#*meansize_h1pdm
  segments(idx+0.08,pts[1],idx+0.08,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
  segments(idx+0.08,pts[2],idx+0.08,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
  points(idx+0.08,pts[3],pch=15,col="#DDCC77",lend=1,cex=cex.cube,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_2_h3_noseason)[,2],c(2.5,25,50,75,97.5)/100)#*meansize_b
  segments(idx-0.24,pts[1],idx-0.24,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
  segments(idx-0.24,pts[2],idx-0.24,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
  points(idx-0.24,pts[3],pch=15,col="#44AA99",lend=1,cex=cex.cube,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_2_b)[,9],c(2.5,25,50,75,97.5)/100)#*meansize_h3
  segments(idx+0.24,pts[1],idx+0.24,pts[5],lwd=lwd_2,col="#AA4499",lend=1)
  segments(idx+0.24,pts[2],idx+0.24,pts[4],lwd=lwd_1,col="#AA4499",lend=1)
  points(idx+0.24,pts[3],pch=0,col="#AA4499",lend=1,cex=cex.cube,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_2_h1pdm)[,9],c(2.5,25,50,75,97.5)/100)#*meansize_h1pdm
  segments(idx-0.08,pts[1],idx-0.08,pts[5],lwd=lwd_2,col="#DDCC77",lend=1)
  segments(idx-0.08,pts[2],idx-0.08,pts[4],lwd=lwd_1,col="#DDCC77",lend=1)
  points(idx-0.08,pts[3],pch=0,col="#DDCC77",lend=1,cex=cex.cube,lwd=0.8)
  
  pts = quantile(as.matrix(size_size_model_2_h3)[,9],c(2.5,25,50,75,97.5)/100)#*meansize_b
  segments(idx-0.4,pts[1],idx-0.4,pts[5],lwd=lwd_2,col="#44AA99",lend=1)
  segments(idx-0.4,pts[2],idx-0.4,pts[4],lwd=lwd_1,col="#44AA99",lend=1)
  points(idx-0.4,pts[3],pch=0,col="#44AA99",lend=1,cex=cex.cube,lwd=0.8)
  
  
  abline(v = 4.5,lwd=.1,lty=2)
  
  legend(y = 0.85,x=1.8,pch = c(19,15,18),title="",legend=c("","",""),pt.cex=c(0.8,1,1.2)*1.5,y.intersp=0.7,x.intersp=0,cex=1,horiz=T,bty="n",bg="transparent")
  text(y=0.75,x=2.7,lab="Models\nwithout\nseason\neffects")

  legend(y = 0.85,x=.5,pch = c(1,0,5),title="",legend=c("","", ""),pt.cex=c(0.8,1,.8)*1.5,y.intersp=0.7,x.intersp=0,horiz=T,cex=1,bty="n",bg="transparent")
  text(y=0.75,x=1.5,lab="Models\nwith\nseason\neffects")
  
  legend(y = 1.17,x=3.2,pch = c(19,19,19),title="",legend=c("A/H3N2","A/H1N1\npdm09","B"),col=c("#44AA99","#DDCC77","#AA4499"),pt.cex=c(1,1,1)*1.5,horiz=F,y.intersp=1,x.intersp=0.8,cex=1,bty="n",bg="transparent")

  abline(h=0,lty=2,col="grey",lwd=0.8)
  
  par(xpd=NA)
  
  segments(4.6,-1.1,14.4,-1.1)
  segments(4.6,-1.1,4.6,-1.05)
  segments(14.4,-1.1,14.4,-1.05)
  
  text(y=-1.23,x=9.5,label="Season effects")
  
}

setWidth = 183*0.039370 
setFontSize = 5
pdf(file='Figures/Figure_2.pdf',width=setWidth,height=3,pointsize=setFontSize)

plot_posteriors_size()
dev.off()

source("Figures/Supp_Fig_1.R")