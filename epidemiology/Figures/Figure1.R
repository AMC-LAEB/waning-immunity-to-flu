setwd("epidemiology")
library(colorspace)
library(plotrix)


plot_size_size <- function(){
  
  ili = read.csv("Data/ili_data.csv")
  ili = ili[ili$Season>2010 ,]
  cols = c("#fabed4","#ffd8b1","#FFD700","#aaffc3","#dcbeff","#808000","#469990","#4363d8","#e6194B","#f58231")
  par(mgp=c(2,0.5,0))
  plot(ili$B_REL_LAG1,ili$B_REL,bg=cols[ili$Season-2009],pch=21,cex=1.2,axes=F,ylim=c(0,7),xlim=c(0,2),xlab = "",ylab = "",lwd=0.1)
  axis(1,at = c(0,0.5,1,1.5,2),tck=-0.01)
  axis(1,at = c(0,0.5,1,1.5,2),tck=-0.01,pos=2.3,lab=NA)
  axis(1,at = c(0,0.5,1,1.5,2),tck=-0.01,pos=4.8,lab=NA)
  
  # axis(1,at = c(0,0.5,1,1.5)+2.5,label = c(0,0.5,1,1.5),tck=-0.01,title="3")
  mtext('Previous season\nrelative size', side=1, line=3, at=1,cex=5/5)
  mtext('Relative size', side=2, line=1.7, at=3.5,cex=5/5)
  points(ili$H1pdm_REL_LAG1,ili$H1pdm_REL+2.5,bg=cols[ili$Season-2009],pch=21,axes=F,cex=1.2,ylab = "Previous season size",xlab = "Season size",lwd=0.1)
  points(ili$H3_REL_LAG1,ili$H3_REL+5,bg=cols[ili$Season-2009],pch=21,axes=F,xlab = "Previous season size",ylab = "Season size",cex=1.2,lwd=0.1)
  
  axis(2,at = c(0,0.5,1,1.5,2),lab = c(0,0.5,1,1.5,2),tck=-0.01,las=1)
  axis(2,at = c(0,0.5,1,1.5,2)+2.5,lab = c(0,0.5,1,1.5,2),tck=-0.01,las=1)
  axis(2,at = c(0,0.5,1,1.5,2)+5,lab = c(0,0.5,1,1.5,2),tck=-0.01,las=1)
  
  axis(2,at = c(-0.3,2),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  axis(1,at = c(-100,2),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  # 
  axis(2,at = c(2.3,4.3),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  axis(1,at = c(-100,2),tck=-0.01,las=1,lwd.ticks=0,lab=NA,pos=2.3)
  # 
  axis(2,at = c(4.8,5.5),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  axis(1,at = c(-100,1.5),tck=-0.01,las=1,lwd.ticks=0,lab=NA,pos=4.8)
  
  
  text(x = 1,y = 1.8,lab="B")
  text(x = 1,y = 2.5+1.8,lab="A/H1N1pdm09")
  text(x = 1,y = 5+1.8,lab="A/H3N2")
  
}

plot_size_size_2 <- function(){
  
  ili = read.csv("Data/ili_data.csv")
  ili = ili[ili$Season>2010,]
  cols = c("#fabed4","#ffd8b1","#FFD700","#aaffc3","#dcbeff","#808000","#469990","#4363d8","#e6194B","#f58231")
  par(mgp=c(2,0.5,0))
  plot(ili$B_REL_LAG2,ili$B_REL,bg=cols[ili$Season-2009],pch=21,cex=1.2,axes=F,ylim=c(0,7),xlim=c(0,2),xlab = "",ylab = "",lwd=0.1)
  axis(1,at = c(0,0.5,1,1.5,2),tck=-0.01)
  axis(1,at = c(0,0.5,1,1.5,2),tck=-0.01,pos=2.3,lab=NA)
  axis(1,at = c(0,0.5,1,1.5,2),tck=-0.01,pos=4.8,lab=NA)
  mtext('Previous two seasons\nrelative size sum', side=1, line=3, at=1,cex=5/5)
  mtext('Relative size', side=2, line=1.7, at=3.5,cex=5/5)
  points(ili$H1pdm_REL_LAG2,ili$H1pdm_REL+2.5,bg=cols[ili$Season-2009],pch=21,axes=F,cex=1.2,ylab = "Previous season size",xlab = "Season size",lwd=0.1)
  points(ili$H3_REL_LAG2,ili$H3_REL+5,bg=cols[ili$Season-2009],pch=21,axes=F,xlab = "Previous season size",ylab = "Season size",cex=1.2,lwd=0.1)
  
  # axis(2,at = c(0,0.5,1,1.5),lab = c(0,0.5,1,1.5),tck=-0.01,las=1)
  # axis(2,at = c(0,0.5,1,1.5)+2,lab = c(0,0.5,1,1.5),tck=-0.01,las=1)
  # axis(2,at = c(0,0.5,1,1.5)+4,lab = c(0,0.5,1,1.5),tck=-0.01,las=1)
  
  axis(2,at = c(0,0.5,1,1.5,2),lab = c(0,0.5,1,1.5,2),tck=-0.01,las=1)
  axis(2,at = c(0,0.5,1,1.5,2)+2.5,lab = c(0,0.5,1,1.5,2),tck=-0.01,las=1)
  axis(2,at = c(0,0.5,1,1.5,2)+5,lab = c(0,0.5,1,1.5,2),tck=-0.01,las=1)
  
  axis(2,at = c(-0.3,2),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  axis(1,at = c(-100,2),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  # 
  axis(2,at = c(2.3,4.3),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  axis(1,at = c(-100,2),tck=-0.01,las=1,lwd.ticks=0,lab=NA,pos=2.3)
  # 
  axis(2,at = c(4.8,5.5),tck=-0.01,las=1,lwd.ticks=0,lab=NA)
  axis(1,at = c(-100,1.5),tck=-0.01,las=1,lwd.ticks=0,lab=NA,pos=4.8)
  
  
  text(x = 1,y = 1.8,lab="B")
  text(x = 1,y = 2.5+1.8,lab="A/H1N1pdm09")
  text(x = 1,y = 5+1.8,lab="A/H3N2")
}

plot_y_since_prev <- function(){

  h3 = c()
  h1 = c()
  b = c()
  h1pdm = c()
  h3ci = c()
  h1ci = c()
  bci = c()
  h1pdmci = c()
  
  byyear = read.csv("Data/viro_data.csv")

  for (year in seq(1,3)){
    test_h3 = binom.test(nrow(byyear[byyear$IS_H3_DOM==TRUE & byyear$Y_SINCE_LAST_H3 == year & !(is.na(byyear$Y_SINCE_LAST_H3)),]),nrow(byyear[byyear$Y_SINCE_LAST_H3 == year & !(is.na(byyear$Y_SINCE_LAST_H3)),]))
    h3 = c(h3,test_h3$estimate)
    h3ci = c(h3ci,test_h3$conf.int)
    test_h1pdm = binom.test(nrow(byyear[byyear$IS_H1pdm_DOM==TRUE & byyear$Y_SINCE_LAST_H1pdm == year & !(is.na(byyear$Y_SINCE_LAST_H1pdm)),]),nrow(byyear[byyear$Y_SINCE_LAST_H1pdm == year & !(is.na(byyear$Y_SINCE_LAST_H1pdm)),]))
    h1pdm = c(h1pdm,test_h1pdm$estimate)
    h1pdmci = c(h1pdmci,test_h1pdm$conf.int)
    test_b = binom.test(nrow(byyear[byyear$IS_B_DOM==TRUE & byyear$Y_SINCE_LAST_B == year & !(is.na(byyear$Y_SINCE_LAST_B)),]),nrow(byyear[byyear$Y_SINCE_LAST_B == year & !(is.na(byyear$Y_SINCE_LAST_B)),]))
    b = c(b,test_b$estimate)
    bci = c(bci,test_b$conf.int)
    
  }
  
  plot(seq(1,3)-0.06,h3,type="b",col="#44AA99",ylim=c(0,1),pch=19,ylab = "",xlab = "",axes=F,xlim=c(0.8,3.2))
  mtext('Seasons since substantial circulation', side=1, line=2.5, at=2,cex=5/5)
  mtext('Probability of substantial circulation', side=2, line=2, at=0.5,cex=5/5)
  
  box(bty="l")
  axis(1,at = 1:3,lab=0:2)
  axis(2)
  arrows(x0=seq(1,3)-0.06, y0=h3ci[seq(1,8,2)], x1=seq(1,3)-0.06, y1=h3ci[seq(2,8,2)], code=3, angle=90, length=0.03, col="#44AA99", lwd=1)
  lines(seq(1,3)-0.02,h1pdm,type="b",col="#DDCC77",pch=19)
  arrows(x0=seq(1,3)-0.02, y0=h1pdmci[seq(1,8,2)], x1=seq(1,3)-0.02, y1=h1pdmci[seq(2,8,2)], code=3, angle=90, length=0.03, col="#DDCC77", lwd=1)
  lines(seq(1,3)+0.02,b,type="b",col="#AA4499",pch=19)
  arrows(x0=seq(1,3)+0.02, y0=bci[seq(1,8,2)], x1=seq(1,3)+0.02, y1=bci[seq(2,8,2)], code=3, angle=90, length=0.03, col="#AA4499", lwd=1)
  legend('topleft', horiz=F, bty="n",legend = c("A/H3N2","A/H1N1pdm09","B"),pch=19,col = c('#44AA99',"#DDCC77","#AA4499"),pt.cex=1,cex=1)

  #out = data.frame()
}

plot_size_by_lag <- function(){
  
  epi = read.csv("Data/viro_data.csv")
  ili = read.csv("Data/ili_data.csv")
  
  dat = merge(epi,ili,by=c("Season","Country"))
  par(mgp=c(1.7,0.5,0))
  cols = c("#fabed4","#ffd8b1","#FFD700","#aaffc3","#dcbeff","#808000","#469990","#4363d8","#e6194B","#f58231")
  
  plot(as.numeric(dat[dat$Y_SINCE_LAST_H3<4,]$Y_SINCE_LAST_H3)+2+rnorm(nrow(dat[dat$Y_SINCE_LAST_H3<4,]),0,0.10),axes=F,xlim=c(2.5,13.5),ylim=c(0,1.75),dat[dat$Y_SINCE_LAST_H3<4,]$H3_REL,pch=21,lwd=0.1,bg=cols[dat[dat$Y_SINCE_LAST_H3<4,]$Season-2009],xlab = "", ylab = "",cex=1.2)
  points(as.numeric(dat[dat$Y_SINCE_LAST_H1pdm<4,]$Y_SINCE_LAST_H1pdm)+6+rnorm(nrow(dat[dat$Y_SINCE_LAST_H1pdm<4,]),0,0.10),dat[dat$Y_SINCE_LAST_H1pdm<4,]$H1pdm_REL,pch=21,lwd=0.1,bg=cols[dat[dat$Y_SINCE_LAST_H1pdm<4,]$Season-2009],xlab = "Seasons since previous dominance", ylab = "Relative size",cex=1.2)
  
  points(as.numeric(dat[dat$Y_SINCE_LAST_B<4,]$Y_SINCE_LAST_B)+10+rnorm(nrow(dat[dat$Y_SINCE_LAST_B<4,]),0,0.10),dat[dat$Y_SINCE_LAST_B<4,]$B_REL,pch=21,bg=cols[dat[dat$Y_SINCE_LAST_B<4,]$Season-2009],lwd=0.1,xlab = "Seasons since previous dominance", ylab = "Relative size",cex=1.2,alpha=0.5)
  axis(2,tck=-0.01,las=1)
  axis(side=2,at = c(1,1.7),lwd.ticks=0,lab = c("",""))
  axis(side=1,at = c(3,4,5), lab = c(1,2,3)-1,tck=-0.01)
  axis(side=1,at = c(7,8,9), lab = c(1,2,3)-1,tck=-0.01)
  axis(side=1,at = c(11,12,13), lab = c(1,2,3)-1,tck=-0.01)
  
  text(x=c(4,8,12),y=0.14*10,labels = c("A/H3N2","A/H1N1pdm09","B")) 
  
  mtext('Seasons since substantial circulation', side=1, line=2, at=8,cex=5/5)
  mtext('Relative size', side=2, line=1.7, at=.8,cex=5/5)
  
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_H3==1,]$H3_REL,xlim=c(2.5,13.5),axes=F,outline = F,at = 3,col= NA,ylim=c(0,1.75),lwd=0.7,width=2)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_H3==2,]$H3_REL,xlim=c(2.5,13.5),axes=F,alpha=0.3,outline = F,at = 4,col=NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_H3==3,]$H3_REL,xlim=c(2.5,13.5),axes=F,alpha=0.3,outline = F,at = 5,col=NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_H1pdm==1,]$H1pdm_REL,xlim=c(2.5,13.5),axes=F,outline = F,at = 7,col= NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_H1pdm==2,]$H1pdm_REL,xlim=c(2.5,13.5),axes=F,alpha=0.3,outline = F,at = 8,col=NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_H1pdm==3,]$H1pdm_REL,xlim=c(2.5,13.5),axes=F,alpha=0.3,outline = F,at = 9,col=NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_B==1,]$B_REL,xlim=c(2.5,13.5),axes=F,outline = F,at = 11,col= NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_B==2,]$B_REL,xlim=c(2.5,13.5),axes=F,alpha=0.3,outline = F,at = 12,col=NA,ylim=c(0,1.75),width=2,lwd=0.7)
  par(new=T)
  boxplot(dat[dat$Y_SINCE_LAST_B==3,]$B_REL,xlim=c(2.5,13.5),axes=F,alpha=0.3,outline = F,at = 13,col=NA,ylim=c(0,1.75),width=2,lwd=0.7)
  
  legend(4,1.85,horiz=F,legend = c("2010/2011","2011/2012","2012/2013","2013/2014","2014/2015","2015/2016","2016/2017","2017/2018","2018/2019","2019/2020"),col=cols,bty="n",pch=19,pt.cex=1,cex=1,ncol=5)
  
}

get_size_hist <- function(){
  
  par(mgp=c(3,0.5,0))
  epi = read.csv("Data/viro_data.csv")
  epi = epi[epi$Season != "2009",] #col='#44AA99'
  a <- hist(as.integer(epi[epi$IS_H3_DOM==TRUE & epi$Y_SINCE_LAST_H3 < 4,]$Y_SINCE_LAST_H3),breaks=seq(0.5,3.5,1),na.rm=T,axes=F,xlim=c(.5,11.5),col='#44AA99',lty=1,ylim=c(0,300),ylab = "",xlab = "",cex.axis=1.5,main="")
  
  mtext('Lull duration', side=1, line=2, at=6,cex=5/5)
  mtext('Count', side=2, line=2.5, at=125,cex=5/5)
  
  axis(1,at = 1:3 ,lab = c(1,2,3)-1,tck=-0.01)
  axis(1,at = 4:6 + 1 ,lab =c(1,2,3)-1,tck=-0.01)
  axis(1,at = 7:9 + 2 ,lab = c(1,2,3)-1,tck=-0.01)
  
  par(new=T)
  hist(as.numeric(epi[epi$IS_H1pdm_DOM==TRUE & epi$Y_SINCE_LAST_H1pdm < 4,]$Y_SINCE_LAST_H1pdm)+4,breaks=seq(4.5,7.5,1),na.rm=T,axes=F,xlim=c(.5,11.5),col='#DDCC77',lty=1,ylim=c(0,300),ylab = "",xlab = "",cex.axis=1.5,main="")
  par(new=T)
  hist(as.numeric(epi[epi$IS_B_DOM==TRUE & as.numeric(epi$Y_SINCE_LAST_B) < 4,]$Y_SINCE_LAST_B)+8,breaks=seq(8.5,11.5,1),na.rm=T,axes=F,xlim=c(.5,11.5),col="#AA4499",lty=1,ylim=c(0,300),ylab = "",xlab = "",cex.axis=1.5,main="")
  axis(2,tck=-0.03,las=1)
  
  text(x=2, y = 275,lab= "A/H3N2",cex=1)
  text(x=6, y = 275,lab= "A/H1N1\npdm09",cex=1)
  
  text(x=10, y = 275,lab= "B",cex=1)
  par(mar = c(4, 4, 2, 0))
  epi = read.csv("Data/ili_data.csv")
  epi = epi[epi$Season != "2009",]
  
  hist(epi$H3_REL/3,breaks=seq(0,2/3,0.2/3),na.rm=T,axes=F,xlim=c(0,2.4),lty=1,col='#44AA99',ylim=c(0,120),ylab = "",xlab = "",main="")
  
  mtext('Relative size', side=1, line=1.7, at=2/3/2+2/3+0.2,cex=5/5)
  mtext('Count', side=2, line=2.5, at=60,cex=5/5)
  
  axis(1,at = c(0,1,2)/3,lab = c(0,1,2),tck=-0.01)
  axis(1,at = c(0,1,2)/3 + 2/3 + 0.2,lab = c(0,1,2),tck=-0.01)
  axis(1,at = c(0,1,2)/3 + 4/3 + 0.4,lab = c(0,1,2),tck=-0.01)
  
  par(new=T)
  hist(epi[epi$Season>2009,]$H1pdm_REL/3+2/3+0.2,breaks=seq(2/3+0.2,4/3+0.2,0.2/3),lty=1,na.rm=T,axes=F,ylim=c(0,120),xlim=c(0,2.4),col="#DDCC77",ylab = "",xlab = "",main="")
  par(new=T)
  hist(epi$B_REL/3+4/3+0.4,breaks=seq(4/3+0.4,6/3+0.4,0.2/3),na.rm=T,ylim=c(0,120),lty=1,axes=F,xlim=c(0,2.4),col="#AA4499",ylab = "",xlab = "",main="")
  
  axis(2,tck=-0.01,las=1)
  text(x=2/3/2,y = 110,lab= "A/H3N2",cex=1)
  text(x=2/3/2+2/3+0.2,y = 110,lab= "A/H1N1\npdm09",cex=1)
  text(x=2/3/2+4/3+0.4,y = 110,lab= "B",cex=1)
  
}


setWidth = 183*0.039370 
setFontSize = 5
pdf(file='Figures/Figure_1.pdf',width=setWidth,height=4.5,pointsize=setFontSize)

layout(matrix(c(1, 1, 3, 3,
                1, 1, 3, 3,
                2, 2, 3, 3,
                2, 5, 5, 5,
                4, 4, 6, 7,
                4, 4, 6, 7
)
, 6, 4, byrow = TRUE),heights = c(0.05,0.15,0.2,0.08,0.25,0.2),widths = c(0.13,0.13,0.25,0.25))

par(cex=1)
par(mar = c(2, 4, 1, 0))
get_size_hist()

par(mar = c(3, 4, 1, 1)) 
plot_size_by_lag()

par(mar = c(5, 4, 0, 0))
plot_y_since_prev()

par(mar=c(0,13,1.5,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
cols = c("#fabed4","#ffd8b1","#FFD700","#aaffc3","#dcbeff","#808000","#469990","#4363d8","#e6194B","#f58231")
cols = c("#ffd8b1","#FFD700","#aaffc3","#dcbeff","#808000","#469990","#4363d8","#e6194B","#f58231")

legend('top',horiz=F,legend = c("2011/2012","2012/2013","2013/2014","2014/2015","2015/2016","2016/2017","2017/2018","2018/2019","2019/2020"),col=cols,bty="n",pch=19,pt.cex=1,cex=1,ncol=5)

par(mar = c(5, 4, 0, 0))
plot_size_size()

plot_size_size_2()

dev.off()

