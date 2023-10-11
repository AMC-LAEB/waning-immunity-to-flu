setwd("epidemiology")


setWidth = 183*0.039370*0.5
setFontSize = 5
pdf(file='Figures/Supp_Fig_1.pdf',width=setWidth,height=4,pointsize=setFontSize)


plot_pred <- function(){
                par(mar=c(5.1,6.1,4.1,2.1))
        c = barplot(c(
                      h3_compare[2,1],h1pdm_compare[2,1],b_compare[2,1],
                      h3_compare_size_size[2,1],h1pdm_compare_size_size[2,1],b_compare_size_size[2,1],
                      h3_compare_size_size_2[2,1],h1pdm_compare_size_size_2[2,1],b_compare_size_size_2[2,1]),space=c(0,0,0,2,0,0,2,0,0),
                    ylim=c(-120,25),col=c("#44AA99","#DDCC77","#AA4499"),ylab = "")
        text(1.5,13,"Size ~\nseasons since\nsubstantial\ncirculation",cex=1)
        text(6.5,13,"Size ~\nprevious\nseason\nsize",cex=1)
        text(11.5,13,"Size ~\n sum two\nprevious\nseasons size",cex=1)
        legend('bottom', horiz=F, bty="n",legend = c("A/H3N2","A/H1N1pdm09","B"),fill = c('#44AA99',"#DDCC77","#AA4499"),pt.cex=1,cex=1)
        
        est = c(
                h3_compare[2,1],h1pdm_compare[2,1],b_compare[2,1],
                h3_compare_size_size[2,1],h1pdm_compare_size_size[2,1],b_compare_size_size[2,1],
                h3_compare_size_size_2[2,1],h1pdm_compare_size_size_2[2,1],b_compare_size_size_2[2,1])
        se = c(
               h3_compare[2,2],h1pdm_compare[2,2],b_compare[2,2],
               h3_compare_size_size[2,2],h1pdm_compare_size_size[2,2],b_compare_size_size[2,2],
               h3_compare_size_size_2[2,2],h1pdm_compare_size_size_2[2,2],b_compare_size_size_2[2,2])
        
        arrows(c,est-se,c ,est+se, ,length=0.05, angle=90, code=3)
        mtext('Difference in expected\nlog pointwise predictive density', side=2, line=2.7, at=-60,cex=5/5)
        
}

plot_pred()
dev.off()



