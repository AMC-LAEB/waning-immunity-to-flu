library(tidyr)
library(ggplot2)
setwd("epidemiology")
epi_sizes = read.csv("Data/ili_data.csv")
epi_sizes=epi_sizes[epi_sizes$Country == "Netherlands",]
epi_sizes = epi_sizes[,c(1,5,6,7)]
epi_sizes = gather(epi_sizes,Subtype,Size,B_REL:H1pdm_REL)

epi_sizes$Season = factor(epi_sizes$Season,levels=2010:2019,labels = c("2010/2011","2011/2012","2012/2013","2013/2014","2014/2015","2015/2016","2016/2017","2017/2018","2018/2019","2019/2020"))
epi_sizes$Subtype = factor(epi_sizes$Subtype,levels=c("H3_REL","H1pdm_REL","B_REL"),labels=c("A/H3N2","A/H1N1pdm09","B"))

fig = ggplot(epi_sizes,aes(fill=Subtype,x=Season,y=Size)) + geom_bar(position="stack", stat="identity") + theme_minimal() + 
  ylab("Relative size") + 
  theme(axis.title = element_text(size=6),
        legend.text = element_text(size=6),
        axis.text=element_text(size=5,color='black'),
        legend.title=element_text(size=6)) + scale_fill_manual(values = c('#44AA99',"#DDCC77","#AA4499"))


ggsave("Figures/Supp_Fig_2.pdf", fig, width=140, height=90, units="mm")
