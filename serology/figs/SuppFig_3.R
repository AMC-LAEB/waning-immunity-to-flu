setwd("serology")


library(ggplot2)

thm1 =     theme(axis.text = element_text(color="black",size=6),
                 panel.spacing.x = unit(1, "mm"),
                 axis.line=element_line(color='black',size = 0.2),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 legend.position='none',
                 axis.title= element_text(size=6),
                 strip.background=element_rect(colour="black",fill="white"),
                 strip.text.x = element_text(size = 8),
                 legend.text=element_text(size=6),
                 legend.title =element_text(size=6),
                 legend.key=element_rect(fill="white"),
                 legend.key.width = unit(.5, "line"),
                 legend.spacing.y = unit(.001, 'cm'),
                 legend.box.spacing = unit(0, "pt"),
                 legend.margin=margin(1,1,1,1))


mtdt_acs = read.csv("dat/mtdt_acs.csv",sep=";")
mtdt_acs = mtdt_acs[!(duplicated(mtdt_acs$subject)),]
mtdt_acs = mtdt_acs[,c("age","born")]
mtdt_acs$Sex = "Male"
mtdt_acs$Cohort = c("ACS")
mtdt_acs =mtdt_acs[,c("age","Sex","Cohort")]
colnames(mtdt_acs)[1] = "Age"

mtdt_recovered = read.csv("dat/mtdt_recovered.csv",sep=";")
mtdt_recovered = mtdt_recovered[!(is.na(mtdt_recovered$age)),]
mtdt_recovered = mtdt_recovered[,c("gender","ageyears")]
mtdt_recovered$Cohort = "RECoVERED"
colnames(mtdt_recovered) = c("Sex","Age","Cohort")
mtdt_recovered = mtdt_recovered[,c(2,1,3)]

mtdt = rbind(mtdt_acs,mtdt_recovered)


fig = ggplot(mtdt,aes(x=Age,fill=Sex)) + geom_histogram(linewidth=0.1,alpha=0.7,col='black') + thm1 + theme(legend.position='top') + ylab("Count") + facet_wrap(.~Cohort)

ggsave("figs/SuppFig_3.pdf", fig, width= 130, height=80, units="mm")
