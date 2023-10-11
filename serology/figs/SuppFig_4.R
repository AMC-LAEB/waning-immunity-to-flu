setwd("serology")

library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(patchwork)
library(ggthemes)
library(ggpubr)

load("dat/ACS_dat.RData")

# random noise
acs_dat$LogTiter<-acs_dat$LogTiter+runif(nrow(acs_dat), 0, 0.2)
acs_dat$date<-acs_dat$date+runif(nrow(acs_dat), 0, 0.1)

# strain ordering
acs_dat$strain<-factor(acs_dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

fig1<-ggplot(acs_dat, aes(x=year, y=LogTiter, color=strain, linetype=as.factor(rep)))+
  geom_line(alpha=0.8, size=0.3)+
  geom_point(alpha=0.8, size=0.7)+
  theme_bw(base_size = 5)+
  theme(legend.position="bottom",
        legend.key.size = unit(8, 'points'),
        legend.title = element_text(size=6),
        axis.title = element_text(size=6),
        
        axis.text=element_text(size=5,color='black'),
        legend.text = element_text(size=6))+
  scale_y_continuous(breaks = 0:8)+
  scale_x_continuous(breaks=2017:2021, limits = c(2016.8, 2021.2),labels=c("'17", "'18","'19","20", "'21"))+
  scale_color_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  labs(x="Year", y=expression(paste(" Titre (", log[2],")")), color="(Sub)type", linetype="Replicate")+
  facet_wrap(vars(pat_id))

################################################################################
################################################################################

load("dat/VIS_dat.RData")

#average over replicates
vis_dat<-vis_dat%>%group_by(pat_id, year, date, strain)%>%summarise(LogTiter=mean(LogTiter, na.rm=T))

#random noise to jitter overlapping y and x
vis_dat$LogTiter<-vis_dat$LogTiter+rnorm(nrow(vis_dat), 0, 0.1)

#rearrange strain order to match paper
vis_dat$strain<-factor(vis_dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

fig2<-ggplot(vis_dat, aes(x=year, y=LogTiter, group=pat_id, color=strain))+
  geom_point(alpha=0.7, size=0.7)+
  geom_line(alpha=0.7, size=0.3)+
  theme_bw(base_size = 7)+
  theme(legend.position="bottom",
        legend.key.size = unit(8, 'points'),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6),
        axis.title = element_text(size=6),
        axis.text=element_text(color='black'),
        
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  scale_y_continuous(breaks = 0:8)+
  scale_x_continuous(breaks=2020:2021, limits = c(2019.8, 2021.2),labels=c("'20", "'21"))+
  scale_color_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  labs(x="Year", y=expression(paste(" Titre (", log[2],")")), color="(Sub)type", linetype="Replicate")+
  facet_grid(cols=vars(strain))

# fig<-ggarrange(fig1+theme(plot.margin = margin(1,1,1,10, "points")),
#                fig2+theme(plot.margin = margin(1,40,1,40, "points")), 
#                ncol=1, heights = c(0.8,0.2), labels = c("a", "b"))

#fig <- ggarrange(fig1,fig2,ncol=1,heights=c(0.8,0.2))#fig1/fig2 + plot_layout(heights=c(0.8,0.2))

fig <- fig2/fig1 + plot_layout(heights=c(0.2,0.8))
ggsave("figs/SuppFig_4.pdf", fig, width= 183, height=230, units="mm")

