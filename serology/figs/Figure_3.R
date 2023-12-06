setwd("serology")


# Load libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggnewscale)
library(ggpattern)
library(patchwork)
library(ggpubr)

################################################################################
# Functions / plotting styles
################################################################################

# Get data ready for plots
prep_dat<-function(dat)
{
  # Take average over replicates
  dat<-dat%>%group_by(cohort, pat_id, strain, year, date)%>%summarise(LogTiter=mean(LogTiter, na.rm=T),
                                                                      diff=mean(diff, na.rm=T))
  # Find seroconverters
  dat<-dat%>%mutate(sero=ifelse(diff>=2, "yes", "no"))
  # Add column for titers from previous year
  dat<-dat[order(dat$pat_id, dat$strain, dat$year),]
  dat<-dat%>%group_by(pat_id, strain)%>%mutate(prevTiter=lag(LogTiter))
}

# Pandemic onset for (a) and (b)
first_case = (28+31)/365+2020

# Make random jitter reproducible
set.seed(1)

# Style functions
base_breaks_x <- function(x, br, str){
  b <- pretty(x)
  d <- data.frame(strain=str,y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  d$strain<-factor(d$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks=br))
}
base_breaks_y <- function(x, br, str){
  b <- pretty(x)
  d <- data.frame(strain=str, year=2018, x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  d$strain<-factor(d$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=br))
}

base_breaks_x2 <- function(x, br, yr){
  b <- pretty(x)
  d <- data.frame(strain=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"), year=yr, y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  d$strain<-factor(d$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks=br))
}
base_breaks_y2 <- function(x, br, yrs){
  b <- pretty(x)
  d <- data.frame(strain="A/H3N2", year=yrs, x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  d$strain<-factor(d$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=br))
}

################################################################################
# Prepare/load data
################################################################################


#################
# Load titre data
#################

load("dat/ACS_dat.RData")
load("dat/VIS_dat.RData")

#################
# Filter ACS data
#################

# Not necessary for VIS since there are no confirmed vaccine recipients and no
# faulty batches

# Subset so that only individuals 31-100 are plotted
acs_dat<-acs_dat%>%filter(pat_no>=31)
# Omit vaccine recipients
acs_dat<-acs_dat%>%group_by(pat_id)%>%mutate(vaxany=("yes"%in%vax))
acs_dat<-acs_dat%>%filter(vaxany==F)

###########################
# Prepare data for plotting
###########################

acs_dat<-prep_dat(acs_dat)

vis_dat$cohort<-"RECoVERED" # Rename VIS
vis_dat<-prep_dat(vis_dat)

# Bind cohorts
all_dat<-rbind(acs_dat, vis_dat)


###################
# Load MCMC outputs
###################

load("out/acs_yrs_17_21_pats_31_100_5000.RData")
out1<-out%>%filter(grepl("alpha_mn", param))
out1$cohort<-"ACS\n2017-21"


load("out/acs_yrs_20_21_pats_31_100_5000.RData")
out2<-out%>%filter(grepl("alpha_mn", param))
out2$cohort<-"ACS\n2020-21"

load("out/vis_yrs_20_21_5000.RData")
out3<-out%>%filter(grepl("alpha_mn", param))
out3$cohort<-"RECoVERED\n2020-21"

load("out/acs_vis_pats_31_100_5000.RData")
out4<-out%>%filter(grepl("alpha_mn", param))
out4$cohort<-"ACS & RECoVERED\n2020-21"

# Bind MCMC outputs
waning_mcmc_intervals<-rbind(out1, out2, out3, out4)


################################################################################
# Panel (b) GMT
################################################################################

gmt_dat<-all_dat

# # Offset year to reflect sample collection date
# gmt_dat$year<-gmt_dat$year+0.6478333          # Average time since start of year for sample collection


dat_mn<-gmt_dat%>%group_by(cohort, year, strain)%>%summarise(date=mean(date),
                                                             gmt=mean(LogTiter, na.rm=T),
                                                             sd=sd(LogTiter, na.rm=T)/sqrt(n()))

# Add random jitter to points
gmt_dat$LogTiter<-gmt_dat$LogTiter+rnorm(nrow(gmt_dat), 0, 0.2)
# gmt_dat$year<-gmt_dat$year+rnorm(nrow(gmt_dat), 0, 0.1)


# Rearrange strain order
gmt_dat$strain<-factor(gmt_dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
dat_mn$strain<-factor(dat_mn$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))


fig_gmt<-ggplot()+
  geom_rect(data=gmt_dat, xmin=first_case, xmax=Inf, ymin=-Inf, ymax=Inf, fill="grey95")+
  annotate("text", x=2020.95, label="Pandemic", y=8.5, angle=90, size=5/.pt)+
  geom_line(data=gmt_dat, aes(x=date, y=LogTiter, color=strain, group=pat_id, linetype=cohort), alpha=0.3, size=0.1)+
  geom_linerange(data=dat_mn, aes(x=date, ymin=gmt-sd, ymax=gmt+sd, color=strain), size=0.3)+
  geom_line(data=dat_mn, aes(x=date, y=gmt, color=strain, linetype=cohort), size=0.4)+
  coord_cartesian(xlim=c(2017.5,2021.5))+
  theme_bw(base_size = 8)+
  theme(legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x=element_text(colour="black",size=6),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks=element_line(size=0.2),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=5),
        legend.margin=margin(b=-0.28,t = -0.2, unit='cm'),
        axis.title=element_text(size=6),
        panel.spacing = unit(0.15, "lines"))+
  facet_grid(cols=vars(strain))+
  scale_colour_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  labs(x="Year", y=expression(paste(" Titre (", log[2],")")) )+
  base_breaks_y(0:10, 0:10, "A/H3N2")+
  base_breaks_x(gmt_dat$data, 2017:2021, c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria")) + 
  guides(color = 'none') 

################################################################################
# Panel (a) Epi
################################################################################

# Load epi data
load("dat/NL_data.RData")

df$strain<-factor(df$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

fig_epi<-ggplot(df, aes(x=year, y=prev))+
  geom_rect(xmin=first_case, xmax=Inf, ymin=-Inf, ymax=Inf, fill="grey95")+
  geom_line(aes(color=strain),size=0.3)+
  theme_bw(base_size = 8)+
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text=element_text(colour="black",size=6),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks=element_line(size=0.2),
        panel.grid.minor = element_blank(),
        axis.title=element_text(size=6),
        panel.spacing = unit(0.15, "lines"))+
  facet_grid(cols=vars(strain))+
  coord_cartesian(xlim=c(2017.5,2021.5))+
  facet_grid(cols=vars(strain))+
  labs(y="(Sub)type-specific ILI", x="Year", color="Strain")+
  annotate("text", x=2020.95, label="Pandemic", y=100, angle=90, size=5/.pt)+
  scale_colour_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  base_breaks_y(0:120, seq(0,120,20), "A/H3N2")+
  base_breaks_x(gmt_dat$year, 2017:2021, c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

################################################################################
# Panel (c) Seroconversion
################################################################################

dat<-all_dat
seros<-dat%>%filter(sero=="yes")
seros$year<-seros$year-1

labs<-data.frame(strain="A/H3N2", year=2017:2021, lab=c("2017", "2018", "2019", "2020", "2021"))
key<-data.frame(strain="A/H3N2", year=2017, lab="Seroconversion")
key2<-data.frame(strain="A/H3N2", year=2017, lab="next season")

# Rearrange strain order to match paper, have to do with all data frames
# otherwise R rearranges to alphabetical...
dat$strain<-factor(dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
seros$strain<-factor(seros$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
labs$strain<-factor(labs$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
key$strain<-factor(key$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
key2$strain<-factor(key2$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))


all_dat$strain<-factor(all_dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
acs_dat$strain<-factor(acs_dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))
vis_dat$strain<-factor(vis_dat$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

# Set cohort order
all_dat$cohort<-factor(all_dat$cohort, levels=c("RECoVERED", "ACS"))

# Position of seroconversion box
key_y<-55

fig_sero<-ggplot()+
  scale_fill_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  geom_histogram_pattern(data=all_dat, aes(x=LogTiter, fill=strain, pattern=cohort), binwidth=1, color="black", size=0.1,
                         pattern_fill = "red",
                         pattern_size=0.1,
                         pattern_angle = 45,
                         pattern_density = 0.02,
                         pattern_color = 'white',
                         pattern_spacing = 0.03,
                         pattern_key_scale_factor = 0.2) +
  scale_pattern_manual(values = c(ACS = "none",RECoVERED = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white",pattern_color='black')),fill='none') +
  geom_rect(data=key, xmin=-0.5, xmax=0.5, ymin=key_y-3 , ymax=key_y+3, color="black", fill="grey89", size=0.1)+
  geom_text(data=key, aes(label=lab), x=4.5, y=key_y+3, size=5/.pt)+
  geom_text(data=key2, aes(label=lab), x=4.5, y=key_y-3, size=5/.pt)+
  geom_histogram(data=seros, aes(x=prevTiter), fill="grey89", binwidth=1, color="black", size=0.1)+
  theme_bw(base_size = 8)+
  theme(
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text=element_text(colour="black",size=6),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title=element_text(size=6),
        axis.ticks=element_line(size=0.2),
        legend.key.size = unit(5, 'points'),
        legend.position=c(0.8,0.98),
        
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        panel.spacing = unit(0.05, "lines"))+
  geom_text(data = labs, aes(label = lab),x = 1, y = 30, size=6/.pt)+
  labs(x=expression(paste(" Titre (", log[2],")")), y="Count", fill="(Sub)type", pattern="Cohort")+
  facet_grid(rows=vars(year), cols=vars(strain) )+
  base_breaks_y2(0:60, seq(0,60,10), 2017:2021)+
  base_breaks_x2(0:8, 0:8, 2021) + theme(legend.text=element_text(size=6))


################################################################################
# Panel (d) MCMC waning parameter output
################################################################################

# Change order of data
waning_mcmc_intervals$cohort<-factor(waning_mcmc_intervals$cohort, levels = c("ACS\n2020-21", "RECoVERED\n2020-21", "ACS & RECoVERED\n2020-21", "ACS\n2017-21"))

waning_mcmc_intervals$strain<-factor(waning_mcmc_intervals$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))


fig_wane<-ggplot(waning_mcmc_intervals, aes(x=cohort))+
  geom_hline(yintercept=-1, linetype=2, color="grey", lwd=0.2)+
  geom_hline(yintercept=0, linetype=2, color="grey", lwd=0.2)+
  geom_linerange(aes(ymin=lo, ymax=hi,color=strain), position=position_dodge(0.5), size=3)+
  geom_linerange(aes(ymin=lolo, ymax=hihi,color=strain), position=position_dodge(0.5), size=1)+
  geom_linerange(aes(ymin=med-0.007, ymax=med+0.007,group=strain),color='grey', position=position_dodge(0.5), size=2.5)+
  coord_cartesian(ylim=c(-1, 0.3))+
  scale_color_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  scale_fill_manual(values=c("#44AA99", "#DDCC77", "#AA4499", "#332288"))+
  labs(x="Cohort", y=expression(paste("Waning rate (", log[2], " titre per year)")))+
  theme_bw(base_size = 8)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text=element_text(colour="black",size=6),
        axis.title=element_text(size=6),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=6),
        legend.key.size = unit(8, 'points'),
        axis.ticks=element_line(size=0.2),
        panel.grid.minor = element_blank(),
        axis.line.x=element_line(size=0.2))+
  base_breaks_y((seq(-10,2,2))/10, (seq(-10,2,2))/10, "A/H3N2")

################################################################################
# Join panels
################################################################################

# Get legend
leg = get_legend(fig_wane) 

#Arrange plots
p = ((((fig_epi/fig_gmt/fig_wane+theme(legend.position = 'none'))|(fig_sero)) + plot_layout(widths=c(0.57,0.43)))
     /as_ggplot(leg)) + plot_layout(heights=c(0.5,0.02))


out_name<-paste0("figs/Figure_3.pdf")
ggsave(out_name,p, width= 183, height=110, units="mm")
