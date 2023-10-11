
setwd("serology")


library(ggplot2)
library(dplyr)
library(ggpubr)

################################################################################
# Plotting functions/themes
################################################################################

# Text size theme to adhere to nature guidelines
natty_theme<-theme(axis.text = element_text(size = 5,color='black'),
                   axis.title = element_text(size = 6),
                   strip.text = element_text(size = 6),
                   legend.text = element_text(size = 6),
                   legend.title = element_text(size = 6),
                   strip.background=element_rect(colour="white",fill="white"))

# Make random jitter reproducible
set.seed(1)

# Function I had to invoke to get y axis to scale the way I wanted for some
# annoying reason I can't remember

equal_breaks <- function(){
  function(x){
    # rescaling
    x0<-round(min(x)/0.5)*0.5
    x1<-round(max(x)/0.5)*0.5
    
    diff<-x1-x0
    
    ts<-(seq(-3, 2, by=0.5))
    if(diff<2)
    {
      ts<-(seq(-6, 5, by=1))
      ts<-ts/10
    }
    return(ts)
  }
}


################################################################################
# Prepare data
################################################################################

##################
# Load MCMC output
################## 

# Data used for model fitting are saved along with model output. Retrieve the
# data from here to ensure that any preprocessing of the data before model
# fitting is accounted for and doesn't need to be redone

# ACS 2017-21
load("out/acs_yrs_17_21_pats_1_30_5000.RData")
acs_dat<-data
out_acs<-out

# VIS
load("out/vis_yrs_20_21_5000.RData")
vis_dat<-data
out_vis<-out

##########################
# Extract sex/age metadata
##########################

# Get metadata and then onvert subject id in the metadata to match the id in the
# stan model

meta_acs<-acs_dat%>%group_by(pat_id)%>%summarise(YoB=YoB[1], gender=gender[1])
meta_acs$id<-as.numeric(as.factor(meta_acs$pat_id))

meta_vis<-vis_dat%>%group_by(pat_id)%>%summarise(YoB=YoB[1], gender=gender[1])
meta_vis$id<-as.numeric(as.factor(meta_vis$pat_id))

######################
# Extract waning rates
######################

# Waning rates for patient i are stored as alpha[i], extract these and then
# merge to the above found metadata

out_acs<-out_acs%>%filter(grepl("alpha\\[", param))
out_acs<-merge(out_acs, meta_acs)
out_acs$cohort<-"ACS"

out_vis<-out_vis%>%filter(grepl("alpha\\[", param))
out_vis<-merge(out_vis, meta_vis)
out_vis$cohort<-"RECoVERED"

# Bind data and calculate ages in 2021
df<-rbind(out_acs, out_vis)
df$age<-2021-df$YoB

################################################################################
# Plot panel (b)
################################################################################

# Get strain legend order to match that of manuscript
df$strain<-factor(df$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

# Waning rates figure

acs_dat$id = as.numeric(as.factor(acs_dat$pat_id))
for (strain in c("A/H3N2","A/H1N1pdm09","B/Victoria","B/Yamagata")){
  df = df[-which(df$strain==strain & df$cohort=="ACS" &  !(df$pat_id %in% acs_dat[acs_dat$strain==strain,]$pat_id)),]
}

fig_b  = ggplot(df[df$cohort=="ACS",], aes(x=age, y=med, fill=gender))+
  geom_smooth(alpha=0.3,aes(group=gender,color=gender))+
  geom_linerange(aes(ymin=lo, ymax=hi,color=gender), linewidth=0.8, alpha=0.5)+
  geom_linerange(aes(ymin=lolo, ymax=hihi,color=gender), linewidth=0.4, alpha=0.5)+
  
  geom_point(alpha=0.5, size=0.8,aes(bg=gender),stroke=0.2,pch=21)+
  geom_hline(yintercept = 0, linetype="dashed", linewidth=0.3)+
  theme_bw()+
  natty_theme+
  
  scale_y_continuous(breaks=equal_breaks())+
  scale_x_continuous(breaks=seq(20,80,10))+
  facet_grid(cols=vars(strain), rows=vars(cohort), scales = "free_y")+
  labs(x="Age in 2021", y=expression(paste("Waning rate (", log[2], " titre per year)")), color="Gender", fill="Gender") +
  theme(legend.position='none') + scale_fill_manual(color="#619CFF")



################################################################################
# Plot panel (a)
################################################################################

# Use all subjects for titer distribution in 2021 rather than eliminating due to
# vax or seroconversion. Load data and average titer over replicates and
# calculate ages

load("dat/ACS_dat.RData")
acs_dat<-acs_dat%>%filter(year==2021 & pat_no<31)%>%group_by(pat_id, strain, cohort, YoB, gender)%>%summarise(LogTiter=mean(LogTiter, na.rm=T))
acs_dat$age<-2021-acs_dat$YoB




load("dat/VIS_dat.RData")
vis_dat<-vis_dat%>%filter(year==2021)%>%group_by(pat_id, strain, cohort, YoB, gender)%>%summarise(LogTiter=mean(LogTiter, na.rm=T))
vis_dat$age<-2021-vis_dat$YoB

# Bind data sets
df<-rbind(acs_dat, vis_dat)

# Change name of VIS cohort
df$cohort<-gsub("VIS", "RECoVERED", df$cohort)

# Get strain legend order to match that of manuscript
df$strain<-factor(df$strain, levels=c("A/H3N2", "A/H1N1pdm09", "B/Yamagata", "B/Victoria"))

# Titer in 2021 figure

fig_a = ggplot(df[df$cohort=="ACS",], aes(x=age, y=LogTiter, bg=gender))+
  geom_jitter(aes(bg=gender), width=0.3, height=0.3, alpha=0.8, size=1.2,pch=21,stroke=0.2)+
  geom_smooth(aes(color=gender),alpha=0.3, linewidth=0.5)+
  scale_y_continuous(breaks = 0:8)+
  scale_x_continuous(breaks=seq(20,80,10))+
  theme_bw()+
  natty_theme+
  labs(x="Age in 2021", y=expression(paste("Titre in 2021 (", log[2],")")), color="Sex", fill="Sex")+
  facet_grid(cols=vars(strain), rows=vars(cohort)) + theme(legend.position='top',legend.margin=margin(0,0,0,0),
                                                           legend.box.margin=margin(-10,-10,-10,-10),
                                                           legend.background = element_blank(),
                                                           legend.key.size = unit(.4, 'cm')) + guides(color=guide_legend(override.aes=list(color=NA)),fill=guide_legend())


fig = (fig_a/fig_b)


ggsave("figs/SuppFig_6.pdf", fig, width=180, height=110, units="mm")
