################################################################################
# Functions to process serology data. Used before performing analysis and
# in prepping data for figures
################################################################################

# Process data
prep_dat<-function(dat)
{
  # Get rid of NAs
  dat<-dat[!is.na(dat$LogTiter),]
  
  # Set start time to 0 for each patient
  dat<-dat%>%group_by(pat_id, strain)%>%mutate(t=date-min(date, na.rm=T))
  
  # Eliminate subject who were vaccinated in any year
  dat<-dat%>%group_by(pat_id)%>%mutate(vaxany=("yes"%in%vax))
  dat<-dat%>%filter(vaxany==F)
  
  return(dat)
}

# Remove strain data to which individuals seroconverted (>= 2 log unit increase
# in a given year) from dat
remove_sero<-function(dat)
{
  dat<-dat%>%group_by(pat_id, strain)%>%filter(max(diff, na.rm = T)<2)
  
  return(dat)
}