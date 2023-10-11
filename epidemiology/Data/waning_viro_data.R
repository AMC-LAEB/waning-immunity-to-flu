library(lubridate)
setwd("epidemiology")
#File path
epi <- read.csv("Data/viro_raw.csv") #File containing raw FluNet surveillance data
#Threshold for the percentage of total circulation in a season for a subtype to be considered substantially circulating/dominant
thresh = 0.2

epi <- epi[,c("Country","FLUREGION","Year","Week","SDATE","SPEC_RECEIVED_NB","SPEC_PROCESSED_NB","AH1","AH1N12009","AH3","ANOTSUBTYPED","INF_A","BYAMAGATA","BVICTORIA","BNOTDETERMINED","INF_B","ALL_INF")]
epi$Date <- decimal_date(ymd(epi$SDATE))
epi = epi[order(epi$Date),]
epi[is.na(epi)] = 0
epi = epi[epi$Date < 2020.2,]
epi = epi[!(epi$Date > 2009.247 & epi$Date < 2009.5),]
epi$Season = 0

southern_hemisphere = c("South Africa","Argentina","Chile","Uruguay","Australia","New Zealand")
for (year in seq(1995,2021)){
  if(nrow(epi[epi$Country %in% southern_hemisphere & epi$Year == year,])>0){epi[epi$Country %in% southern_hemisphere & epi$Year == year,]$Season = year}
  if(nrow(epi[!(epi$Country %in% southern_hemisphere) & epi$Date >= year + 40/52 & epi$Date < year + 72/52,])>0){epi[!(epi$Country %in% southern_hemisphere) & epi$Date >= year + 0.5 & epi$Date < year + 1.5,]$Season = year}
}

byyear <- aggregate(cbind(SPEC_RECEIVED_NB,SPEC_PROCESSED_NB,AH1,AH1N12009,AH3,ANOTSUBTYPED,INF_A,BYAMAGATA,BVICTORIA,BNOTDETERMINED,INF_B,ALL_INF)~Season+Country,epi,sum)
byyear$Region = NA
for (i in 1:nrow(byyear)){
  byyear[i,]$Region = epi[epi$Country == byyear[i,]$Country,]$FLUREGION[1]
}
byyear = byyear[byyear$Season > 2001 & byyear$Season < 2020,]

#Get counts per subtype where the unsubtyped ones are assumed to follow the distribution of the subtypes ones
byyear$H3_FINAL = byyear$AH3 + (byyear$AH3/(byyear$AH3 + byyear$AH1N12009 + byyear$AH1)) * byyear$ANOTSUBTYPED
byyear$H1_FINAL = byyear$AH1 + (byyear$AH1/(byyear$AH3 + byyear$AH1 +  byyear$AH1N12009)) * byyear$ANOTSUBTYPED
byyear$H1pdm_FINAL = byyear$AH1N12009 + (byyear$AH1N12009/(byyear$AH3 + byyear$AH1N12009 + byyear$AH1)) * byyear$ANOTSUBTYPED

#Get proportions of each subtype for each season-country pair
byyear$H3_PROP = byyear$H3_FINAL / byyear$ALL_INF
byyear$H1_PROP = byyear$H1_FINAL / byyear$ALL_INF
byyear$H1pdm_PROP = byyear$H1pdm_FINAL / byyear$ALL_INF
byyear$B_PROP = byyear$INF_B / byyear$ALL_INF

#Remove country-seasons with insufficient tests
#byyear = byyear[(byyear$SPEC_RECEIVED_NB>200) | (byyear$SPEC_PROCESSED_NB>200) | (byyear$INF_A>200),]
byyear = byyear[byyear$ALL_INF > 20,]



#Binary cols whether a subtype was dominant for season-country
byyear$IS_H1_DOM = as.integer(byyear$H1_PROP > thresh)
byyear$IS_H3_DOM = as.integer(byyear$H3_PROP > thresh)
byyear$IS_H1pdm_DOM = as.integer(byyear$H1pdm_PROP > thresh)
byyear$IS_B_DOM = as.integer(byyear$B_PROP > thresh)

byyear = byyear[!(is.na(byyear$IS_H3_DOM)),]

##Make sure to keep only largest consecutive blocks of seasons, starting in 2019
for (country in unique(byyear$Country)){
  minyear = 2019 - length(which(sort(unique(byyear[byyear$Country==country,]$Season),decreasing=TRUE) - seq(2019,2019-nrow(byyear[byyear$Country==country,])+1,-1) == 0)) + 1
  byyear = byyear[!(byyear$Country==country & byyear$Season < minyear),]
}

#Cols with years since last dominance of a subtype
byyear$Y_SINCE_LAST_H3 = NA
byyear$Y_SINCE_LAST_H1 = NA
byyear$Y_SINCE_LAST_H1pdm = NA
byyear$Y_SINCE_LAST_B = NA

for (row in 1:nrow(byyear)){
  year = byyear[row,]$Season
  country = byyear[row,]$Country
  when_h3 = byyear[byyear$Country == country & byyear$IS_H3_DOM & byyear$Season < year,]$Season
  when_h1 = byyear[byyear$Country == country & byyear$IS_H1_DOM & byyear$Season < year,]$Season
  when_h1pdm = byyear[byyear$Country == country & byyear$IS_H1pdm_DOM & byyear$Season < year,]$Season
  when_b = byyear[byyear$Country == country & byyear$IS_B_DOM & byyear$Season < year,]$Season
  
  if (length(when_h3) > 0){
    byyear[row,]$Y_SINCE_LAST_H3 = min(year - when_h3)
  } else if (min(byyear[byyear$Country == country,]$Season) < year) {
    byyear[row,]$Y_SINCE_LAST_H3 = paste(">",year - min(byyear[byyear$Country == country,]$Season),sep="")
  }
  
  if (length(when_h1) > 0){
    byyear[row,]$Y_SINCE_LAST_H1 = min(year - when_h1)
  } else if (min(byyear[byyear$Country == country,]$Season) < year) {
    byyear[row,]$Y_SINCE_LAST_H1 = paste(">",year - min(byyear[byyear$Country == country,]$Season),sep="")
  }
  
  if (length(when_h1pdm) > 0){
    byyear[row,]$Y_SINCE_LAST_H1pdm = min(year - when_h1pdm)
  } else if (min(byyear[byyear$Country == country,]$Season) < year) {
    byyear[row,]$Y_SINCE_LAST_H1pdm = paste(">",year - min(byyear[byyear$Country == country,]$Season),sep="")
  }
  
  if (length(when_b) > 0){
    byyear[row,]$Y_SINCE_LAST_B = min(year - when_b)
  } else if (min(byyear[byyear$Country == country,]$Season) < year) {
    byyear[row,]$Y_SINCE_LAST_B = paste(">",year - min(byyear[byyear$Country == country,]$Season),sep="")
  }
}

byyear[byyear$Season <= 2009,]$Y_SINCE_LAST_H1pdm = NA
byyear[byyear$Season == 2010,]$Y_SINCE_LAST_H1pdm = 1
byyear[byyear$Season >= 2009,]$Y_SINCE_LAST_H1 = NA

write.csv(byyear,"Data/viro_data.csv",row.names=F,quote=F)




