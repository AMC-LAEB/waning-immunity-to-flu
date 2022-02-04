library(lubridate)

ili <- read.csv("ili_raw.csv")
ili = ili[,c("Country","Year","Week","SDATE","ILI_CASES","ILI_POP_COV","ILI_POP_RATE","FLULONG")]
ili$Date <- decimal_date(ymd(ili$SDATE))
ili = ili[ili$Date < 2020.2 & ili$Date > 2010.0,]
ili[is.na(ili)] = 0
ili$Season = 0

southern_hemisphere = c("South Africa","Argentina","Chile","Uruguay","Australia","New Zealand")
#removed because of ARI or ILI curves that are not well-formed
ili = ili[!(ili$Country %in% c("Germany","Slovenia","Bulgaria","Morocco","United States of America", southern_hemisphere)),]

viro_data = read.csv("~/viro_data.csv")
  
for (year in seq(2010,2021)){
  if(nrow(ili[ili$Country %in% southern_hemisphere & ili$Year == year,])>0){ili[ili$Country %in% southern_hemisphere & ili$Year == year,]$Season = year}
  if(nrow(ili[!(ili$Country %in% southern_hemisphere) & ili$Date >= year + 40/52 & ili$Date < year + 70/52,])>0){ili[!(ili$Country %in% southern_hemisphere) & ili$Date >= year + 0.5 & ili$Date < year + 1.5,]$Season = year}
}


ili_byyear <- aggregate(cbind(ILI_CASES)~Season+Country,ili,sum)
ili_byyear = ili_byyear[ili_byyear$Season > 2009 & ili_byyear$Season < 2020,]


ili_byyear$REL = 0
ili_byyear$B_REL = 0
ili_byyear$H3_REL = 0
ili_byyear$H1pdm_REL = 0

for (i in 1:nrow(ili_byyear)){
  season = ili_byyear[i,]$Season
  country = ili_byyear[i,]$Country
  ili_byyear[i,]$REL = ili_byyear[i,]$ILI_CASES / sum(ili_byyear[ili_byyear$Country==country,]$ILI_CASES)
  prop_h3 = viro_data[viro_data$Country == country & viro_data$Season == season,]$H3_PROP
  prop_h1 = viro_data[viro_data$Country == country & viro_data$Season == season,]$H1pdm_PROP
  prop_b = viro_data[viro_data$Country == country & viro_data$Season == season,]$B_PROP
  ili_byyear[i,]$H3_REL = ili_byyear[i,]$REL  * prop_h3
  ili_byyear[i,]$H1pdm_REL = ili_byyear[i,]$REL  * prop_h1
  ili_byyear[i,]$B_REL = ili_byyear[i,]$REL  * prop_b
}

ili_byyear$H3_REL = ili_byyear$H3_REL * 10
ili_byyear$H1pdm_REL = ili_byyear$H1pdm_REL * 10
ili_byyear$B_REL = ili_byyear$B_REL * 10


ili_byyear$H3_REL_LAG1 = NA
ili_byyear$H1pdm_REL_LAG1 = NA
ili_byyear$B_REL_LAG1 = NA


for (i in 1:nrow(ili_byyear)){
  ctry = ili_byyear[i,]$Country
  season = ili_byyear[i,]$Season
  if (season == min(ili_byyear[ili_byyear$Country==ctry,]$Season)){next}
  ili_byyear[i,]$H3_REL_LAG1 = ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-1,]$H3_REL
  ili_byyear[i,]$H1pdm_REL_LAG1 = ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-1,]$H1pdm_REL
  ili_byyear[i,]$B_REL_LAG1 = ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-1,]$B_REL
  
}

ili_byyear$H3_REL_LAG2 = NA
ili_byyear$H1pdm_REL_LAG2 = NA
ili_byyear$B_REL_LAG2 = NA

for (i in 1:nrow(ili_byyear)){
  ctry = ili_byyear[i,]$Country
  season = ili_byyear[i,]$Season
  if (season <= min(ili_byyear[ili_byyear$Country==ctry,]$Season)+1){next}
  ili_byyear[i,]$H3_REL_LAG2 = ili_byyear[i,]$H3_REL_LAG1 + ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-2,]$H3_REL
  ili_byyear[i,]$H1pdm_REL_LAG2 = ili_byyear[i,]$H1pdm_REL_LAG1 + ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-2,]$H1pdm_REL
  ili_byyear[i,]$B_REL_LAG2 = ili_byyear[i,]$B_REL_LAG1 + ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-2,]$B_REL
}

ili_byyear$H3_REL_LAG3 = NA
ili_byyear$H1pdm_REL_LAG3 = NA
ili_byyear$B_REL_LAG3 = NA

for (i in 1:nrow(ili_byyear)){
  ctry = ili_byyear[i,]$Country
  season = ili_byyear[i,]$Season
  if (season <= min(ili_byyear[ili_byyear$Country==ctry,]$Season)+2){next}
  ili_byyear[i,]$H3_REL_LAG3 = ili_byyear[i,]$H3_REL_LAG2 + ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-3,]$H3_REL
  ili_byyear[i,]$H1pdm_REL_LAG3 = ili_byyear[i,]$H1pdm_REL_LAG2 + ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-3,]$H1pdm_REL
  ili_byyear[i,]$B_REL_LAG3 = ili_byyear[i,]$B_REL_LAG2 + ili_byyear[ili_byyear$Country == ctry & ili_byyear$Season == season-3,]$B_REL
}

#write.csv(ili_byyear,"ili_data.csv",row.names=F,quote=F)






