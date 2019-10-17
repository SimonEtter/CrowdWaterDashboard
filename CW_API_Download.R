## Function to Download CrowdWater data from API ----
Download_AllCWdata_from_API = function(){
  require(jsonlite)
  require(curl)
  WL_LUT = data.frame(WLID = 492:505,WLInput=-6:7)
  SM_LUT = data.frame(SMID = 477:484,SMnr = seq_along(477:484),SMInput=c("dry","gradually damp","gradually wet","immediately wet","muddy","welling","submerged","rain / snow"))
  TS_LUT = data.frame(TSID = 471:476,TSnr = seq_along(471:476),TSInput=c("dry streambed","damp / wet streambed","isolated pools","trickling water","standing water","flowing"))
  PP_LUT = data.frame(PPID = 1926:1933,PPnr=seq_along(1926:1933),PPInput= c("no plastic","1-2 pieces","3-5 pieces","6-10 pieces","11-20 pieces","21-100 pieces","100+ pieces","covered entirely"))
  
  # output  path
  # outFile = 'G:/h2k-data/Projects/CrowdWater/Daten/CrowdWater/API_Exports/CrowdWater_APIData.csv'
  
  # String used as a basis for making the API-Queries
  baseString = 'https://www.spotteron.com/api/v2/spots?filter[topic_id]=7&filter[created_at__gt]=2016-01-01%2014:30:00&limit=100&page=1&order[]=created_at+asc'
  # imagePreString = "https://files.spotteron.com/images/spots/"
  
  allDownloaded = FALSE
  # last_startDate = "2016-01-01 00:00"
  t.downloadString = baseString
  counter = 1
  while(!allDownloaded){
    
    last_startDate = substr(t.downloadString,82,102)
    #insert desired ROOT ID
    if(counter==1){
      t.preldata = fromJSON(t.downloadString)[[1]]
      t.data = cbind(t.preldata$id,t.preldata$attributes)
    }else{
      t.preldata = fromJSON(t.downloadString)[[1]]
      t.newdata = cbind(t.preldata$id,t.preldata$attributes)
      if(is.null(t.newdata)){
        allDownloaded=T
        # stop("all CW data downloaded")
      }
      t.data = rbind(t.data,t.newdata)
    }
    
    # replace the start date in the download string
    next_startDate = sub(' ','%20',t.data$created_at[nrow(t.data)])
    t.downloadString = sub(last_startDate,next_startDate,t.downloadString)
    print(counter)
    counter = counter +1
  }  
  # change the value of the water level, soil moisture etc data
  WLIds = t.data$fld_05_00000066
  t.data$Streamlevel = unlist(sapply(WLIds, function(x) if(is.na(x)){return(x)}else{WL_LUT$WLInput[WL_LUT$WLID==x]}))  
  
  SMIds = t.data$fld_05_00000052
  t.data$SoilMoisture = unlist(sapply(SMIds, function(x) if(is.na(x)){return(x)}else{SM_LUT$SMInput[SM_LUT$SMID==x]}))  
  
  TSIds = t.data$fld_05_00000051
  t.data$TempStream = unlist(sapply(TSIds, function(x) if(is.na(x)){return(x)}else{TS_LUT$TSInput[TS_LUT$TSID==x]}))  
  
  PPIds = t.data$fld_05_00000286
  t.data$PlasticPieces = unlist(sapply(PPIds, function(x) if(is.na(x)){return(x)}else{PP_LUT$PPInput[PP_LUT$PPID==x]}))  
  
  return(t.data)
  stop("all CW data downloaded")
}

# Download latest CW data from API ----
Download_LatestCWdata_from_API = function(lastDate = '2016-01-01 14:30:00'){
  require(jsonlite)
  require(curl)
  WL_LUT = data.frame(WLID = 492:505,WLInput=-6:7)
  SM_LUT = data.frame(SMID = 477:484,SMnr = seq_along(477:484),SMInput=c("dry","gradually damp","gradually wet","immediately wet","muddy","welling","submerged","rain / snow"))
  TS_LUT = data.frame(TSID = 471:476,TSnr = seq_along(471:476),TSInput=c("dry streambed","damp / wet streambed","isolated pools","trickling water","standing water","flowing"))
  PP_LUT = data.frame(PPID = 1926:1933,PPnr=seq_along(1926:1933),PPInput= c("no plastic","1-2 pieces","3-5 pieces","6-10 pieces","11-20 pieces","21-100 pieces","100+ pieces","covered entirely"))
  
  lastDate = sub(' ','%20',lastDate)
  
  # String used as a basis for making the API-Queries
  baseString = 'https://www.spotteron.com/api/v2/spots?filter[topic_id]=7&filter[created_at__gt]=2016-01-01%2014:30:00&limit=100&page=1&order[]=created_at+asc'

  allDownloaded = FALSE
  downloadString = sub('2016-01-01%2014:30:00',lastDate,baseString)
  counter = 1
  last_startDate = lastDate
  while(!allDownloaded){
    if(counter==1){
      t.downloadString = downloadString
      t.preldata = fromJSON(t.downloadString)[[1]]
      if(length(t.preldata)==0){
        allDownloaded = T
        t.data = NULL
      }else{
        t.data = cbind(t.preldata$id,t.preldata$attributes)
      }
      
    }else{
      t.preldata = fromJSON(t.downloadString)[[1]]
      t.newdata = cbind(t.preldata$id,t.preldata$attributes)
      if(is.null(t.newdata)){
        allDownloaded=T
      }else{
        t.data = rbind(t.data,t.newdata)
      }
      
    }
    
    # replace the start date in the download string
    next_startDate = sub(' ','%20',t.data$created_at[nrow(t.data)])
    if(!is.null(t.data)){
      t.downloadString = sub(lastDate,next_startDate,downloadString)
    }
    print(counter)
    counter = counter +1
  }  
  # change the value of the water level data
  WLIds = t.data$fld_05_00000066
  t.data$Streamlevel = unlist(sapply(WLIds, function(x) if(is.na(x)){return(x)}else{WL_LUT$WLInput[WL_LUT$WLID==x]}))  
  
  SMIds = t.data$fld_05_00000052
  t.data$SoilMoisture = unlist(sapply(SMIds, function(x) if(is.na(x)){return(x)}else{SM_LUT$SMInput[SM_LUT$SMID==x]}))  
  
  TSIds = t.data$fld_05_00000051
  t.data$TempStream = unlist(sapply(TSIds, function(x) if(is.na(x)){return(x)}else{TS_LUT$TSInput[TS_LUT$TSID==x]}))  
  
  PPIds = t.data$fld_05_00000286
  t.data$PlasticPieces = unlist(sapply(PPIds, function(x) if(is.na(x)){return(x)}else{PP_LUT$PPInput[PP_LUT$PPID==x]}))  
  
  
  return(t.data)
  # stop("all CW data updated")
}

# make the cumsumplot with the contributions ----
# dateSeries=dateSeriesPP
# cumSums=cumSumsUsersPP
cumplot = function(dateSeries,cumSums){
  
  if(max(cumSums>1000)){
    thousandSeq = which(1:max(cumSums) %% 1000 == 0)
  }else if(max(cumSums)>500 && max(cumSums)<=1000){
    thousandSeq = which(1:max(cumSums) %% 250 == 0)
  }else if(max(cumSums)>100 && max(cumSums)<=500){
    thousandSeq = which(1:max(cumSums) %% 100 == 0)
  }else if (max(cumSums)<=100){
    thousandSeq = which(1:max(cumSums) %% 50 == 0)
    }
  datesCracking1000Marks = lapply(thousandSeq,function(x) min(dateSeries[cumSums>=x]))
  datesCracking1000Marks_ext = datesCracking1000Marks
  datesCracking1000Marks_ext[[length(datesCracking1000Marks_ext)+1]] = dateSeries[length(dateSeries)]
  datesCracking1000Marks_ext = c(dateSeries[1],datesCracking1000Marks_ext)
  #calc the days it took to get another 1000 contributions
  TimeDiffs=vector()
  midDates = list()
  for(i in 1:(length(datesCracking1000Marks_ext)-1)){
    TimeDiffs[i] = (datesCracking1000Marks_ext[[i+1]][1]-datesCracking1000Marks_ext[[i]][1])
    midDates[[i]] = seq(from=datesCracking1000Marks_ext[[i]][1],to=datesCracking1000Marks_ext[[i+1]][1],length.out = 3)[2]
  }
  
  plot1DF = data.frame(dateseries = dateSeries, cumulativeSums = cumSums)
  
  
  cumPlot = 
    ggplot(data=plot1DF)+
    geom_line(aes(x=dateseries,y=cumulativeSums))+
    geom_point(aes(x=dateSeries[length(dateseries)],y=max(cumSums))) + 
    geom_hline(yintercept = thousandSeq,col='lightgray',lty=2)+
    geom_vline(xintercept = datesCracking1000Marks_ext,col='lightgray',lty=2) +
    ylab('Cumulative Contributions') + xlab('Project duration') + 
    scale_y_continuous(limits = c(0,max(cumSums)+200),labels = if(max(cumSums)-max(thousandSeq)>500){c(0,thousandSeq,max(cumSums))}else{c(0,thousandSeq)}, 
                       breaks = if(max(cumSums)-max(thousandSeq)>500){c(0,thousandSeq,max(cumSums))}else{c(0,thousandSeq)})+
    annotate(geom = "text",x = as.POSIXct(unlist(datesCracking1000Marks),origin = '1970-01-01 00:00:00',format = '%Y-%m-%d %H:%M:%S'),y = thousandSeq+thousandSeq[1]/12, 
             label = as.character(sapply(datesCracking1000Marks,function(x) as.character(as.Date(x),format='%d.%m.%Y'))),vjust=-.05,hjust=1.05)+
    annotate(geom = "text",x = dateSeries[length(dateSeries)],y = max(cumSums),label=format(as.Date(dateSeries[length(dateSeries)]),format='%d.%m.%Y'),vjust=-1,hjust=1.05)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text(size=20))
  
  for(i in seq_along(midDates)){
    # cumPlot = cumPlot + annotate(geom = "text",x=as.POSIXct(unlist(midDates[i]),origin = '1970-01-01 00:00:00'),y=ifelse(TimeDiffs[i]>80,0,max(cumSums)/20),label = paste0(TimeDiffs[i],' days'),angle=ifelse(TimeDiffs[i]>80,0,90))
    cumPlot = cumPlot + annotate(geom = "text",x=as.POSIXct(unlist(midDates[i]),origin = '1970-01-01 00:00:00'),y=ifelse(cumSums[as.Date(dateSeries)==as.Date(midDates[[i]][1])]<10,20,0),label = paste0(TimeDiffs[i],' days'),angle=ifelse(TimeDiffs[i]>80,0,90))
    
  }
  return(cumPlot)
}