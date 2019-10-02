library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(rdrop2)
library(V8)
# setwd('G:/h2k-data/Projects/CrowdWater/App & Homepage/Homepage/DataDashboard/CrowdWaterDashboard/')
source('./CW_API_Download.R')
# icons: http://rstudio.github.io/shinydashboard/appearance.html
# the javascript code to refresh the entire page.
jscode <- "shinyjs.refresh = function() { history.go(0); }"
# Define UI for application ----
ui <- dashboardPage(
  dashboardHeader(titleWidth = 12,title = "CrowdWater Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall",tabName = "sb_stats", icon = icon("dashboard")),
      menuItem("Water Level",tabName = 'sb_wl_stats',icon = icon("dashboard")),
      menuItem("Soil Moisture",tabName = 'sb_sm_stats',icon = icon("dashboard")),
      menuItem("Intermittent Streams" ,tabName = 'sb_ts_stats',icon = icon("dashboard")),
      menuItem("Plastic Pollution",tabName = 'sb_pp_stats',icon = icon("dashboard")),
      menuItem("Contribution Plots", tabName = "sb_plots",icon = icon("chart-line")),
      menuItem("Citizen Scientist Plots", tabName = "sb_citsciplots",icon = icon("chart-line")),
      menuItem("About", tabName = "about",icon = icon("info-circle"))
    )),
  dashboardBody(shinyjs::useShinyjs(),
                extendShinyjs(text = jscode),# the javascript code to refresh the entire page.
                tags$head(tags$style(HTML(".small-box {height: 110px}"))),
                # # add custom JS code to disable the header, however then the switching between sidebartabs is also disabled...
                # shinyjs::extendShinyjs(text = "shinyjs.hidehead = function(parm){
                #                     $('header').css('display', parm);
                #                 }"),
                tabItems(
                  tabItem(tabName="sb_stats",
                          
                          fluidRow(
                            valueBoxOutput(width = 6,"TotnContribs") %>% withSpinner(color='#7dbdeb'),
                            valueBoxOutput(width = 6,"uqRootsSpots")),
                          fluidRow(valueBoxOutput(width = 3,"nWLContribs"),
                                   valueBoxOutput(width = 3,"nSMContribs"),
                                   valueBoxOutput(width = 3,"nTSContribs"),
                                   valueBoxOutput(width = 3,"nPLContribs")),
                          fluidRow(box(width = 6,title = NULL,uiOutput('sliderAll')), # see output$sliderAll = renderUI() in server()
                                   valueBoxOutput(width=3,"stationsWithXcontribs"),
                                   valueBoxOutput(width=3,"UsersWithXcontribs")
                          ),
                          actionButton("reloadAllCWdata","Refresh Dashboard from CrowdWater servers (might take a while)",icon = icon("download"))
                  ),
                  tabItem(tabName="sb_wl_stats",
                          fluidRow(valueBoxOutput(width=3,"nWLRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderWL')),
                                   valueBoxOutput(width = 3,"WLStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXWLcontribs"))
                  ),
                  
                  tabItem(tabName="sb_sm_stats",
                          fluidRow(valueBoxOutput(width=3,"nSMRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderSM')),
                                   valueBoxOutput(width = 3,"SMStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXSMcontribs"))
                  ),
                  tabItem(tabName="sb_ts_stats",
                          fluidRow(valueBoxOutput(width=3,"nTSRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderInterStr')),
                                   valueBoxOutput(width = 3,"IntStrStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXTScontribs"))
                  ),
                  tabItem(tabName="sb_pp_stats",
                          fluidRow(valueBoxOutput(width=3,"nPPRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderPP')),
                                   valueBoxOutput(width = 3,"PPStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXPPcontribs"))
                  ),
                  tabItem(tabName="sb_plots",
                            fluidRow(
                              box(width = 12,    
                                  title = "Cumulative CrowdWater Contributions",    
                                  plotOutput("cumsumplot", height = "500px") %>% withSpinner(color='#7dbdeb'))
                            ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Water Level Contributions",    
                                plotOutput("cumsumplotWL", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Soil Moisture contributions",    
                                plotOutput("cumsumplotSM", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Intermittent Stream contributions",    
                                plotOutput("cumsumplotTS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Plastic Pollution contributions",    
                                plotOutput("cumsumplotPP", height = "500px"))
                          )
                  ),
                  tabItem(tabName="sb_citsciplots",
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative CrowdWater Contributors",    
                                plotOutput("cumsumplotUsers", height = "500px") %>% withSpinner(color='#7dbdeb'))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Water Level Contributors",    
                                plotOutput("cumsumplotUsersWL", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Soil Moisture Contributors",    
                                plotOutput("cumsumplotUsersSM", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Intermittent Stream Contributors",    
                                plotOutput("cumsumplotUsersTS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Plastic Pollution Contributors",    
                                plotOutput("cumsumplotUsersPP", height = "500px"))
                          )
                  ),
                  tabItem(tabName = "about",
                          fluidPage(
                            img(src='Logo_Crowdwater_pos.png', width = "240px"),
                            HTML('<br><br>This Dashboard was created for the <a href="www.crowdwater.ch">CrowdWater</a> project at the <a href="www.geo.uzh.ch">Department of Geography</a> of the <a href="www.uzh.ch">University of Zurich</a> in <a href="www.cran.org">R</a> and <a href="https://rstudio.com/">RStudio</a> and runs on <a href="www.shinyapps.io">Shinyapps.io</a> <br>'),
                            HTML('The CrowdWater project was funded by the <a href="http://www.snf.ch">Swiss National Science Foundation</a> <br>'),
                            HTML('Click <a href="https://crowdwater.shinyapps.io/CrowdWaterDashboard/"target="_blank">here</a> for the fullscreen version. <br>'),
                            HTML('&#169; Simon Etter, the code can be found on my <a href="https://github.com/SimonEtter/CrowdWaterDashboard"target="_blank">GitHub</a> account.<br><br><br>'),
                            img(src='uzh_logo_e_pos.png', width = "200px"),HTML('<br><br>'),img(src='SNF_RGB_E_POS.png', width = "200px")
                            )
                          )
                )
  )
)


# Define server logic ----
server <- function(input, output,session) {
  # js$hidehead('none')    # would be to hide the header, but that also disables the sidebar controls, therefore not used

  locFile4Attempt = 'CW_Data.csv'
  observeEvent(input$reloadAllCWdata,{
    CWdataFull = Download_AllCWdata_from_API()
    colnames(CWdataFull)[1]='Spot_ID'
    write.csv(CWdataFull,file=paste0("CWdata/",locFile4Attempt),row.names = F)
    js$refresh()
    })
  
  
  if(file.exists(paste0("CWdata/",locFile4Attempt))){
    CWdataFull = read.csv(paste0("CWdata/",locFile4Attempt))
    latestUpdate = CWdataFull$created_at[nrow(CWdataFull)]
    newCWdata = Download_LatestCWdata_from_API(lastDate = latestUpdate)
    if(!is.null(newCWdata)){
      colnames(newCWdata)[1]='Spot_ID'
      CWdataFull = rbind(CWdataFull,newCWdata)
      write.csv(CWdataFull,file=paste0("CWdata/",locFile4Attempt),row.names = F)
    }
  }else{
    CWdataFull = Download_AllCWdata_from_API()
    colnames(CWdataFull)[1]='Spot_ID'
    write.csv(CWdataFull,file=paste0("CWdata/",locFile4Attempt),row.names = F)
  }
  CWdata = CWdataFull # select all CW data 
  
  
  # Base data manipulations for dashboard ----
  CWdata$created_at = as.POSIXlt(CWdata$created_at,format = '%Y-%m-%d %H:%M:%S',tz='GMT',usetz=T)
  uq.dates = unique(CWdata$created_at)
  uq.roots = unique(CWdata$root_id)
  uq.users = unique(CWdata$spotted_by)
  # the start date is approximately when the app was officially launched, there are some pics from earlier dates that were added manually by us
  dateSeries = seq(from=min(as.POSIXct("2017-01-01 01:00:00 GMT")),to=max(uq.dates)+3600*24,by='1 day')
  attr(dateSeries,"tzone") = 'GMT'

  
  # cumsums at dates and Root IDs with corresponding updates
  cumSums = sapply(dateSeries,function(x) length(CWdata$Spot_ID[CWdata$created_at<=x]))
  cumSumUsers = sapply(dateSeries,function(x) length(unique(CWdata$spotted_by[CWdata$created_at<=x])))
  
  IdsPerRoot = sapply(uq.roots,function(x) CWdata$Spot_ID[CWdata$root_id==x])
  IdsPerUser = sapply(uq.users,function(x) CWdata$Spot_ID[CWdata$spotted_by==x])
    
  # for the slider further below 
  maxcontribs = max(sapply(IdsPerRoot, function(x) length(x)))
  maxcontribUser = max(sapply(IdsPerUser, function(x) length(x)))
  
  # for intermittent streams
  CWdataTS = CWdata[CWdata$category==468,]
  IdsPerRootTS = sapply(uq.roots,function(x) CWdataTS$Spot_ID[CWdataTS$root_id==x])
  IdsPerUserTS = sapply(uq.users,function(x) CWdataTS$Spot_ID[CWdataTS$spotted_by==x])
  maxcontribsTS = max(sapply(IdsPerRootTS, function(x) length(x)))
  maxcontribUserTS = max(sapply(IdsPerUserTS, function(x) length(x)))
  uq.datesTS = unique(CWdataTS$created_at)
  dateSeriesTS = seq(from=min(as.POSIXct("2017-01-01 01:00:00 GMT")),to=max(uq.datesTS)+3600*24,by='1 day')
  cumSumsTS = sapply(dateSeriesTS,function(x) length(CWdataTS$Spot_ID[CWdataTS$created_at<=x]))
  cumSumsUsersTS = sapply(dateSeriesTS,function(x) length(unique(CWdataTS$spotted_by[CWdataTS$created_at<=x])))
  
  # for soil moisture
  CWdataSM = CWdata[CWdata$category==469,]
  IdsPerRootSM = sapply(uq.roots,function(x) CWdataSM$Spot_ID[CWdataSM$root_id==x])
  IdsPerUserSM = sapply(uq.users,function(x) CWdataSM$Spot_ID[CWdataSM$spotted_by==x])
  maxcontribsSM = max(sapply(IdsPerRootSM, function(x) length(x)))
  maxcontribUserSM = max(sapply(IdsPerUserSM, function(x) length(x)))
  uq.datesSM = unique(CWdataSM$created_at)
  dateSeriesSM = seq(from=min(as.POSIXct("2017-01-01 01:00:00 GMT")),to=max(uq.datesSM)+3600*24,by='1 day')
  cumSumsSM = sapply(dateSeriesSM,function(x) length(CWdataSM$Spot_ID[CWdataSM$created_at<=x]))
  cumSumsUsersSM = sapply(dateSeriesSM,function(x) length(unique(CWdataSM$spotted_by[CWdataSM$created_at<=x])))
  
  # for plastic pollution streams
  CWdataPP = CWdata[CWdata$category==1919,]
  IdsPerRootPP = sapply(uq.roots,function(x) CWdataPP$Spot_ID[CWdataPP$root_id==x])
  IdsPerUserPP = sapply(uq.users,function(x) CWdataPP$Spot_ID[CWdataPP$spotted_by==x])
  maxcontribsPP = max(sapply(IdsPerRootPP, function(x) length(x)))
  maxcontribUserPP = max(sapply(IdsPerUserPP, function(x) length(x)))
  uq.datesPP = unique(CWdataPP$created_at)
  dateSeriesPP = seq(from=min(as.POSIXct("2017-01-01 01:00:00 GMT")),to=max(uq.datesPP)+3600*24,by='1 day')
  cumSumsPP = sapply(dateSeriesPP,function(x) length(CWdataPP$Spot_ID[CWdataPP$created_at<=x]))
  cumSumsUsersPP = sapply(dateSeriesPP,function(x) length(unique(CWdataPP$spotted_by[CWdataPP$created_at<=x])))
  
  # for water levels
  CWdataWL = CWdata[CWdata$category==470,]
  IdsPerRootWL = sapply(uq.roots,function(x) CWdataWL$Spot_ID[CWdataWL$root_id==x])
  IdsPerUserWL = sapply(uq.users,function(x) CWdataWL$Spot_ID[CWdataWL$spotted_by==x])
  maxcontribsWL = max(sapply(IdsPerRootWL, function(x) length(x)))
  maxcontribUserWL = max(sapply(IdsPerUserWL, function(x) length(x)))
  uq.datesWL = unique(CWdataWL$created_at)
  dateSeriesWL = seq(from=min(as.POSIXct("2017-01-01 01:00:00 GMT")),to=max(uq.datesWL)+3600*24,by='1 day')
  cumSumsWL = sapply(dateSeriesWL,function(x) length(CWdataWL$Spot_ID[CWdataWL$created_at<=x]))
  cumSumsUsersWL = sapply(dateSeriesWL,function(x) length(unique(CWdataWL$spotted_by[CWdataWL$created_at<=x])))

  # Plots with contributions ----
  cumPlot = cumplot(dateSeries, cumSums)
  output$cumsumplot = renderPlot({cumPlot})
  
  cumPlotWL = cumplot(dateSeriesWL, cumSumsWL)
  output$cumsumplotWL = renderPlot({cumPlotWL})
  
  cumPlotSM = cumplot(dateSeriesSM, cumSumsSM)
  output$cumsumplotSM = renderPlot({cumPlotSM})
  
  cumPlotTS = cumplot(dateSeriesTS, cumSumsTS)
  output$cumsumplotTS = renderPlot({cumPlotTS})
  
  cumPlotPP = cumplot(dateSeriesPP, cumSumsPP)
  output$cumsumplotPP = renderPlot({cumPlotPP})
  
  # Plots with Users
  cumPlotUsers = cumplot(dateSeries, cumSumUsers)
  output$cumsumplotUsers = renderPlot({cumPlotUsers})
  
  cumPlotUsersWL = cumplot(dateSeriesWL, cumSumsUsersWL)
  output$cumsumplotUsersWL = renderPlot({cumPlotUsersWL})
  
  cumPlotUsersSM = cumplot(dateSeriesSM, cumSumsUsersSM)
  output$cumsumplotUsersSM = renderPlot({cumPlotUsersSM})
  
  cumPlotUsersTS = cumplot(dateSeriesTS, cumSumsUsersTS)
  output$cumsumplotUsersTS = renderPlot({cumPlotUsersTS})
  
  cumPlotUsersPP = cumplot(dateSeriesPP, cumSumsUsersPP)
  output$cumsumplotUsersPP = renderPlot({cumPlotUsersPP})

  # Value Boxes ----
  output$TotnContribs <- renderValueBox({
    valueBox(      
      formatC(nrow(CWdata), format="d", big.mark=','),
      paste('Total Number of Contributions'),
      icon = icon("globe",lib='font-awesome'),
      color = "navy")    })
  
  output$nWLContribs <- renderValueBox({
    valueBox(      
      formatC(nrow(CWdata[CWdata$category==470,]), format="d", big.mark=','),
      paste('Number of Waterlevel Contributions'),
      icon = icon("ruler-vertical",lib='font-awesome'),
      color = "aqua")    })
  
  output$nSMContribs <- renderValueBox({
    valueBox(      
      formatC(nrow(CWdata[CWdata$category==469,]), format="d", big.mark=','),
      paste('Number of Soil Moisture Contributions'),
      icon = icon("allergies",lib='font-awesome'),
      color = "maroon")    })
  
  output$nTSContribs <- renderValueBox({
    valueBox(      
      formatC(nrow(CWdataTS), format="d", big.mark=','),
      paste('Number of Intermittent Stream Contributions'),
      icon = icon("tint-slash",lib='font-awesome'),
      color = "olive")    })
  
  output$nWLRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataWL$root_id)),format="d", big.mark = ','),
      "Number of Water Level Spots",
      icon=icon("tint-slash",lib='font-awesome'),
      color = "olive")
    })
  
  output$nSMRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataSM$root_id)),format="d", big.mark = ','),
      "Number of Soil Moisture Spots",
      icon=icon("tint-slash",lib='font-awesome'),
      color = "olive")
  })
  
  output$nTSRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataTS$root_id)),format="d", big.mark = ','),
      "Number of Intermittent Stream Spots",
      icon=icon("tint-slash",lib='font-awesome'),
      color = "olive")
  })
  
  output$nPPRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataPP$root_id)),format="d", big.mark = ','),
      "Number of Plastic Pollution Spots",
      icon=icon("tint-slash",lib='font-awesome'),
      color = "olive")
  })
  
  output$nPLContribs <- renderValueBox({
    valueBox(      
      formatC(nrow(CWdata[CWdata$category==1919,]), format="d", big.mark=','),
      paste('Number of Plastic-Pollution Contributions'),
      icon = icon("puzzle-piece",lib='font-awesome'),
      color = "purple")    })
  
  #define the slider with adapted max value----
  output$sliderAll = renderUI({
    sliderInput(inputId = "XnrContribs", label = "Number of contributions:",step = 1,min = 1, max = max(c(maxcontribs,maxcontribUser)),value = 10)
  })
  output$sliderInterStr = renderUI({
    sliderInput(inputId = "XnrInterStrContribs", label = "Number of Intermittent Stream contributions:",step = 1,min = 1, max = max(c(maxcontribsTS,maxcontribUserTS)),value = 10)
  })
  output$sliderSM = renderUI({
    sliderInput(inputId = "XnrSMContribs", label = "Number of Soil Moistre contributions:",step = 1,min = 1, max = max(c(maxcontribsSM,maxcontribUserSM)),value = 10)
  })
  output$sliderPP = renderUI({
    sliderInput(inputId = "XnrPPContribs", label = "Number of Plastic Pollution contributions:",step = 1,min = 1, max = max(c(maxcontribsPP,maxcontribUserPP)),value = 10)
  })
  output$sliderWL = renderUI({
    sliderInput(inputId = "XnrWLContribs", label = "Number of Water Level contributions:",step = 1,min = 1, max = max(c(maxcontribsWL,maxcontribUserWL)),value = 10)
  })
  
  output$uqRootsSpots <- renderValueBox({
    valueBox(      
      formatC(length(uq.roots), format="d", big.mark=','),
      paste('# Root-Spots'),
      icon = icon("signal",lib='font-awesome'),
      color = "light-blue")    })
  
  # Users with more than X (slider) contributions ----
  output$UsersWithXcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUser, function(x) length(x)>=input$XnrContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs), 
      subtitle=paste0('Users with \u2265 ',input$XnrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  output$UsersWithXWLcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserWL, function(x) length(x)>=input$XnrWLContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs), 
      subtitle=paste0('Users with \u2265 ',input$XnrWLContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Soil Moisture (slider) contributions
  output$UsersWithXSMcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserSM, function(x) length(x)>=input$XnrSMContribs )))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs), 
      subtitle=paste0('Users with \u2265 ',input$XnrSMContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Intermittent Stream (slider) contributions
  output$UsersWithXTScontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserTS, function(x) length(x)>=input$XnrInterStrContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs), 
      subtitle=paste0('Users with \u2265 ',input$XnrInterStrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Plastic Pollution (slider) contributions
  output$UsersWithXPPcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserPP, function(x) length(x)>=input$XnrPPContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs), 
      subtitle=paste0('Users with \u2265 ',input$XnrPPContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Stations with more than X (slider) contributions ----
  output$stationsWithXcontribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRoot, function(x) length(x)>=input$XnrContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs), 
      subtitle=paste0('Stations with \u2265 ',input$XnrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Intermittent Stream Stations with more than X (slider) contributions
  output$IntStrStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootTS, function(x) length(x)>=input$XnrInterStrContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs), 
      subtitle=paste0('Intermittent Stream Stations with \u2265 ',input$XnrInterStrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Soil Moisture Stations with more than X (slider) contributions
  output$SMStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootSM, function(x) length(x)>=input$XnrSMContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs), 
      subtitle=paste0('Soil Moisture Stations with \u2265 ',input$XnrSMContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Plastic Pollution Stations with more than X (slider) contributions
  output$PPStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootPP, function(x) length(x)>=input$XnrPPContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs), 
      subtitle=paste0('Plastic Pollution Stations with \u2265 ',input$XnrPPContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Water level Stations with more than X (slider) contributions
  output$WLStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootWL, function(x) length(x)>=input$XnrWLContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs), 
      subtitle=paste0('Water Level Stations with \u2265 ',input$XnrWLContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
