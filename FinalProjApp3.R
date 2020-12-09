library(RSocrata)
library(nycgeo)
library(sqldf)
library(tidyverse)
library(sf)
library(shiny)
library(DT)



NycSnapData<-read.socrata("https://data.cityofnewyork.us/resource/jye8-w4d7.json")
NycDistrictData<-read.socrata("https://data.cityofnewyork.us/resource/xi7c-iiu2.json")
NySnapCaseData<-read.socrata("https://data.ny.gov/resource/dq6j-8u8z.json")
NycSnapData$month<-as.Date(NycSnapData$month)


NycSnapData2<-sqldf("WITH CDNAME AS(
                        SELECT *,
                        CASE WHEN borough='Bronx' THEN 'B'
                             WHEN borough='Brooklyn' THEN 'K'
                             WHEN borough='Manhattan' THEN 'M'
                             WHEN borough='Queens' THEN 'Q'
                             WHEN borough='Staten Island' THEN 'S'
                             END AS boroughcode,
                             RIGHTSTR('00' || cd_number, 2)  as cd_number2
                        FROM NycDistrictData),
                        
                        CDNAME2 AS(
                        SELECT *,boroughcode||cd_number2 as cd FROM CDNAME),
                        
                        OUTPUT1 AS(
                        SELECT s.*,
                        d.borough,
                        d.cd_name,
                        d.cd_number,
                        d.cd_number2,
                        strftime('%m', s.month * 3600 * 24, 'unixepoch') as Mth,
                        strftime('%Y', s.month * 3600 * 24, 'unixepoch') as Yr,
                        SUM(bc_snap_recipients) OVER(PARTITION BY boro,month) as totalbyboro,
                        SUM(bc_snap_recipients) OVER(PARTITION BY month) as totaloverall
                        FROM NycSnapData as s
                        LEFT JOIN CDNAME2 as d
                        ON s.cd=d.cd),
                        
                        
                        OUTPUT2 AS(
                        SELECT * , 
                        printf('%.3f',CAST(bc_snap_recipients as FLOAT)/CAST(totalbyboro as FLOAT))*100 as percentofboro,
                        printf('%.3f',CAST(bc_snap_recipients as FLOAT)/CAST(totaloverall as FLOAT))*100 as percentofoverall
                        FROM OUTPUT1)
                        
                        SELECT *,
                        CASE WHEN bc_snap_recipients between 0 AND 14999 THEN 'Less Than 15k'
                             WHEN bc_snap_recipients between 15000 AND 29999 THEN 'Between 15k-30k'
                             WHEN bc_snap_recipients between 30000 AND 44999 THEN 'Between 30k-45k'
                             WHEN bc_snap_recipients between 45000 AND 59999 THEN 'Between 45k-60k'
                             WHEN bc_snap_recipients > 59999 THEN 'Greater Than 60k' END AS 'SnapRecipientLevels'
                        
                        FROM OUTPUT2
                        WHERE Mth='09' ")

NycSnapData2$bc_snap_recipients<-as.integer(NycSnapData2$bc_snap_recipients)
NycSnapData2<-NycSnapData2 %>%select(cd,cd_name,Yr,borough,bc_snap_recipients)

wide_NycSnapData2 <- NycSnapData2 %>% spread(Yr,bc_snap_recipients)

wide_NycSnapData2<-sqldf("SELECT *, 
                          printf('%.3f',(cast([2019] as float)-cast([2018] as float))/cast([2018] as float))*100 as '%change_19', 
                          printf('%.3f',(cast([2020] as float)-cast([2019] as float))/cast([2019] as float))*100 as '%change_20'
                          FROM wide_NycSnapData2")



ui <- fluidPage(

  fluidRow(column(8,dataTableOutput('dto')))
)

server <- function(input,output){

  thedata <- reactive(wide_NycSnapData2)
  
  #code reference for downloading excel file from table https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table
  output$dto <- renderDataTable(thedata(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = c('excel', 'pdf', 'print'))
  )
}
shinyApp(ui = ui, server = server)

