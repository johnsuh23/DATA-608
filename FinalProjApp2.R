library(RSocrata)
library(nycgeo)
library(sqldf)
library(tidyverse)
library(ggplot2)
library(sf)
library(shiny)
library(rsconnect)


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
                        CASE WHEN bc_snap_recipients between 0 AND 14999 THEN 1
                             WHEN bc_snap_recipients between 15000 AND 29999 THEN 2
                             WHEN bc_snap_recipients between 30000 AND 44999 THEN 3
                             WHEN bc_snap_recipients between 45000 AND 59999 THEN 4
                             WHEN bc_snap_recipients > 59999 THEN 5 END AS 'SnapRecipientLevels'
                        
                        FROM OUTPUT2
                        WHERE Mth='09' ")


NycSnapData2 <- NycSnapData2 %>%  
  mutate(
    boro2 = recode(
      NycSnapData2$borough, 
      "Manhattan" = 1, 
      "Bronx" = 2, 
      "Brooklyn" = 3, 
      "Queens" = 4, 
      "Staten Island" = 5
    ),
    boro_cd = as.integer(str_c(boro2, cd_number2)) # need integer to match cd_shapes format
  )


NycSnapData2 <- NycSnapData2 %>% 
  select(boro_cd,cd,Yr,SnapRecipientLevels,bc_snap_recipients)


nyc_boundaries<-nyc_boundaries(geography = "cd")
nyc_boundaries$borough_cd_id<-as.integer(nyc_boundaries$borough_cd_id)


NycSnapData2<-nyc_boundaries %>% left_join(NycSnapData2, by = c("borough_cd_id" = "boro_cd"))
NycSnapData2$bc_snap_recipients<-as.numeric(NycSnapData2$bc_snap_recipients)




ui <- fluidPage(
  
  titlePanel("SNAP Recipients"),
  
  sidebarLayout(
    
    sidebarPanel(selectInput("Type", 'Select Year', choices = c("2018",
                                                                "2019",
                                                                "2020"
    ))),
    
    mainPanel( plotOutput('snap'))  )
)

server <- function(input, output, session) {
  output$snap  <- renderPlot({
    snap <- NycSnapData2 %>% 
      filter(Yr==input$Type)
    ggplot(snap, aes(fill = SnapRecipientLevels)) +  scale_fill_gradient(low = "dodgerblue", high = "dodgerblue4") +
      geom_sf()+theme_void()+ggtitle("SNAP Recipients By Community District") + labs(caption = "1. < 15k  2. 15k to 30k  3.  30k to 45k  4. 45k to 60k  5. > 60k")}) 
}

shinyApp(ui = ui, server = server)