library(RSocrata)
library(nycgeo)
library(sqldf)
library(tidyverse)
library(ggplot2)
library(sf)
library(shiny)
library(rsconnect)


# Your final project is to create a public visualization (static or interactive) using data relevant
# to a current policy, business, or justice issue. You may use any dataset you can find for this
# assignment, as long as it is either public or you have permission from the data's
# owner/administrator to work with it and share it.
# 
# Recommended data sources are: governmental data, data provided by a
# non-profit/Nongovernmental organizations, and data available from large, semi-structured
# data sets (ie social networks, company financials, etc).
# 
# You must document each step of your data analysis process (excluding data acquisition) in
# code: this will include changing the format of the data and the creation of any images or
# interactive displays that are made.
# 
# You must also include a short (2-3 paragraph) write-up on the visualization. This write-up
# must include the following: the data source, what the parameters of the data set are
# (geography, timeframe, what the data points are, etc) what the data shows, and why it is
# important. Your proposal and your final visualization must be cleared by the instructor, and
# will be hosted on a public page (it will be available for the world to see).


#Write Up
# For the final project, the data set chosen was data pertaining to the number of recipients in the SNAP program. The SNAP program is a program for people to get help through the government to buy essential items such as food. This data set was taken from the NYC data repository and only has data for the SNAP program at the city level of NYC. This data set was also monthly data and for the period of 2018 to the latest month reported which is September. The month of September was taken as the comparison by year for the entire data set to be analyzed and presented. Eeography wise, the data set contains recipient numbers at a community ditrict level for the city of new york.

# This project was done from the point of view of a supermarket business that has several supermarket locations throughout nyc and wanting to create a projection for its stores for its annualized budget.  This business believes its sales are impacted by items bought through the SNAP benefit program and would like to see that impact by looking at the year over year change of the number of SNAP recipients at a community district level. Because of that impact to their sales revenue, this could be important from a business point of view on how to project its sales and from those sales projections budget accordingly for its stores and plan for the upcoming year its limited resources.

# Visualization best practices as learned throughout the semester was applied as best as possible. The main one being to keep the visual simple and tight and not adding elements that could potentially distract as well confuse the viewer of what the visual is trying to convey. As my data visualization skills are just being developed and which  I hope to grow, the temptation to create more advanced visuals that may hurt the visuals is something i will need to keep an eye on and be aware of.


#data set taken from NYC gov directly through and put into dataframe through RSocrota tool
NycSnapData<-read.socrata("https://data.cityofnewyork.us/resource/jye8-w4d7.json")#dataset used
NycDistrictData<-read.socrata("https://data.cityofnewyork.us/resource/xi7c-iiu2.json")#dataset used
NySnapCaseData<-read.socrata("https://data.ny.gov/resource/dq6j-8u8z.json")#not used
NycSnapData$month<-as.Date(NycSnapData$month)


#used SQLDF to manipulate data by adding community district codes, month, yr and putting into buckets based on recipent levels
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
  select(boro_cd,borough,cd,Yr,SnapRecipientLevels,bc_snap_recipients)


#used nycgeo library tool to get shapefile data for the nyc community districts, this was needed to create the nyc map for the ggplot map graph. That data set was then joined to the NYC SNAP DATA set to bring in the recipient data.
nyc_boundaries<-nyc_boundaries(geography = "cd")
nyc_boundaries$borough_cd_id<-as.integer(nyc_boundaries$borough_cd_id)


NycSnapData2<-nyc_boundaries %>% left_join(NycSnapData2, by = c("borough_cd_id" = "boro_cd"))
NycSnapData2$bc_snap_recipients<-as.numeric(NycSnapData2$bc_snap_recipients)




# #This is the code for the shiny apps. 3 apps were made separately. 

# App1 is just a regular bar graph that is filtered by boro and ordered from highest to lowest number of recipients by district.

# App2 is a map of NYC with community district bounderies. Each district is then colored by the levels of buckets the district has been put in based on the number of SNAP recipients in the district. Dark coloring to light coloring represents high to low.
 
# App3 is just a table of the results by year for the business analyst to output as a excel file and use that file to possibly apply the year of over percent changes to an excel file he/she may have that contain historical sales and use it to create their sales projections. Excel output was choses as most business use Excel has their tool of choice for analysis.



ui <- fluidPage(
  
  titlePanel("SNAP Recipients"),
  
  sidebarLayout(
    
    sidebarPanel(selectInput ("Boro", 'Select Boro', choices = c("Bronx","Brooklyn","Queens","Manhattan","Staten Island" )),
                 
                 selectInput ("Year", 'Select Year', choices = c ("2018","2019","2020") )                  
    
    ),
    
    mainPanel( plotOutput('snap'))  )
)

server <- function(input, output, session) {
  output$snap  <- renderPlot({
    snap <- NycSnapData2 %>% 
      filter(Yr == input$Year & borough == input$Boro)
    ggplot(snap, aes(reorder(x = cd,bc_snap_recipients), y = bc_snap_recipients)) +
      geom_col(fill = "#263e63")+coord_flip()+xlab("Community District")+ylab("Snap Recipients")+ggtitle("SNAP Recipients By State")}) 
}

shinyApp(ui = ui, server = server)