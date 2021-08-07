libpack <- .packages();libpack
# library("shiny");# library("knitr");library("leaflet");library("leaflet.providers");# library("rgdal");library("dplyr")

#required package list
necpack <- c("tidyverse","shinydashboard","shiny","knitr","leaflet","leaflet.providers","rgdal","dplyr")
#load required packages
lapply(necpack,require, character.only = TRUE)


NYSSD <- readOGR("data/SchDist_2019_v3.shp") #NYSSD = School Districts
LLcoor1 <-spTransform(NYSSD,CRS("+proj=longlat")) # https://gis.ny.gov/gisdata/inventories/details.cfm?DSID=1326
NYSS <- readOGR("data/Public_K_12.shp") #NYS Schools Info
schdata <- read.csv("data/reorderd.csv") #all processed data 

NYSS@data$SED_CODE <- as.character(NYSS@data$SED_CODE)
schdata$SED_CODE <- stringr::str_sub(schdata$ST_SCHID,-12,-1);schdata$ID

NYSS@data <- dplyr::left_join(NYSS@data,schdata, by = c("SED_CODE"))
NYSS@data$GRADE_ORGA <- dplyr::if_else(is.na(NYSS@data$GRADE_ORGA),"Not Reported",NYSS@data$GRADE_ORGA)
NYSS@data$NRC_DESC <- dplyr::if_else(is.na(NYSS@data$NRC_DESC),"Not Reported",NYSS@data$NRC_DESC)

NYSS@data$OVERALL_STATUS_2018 <- dplyr::if_else(is.na(NYSS@data$OVERALL_STATUS_2018),"Not Reported",NYSS@data$OVERALL_STATUS_2018)
NYSS@data$OVERALL_STATUS_2019 <- dplyr::if_else(is.na(NYSS@data$OVERALL_STATUS_2019),"Not Reported",NYSS@data$OVERALL_STATUS_2019)

NYSS@data$PER_TEACH_INEXP1819_DIFF <- NYSS@data$PER_TEACH_INEXP2019- NYSS@data$PER_TEACH_INEXP2018
NYSS@data$nrc_desc <- NULL

sum(is.na(NYSS$Latitude));sum(is.na(NYSS$Longitude))

ui <- 
  fluidPage(
    navbarPage(theme = shinythemes::shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">NYS Schools</a>'), id="nav",
             windowTitle = "New York State School Performance"),
             sidebarPanel(
               
               titlePanel("Kill me now"),
               fluidRow(
                selectInput(inputId = "SchoolType", 
                              label = "Choose a School Type",multiple = T,
                              choices = unique(NYSS@data$GRADE_ORGA),
                              selected = c("Elementary","Junior-Senior High")
                              ),
                selectInput(inputId = "Needs", 
                              label = "Needs Type",multiple = T,
                              choices = unique(NYSS@data$NRC_DESC),
                              selected = c("Low Needs","Average Needs","High Needs")
                              ),
                selectInput(inputId = "Accountability",
                            label = "Accountabilit Status",multiple = T,
                            choices = unique(NYSS@data$OVERALL_STATUS_2018),
                            selected = c("Not Reported","Good Standing","Targeted Support and Improvement")
                ),
                
                sliderInput("range1", "2018 minus 2019 Inexperienced Teacher %",
                            value = range(NYSS@data$PER_TEACH_INEXP1819_DIFF,na.rm = T),
                            min = min(NYSS@data$PER_TEACH_INEXP1819_DIFF,na.rm = T),
                            max = max(NYSS@data$PER_TEACH_INEXP1819_DIFF,na.rm = T),step = 0.05
                            ),
                checkboxInput("legend", "Show legend", TRUE)
                )),
                mainPanel(
                  fillPage(leafletOutput("map"),
                           tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
                    
                  )
                )
               
)

server <- function(input, output) {
  
  labelsPTS <- sprintf(
    "<strong>%s</strong><br/>%s Accountability Code </sup>",
    NYSS@data$LEGAL_NAME, NYSS@data$ACC_CODE) %>%
    lapply(htmltools::HTML)

  labelsPOLY <- sprintf(
    "<strong>%s</strong><br/>%s </sup>",
    LLcoor1@data$SCHOOLDIST, LLcoor1@data$POPULAR_NA) %>%
    lapply(htmltools::HTML)
  
  output$map <- 
    renderLeaflet({
      leaflet(LLcoor1,height=2000, width=1250) %>%
        addPolygons(fillColor = "transparent",
                    color="white",opacity = .1,
                    weight = 1,fill = T,
                    label = labelsPOLY,
                    # layerId = "polys", #layer ID prohibits polygons from drawing
                    popup = NULL) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        # setView(lng = -73.717848, lat = 45.007906, zoom = 4) %>% 
        addLegend("bottomleft",
                  title = "Legend",
                  pal = pal, 
                  values = ~NYSS@data$GRADE_ORGA,
                  opacity = .8)
    }) #END MAP1
  
  
  
  filteredData <- reactive({
    
    R1 <- input$range1[1] 
    R2 <- input$range1[2]
    ST <- input$SchoolType
    Ne <- input$Needs
    
    NYSS@data[(NYSS@data$PER_TEACH_INEXP1819_DIFF >= R1 & NYSS@data$PER_TEACH_INEXP1819_DIFF <= R2) &
              (NYSS@data$GRADE_ORGA == ST) &
              (NYSS@data$NRC_DESC == Ne),]
          })
  
  # fdata <- filteredData()
  
  pal <- colorFactor(c("#7B9EB2","#C9EBFF",
                       "#8e535b","#b27b83",
                       "#b28f7b","#8f7bb2",
                       "#538e68","#FFFFFF"),NYSS@data$GRADE_ORGA)
  # is that the last object into a JS visualization is on top.
  # So this means as you stack polygons, lines, and other objects 
  # and data onto a map, it is possible to BLOCK the visibility/functionality of objects below it.
  

observe({
  if (is.null(input$Needs)|is.null(input$SchoolType))
    return()
  Ne <- input$Needs
  ST <- input$SchoolType

# isolate({
  leafletProxy("map",data = filteredData()) %>%
  addCircles(#layerId = "cir",
             # lat = filteredData()$Latitude,lng = filteredData()$Longitude,
             lat = ~Latitude,lng = ~Longitude,
             color= pal(~GRADE_ORGA),
             opacity = 1,
             weight = 1, stroke = 5,radius = 200,
             label = labelsPTS,
             popup = paste("Sch Name ", ~LEGAL_NAME, "<br>",
                           "Sch Type ", ~GRADE_ORGA, "<br>",
                           "Acc: ", ~ACC_CODE, "<br>",
                           "Address: ", paste0(~PHYSADDR_1," ",~COUNTY_DES), "<br>",
                           "Since: ", ~ESTABLISHE, "<br>",
                           "Big Cheese:", paste0(~CEO_FNAME," ",~CEO_LNAME),"<br>",
                           "Phone: ", ~CEO_PHONEN,"<br>",
                           "Email: ",~CEO_EMAIL, "<br>",
                           "Website: ",dplyr::if_else(is.na(~WEBSITE),"",
                                                      paste0("<a href = ",~WEBSITE,">School Website</a> "))))
  # }) #END ISOLATE
}) #END REACT

# observe({
#   proxy <- leafletProxy("map", data = filteredData() )
# 
#   proxy %>% clearControls()
# 
#   if (input$legend) {
#   proxy %>% addLegend(position = "bottomright",
#                         pal = pal, values = NYSS@data$GRADE_ORGA
#     )
# }
# }) #END OBSERVE

}

shinyApp(ui = ui, server = server)

