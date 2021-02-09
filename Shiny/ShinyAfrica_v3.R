#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://urldefense.com/v3/__http://shiny.rstudio.com/__;!!Ls64Rlj6!l3Weo_eWNLkyVEz_UZrMWm3SdUmZ0-PT0am1rvBr1DlQkT7e4zrrdOzjPqyILhzG83uYVdYt$
#

library(shiny)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(geojsonio)
library(leaftime)
library(sf)

if (!require("pacman")) {
  install.packages("pacman")
}
pkgs = c("dplyr", "sp", "sf", "ggplot2", "plotly", "tidyr") # package names
pacman::p_load(pkgs, character.only = T)

setwd(
  "C:/Users/BCIcomputer01/Box Sync/Hershey_2020/COVID Modeling/HHH4/stsmodel-africa"
)
tmp = readRDS("tempdata.rds")
# saveRDS(object = md, file.path(outputdir,"md.rds"))
# saveRDS(object = pf, file.path(outputdir,"pf.rds"))
# md=readRDS(file.path(outputdir,"md.rds"))
# pf=readRDS(file.path(outputdir,"pf.rds"))
pf = tmp[[1]]
md = tmp[[2]]





#creating colorsets for polygons
#these color sets will be called  while creating the maps in the "if then" statement

date_vector <- as.Date(unique(pf$time))

#right now it is selecting cases on the final date but want to use cumulative cases
tmp2 <- pf %>%
  select(observed) %>%
  filter(as.Date(levels(pf$time)[pf$time]) %in% date_vector[length(date_vector)])
md$observed <- tmp2$observed



date_exp <- rep(date_vector, each = length(md$name))
# mdmini <- md %>%
#   select(name, geom)

afmap <- md
# for (i in 2:length(date_vector)) {
#   afmap <- rbind(afmap, mdmini)
# }
# afmap$start <- date_exp
# afmap$end <- afmap$start + 1


pftrim = pf %>%
  filter(as.Date(levels(pf$time)[pf$time]) %in% date_vector)


# inputlist = c("observed","SSA","HDI_2018","Pop2020")
# inputname = c("Cases","SSA","HDI","Population")
# inputmapping <- matrix(c(inputlist,inputname,paste0(inputlist,"_color")),length(inputlist),3)
# 
# 
# pal <- colorQuantile(palette = "RdYlBu", n = 10,
#                      domain = md$observed, reverse = T)
# #Case colors
# colorindex = floor(99 * log(md$observed + 1) / log(max(md$observed))) +
#   1
# # ci = t(matrix(
# #   colorindex,
# #   nrow = length(date_vector),
# #   ncol = length(md$name)
# # ))
# ci = as.vector(colorindex)
# afmap$observed_color <- substr(topo.colors(100), 1, 7)[ci]
# 
# #Subsaharan Africa colors
# colorindex = floor(md$SSA) + 1
# ci = as.vector(colorindex)
# afmap$SSA_color <- substr(topo.colors(2), 1, 7)[ci]
# 
# #Population colors
# colorindex = floor(99 * md$Pop2020 / max(md$Pop2020)) + 1
# ci = as.vector(colorindex)
# afmap$Pop2020_color <- 
# 
# #HDI_2018 colors
# colorindex = floor(99 * md$HDI_2018) + 1
# ci = as.vector(colorindex)
# afmap$HDI_2018_color  <- colorQuantile(palette = "RdYlBu", n = 10,
#                                              domain = md$HD_2018, reverse = T)


afmap_geo <- geojson_json(afmap, geometry = "polygon")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Model Inputs"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "layer",
        "Layer",
        c(
          "Cases" = "observed",
          "SSA" = "SSA",
          "Population" = "Pop2020",
          "HDI in 2018" = "HDI_2018"
        ),
        selected = "Pop2020"
      ),
      checkboxGroupInput("countries",
                         "Countries",
                         md$name, selected = "Algeria"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(fluidRow(
      column(11, offset = 1,
             leafletOutput("mymap")),
      
      
      fluidRow(column(11, offset = 1,
                      plotOutput("timeseries")))
    ),
    # tabsetPanel(
    #   tabPanel("Time Series", plotOutput("timeseries", width = "100%", height="300px")
    )
  ))
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
      
      #In the call to output$mymap, only define the things that will not change
      output$mymap <- renderLeaflet({
        # browser()
        
        leaflet() %>%
          addProviderTiles("CartoDB") 
      })
       
      
      #This is called within the observer that changes the bounds of the map
      geom_vec <- reactive({
        if (is.null(input$countries)){
          st_coordinates(md$geom) 
        }
        else{
        st_coordinates(md$geom[is.element(md$name,input$countries)]) 
        }
      })
      
      #This is called within the observer that changes the layer
      color_map <- reactive({
        inputmapping[is.element(inputmapping[,1],input$layer),3]
      })
 
     
      #For changes in country checkbox, resize the map (WORKS!)
      observe({
        geom <- geom_vec()
        leafletProxy("mymap") %>%
          flyToBounds(
                   lat1 = min(geom[, 2]),
                    lng1 = min(geom[, 1]),
                   lat2 = max(geom[, 2]),
                    lng2 = max(geom[, 1])
          )
      })
      
      #For changes in layer, update the colors, labels, and legend (legend not implemented yet)
      observe({
        ##Eventually want to do this
        lidx <- which(colnames(md)==input$layer)
        
        browser()
        pal <- colorQuantile(palette = "RdYlBu", n = min(c(10,length(unique(md[[lidx]]))-1)),
                      domain = md[[lidx]], reverse = T)

        #lbl_text = label_map(input$layer)
      leafletProxy("mymap") %>%
          clearShapes() %>%
          addPolygons(
          data=md$geom,
          #color=afmap$SSA_color[1:46]
          weight=1,
          label = sprintf("<strong> %s </strong>
                  : %s",
                  as.character(md$name),
                  md[[lidx]]) %>%
            lapply(htmltools::HTML),
          fillColor=pal(md[[lidx]]) 
          )
      })
          

 ## Right now, the app does not incorporate the timeline option (which is implemented for cases below)

    #         L = leaflet() %>%
    #           #addTiles() %>%
    #           flyToBounds(
    #             lat1 = min(geom_vec[, 2]),
    #             lng1 = min(geom_vec[, 1]),
    #             lat2 = max(geom_vec[, 2]),
    #             lng2 = max(geom_vec[, 1])
    #           ) %>%
    #           addProviderTiles("CartoDB") %>%
    #           addTimeline(
    #             data = afmap_geo,
    #             timelineOpts = timelineOptions(
    #               styleOptions = htmlwidgets::JS(
    #                 "function(feature) {
    #                    return {
    # fillColor: feature.properties.Case_color,
    # weight: 1,
    # opacity: 1,
    # color: 'white',
    # fillOpacity: 0.7
    #                      };
    #                     }"
    #               )
    #             ),
    #             sliderOpts = sliderOptions(
    #               formatOutput = htmlwidgets::JS(
    #                 "function(date) {return new Date(date).toDateString()}"
    #               ),
    #               step = length(date_vector),
    #               duration = length(date_vector) * 100
    #             )
    #           )
    #       }

      
      
      #tm_tmp = unique(pftrim$time)
      
      output$timeseries <- renderPlot({
        ts_tmp <- pftrim %>%
          select(time, country, input$layer) %>% #selecting time, country, and columns that are input layers
          filter(pftrim$country %in% input$countries) #filtering to keep the countries for the ones that are selected for input countries
        ggplot(data = ts_tmp,
               mapping = aes(x = time, group = country)) +
          aes_string(y = input$layer) +
          geom_line() +
          theme(legend.position = "right") #legend not working
        
        
        # ggplot(data = ts_tmp, aes(x=time, y=input$layer)) +
        #   geom_area(fill="#69b3a2", alpha=0.5) +
        #   geom_line(color="#69b3a2") +
        #   ylab("Case Count")
        # if(input$countries=="Algeria")
        
      })
    }
    
    

    
    
    # Run the application
    shinyApp(ui = ui, server = server)
      