#Shiny APP for the RB8 Ion project
library(base)
library(shiny)
library(shinyjs)
library(sf)
library(DT)
library(leaflet)
library(readr)
library(dplyr)
library(htmltools)
library(shinythemes)

# Load data (look for download link in ReadMe file if needed)
d1 <- as.data.frame(load("SA_Thresh_FINAL2.RData", envir = globalenv()))
nhd <- st_read("NHD_Ca.geojson")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
  fluidPage(
    useShinyjs(),
    tags$div(
    h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
    hr(),
    p("This dashboard is intended to help support waterboard staff identify thresholds for ionic parameters based on biological response models. 
    Users should select one item from each drop-down menu, and then push the filter button. A map showing average thresholds for each segment in the Santa Ana watershed will be rendered, along with a table containing the plotted data. 
      For each segment, we report n (the number of months fitting the selected criteria), the minimum, maximum, average and standard deviation of E (i.e., the predicted natural background level of the parameter in the stream segment), and threshold. 
      The download button will download a CSV file of the resulting rows, which may be joined to an NHD+ shapefile based on the unique stream segment identifier (COMID). 
      For additional information, contact Raphael Mazor (raphaelm@sccwrp.org).", align = "left"),
    ),
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),#makes sure the dropdown menu is on top of the map element
# Create a new Row in the UI for selectInputs
    fluidRow(
      column(2,
             selectInput("Analyte",
                         "Analyte:",
                         c("Select",
                           unique(as.character(data_frame$Analyte))))
      ),
      column(2,
             selectInput("Index",
                         "Index:",
                         c("Select",
                           unique(as.character(data_frame$Index))))
      ),
      column(2,
             selectInput("Biointegrity_goal",
                         "Biointegrity Goal:",
                         c("Select",
                           unique(as.character(data_frame$Biointegrity_goal))))
      ),
      column(2,
             selectInput("Probability",
                         "Probability:",
                         c("Select",
                           unique(as.character(data_frame$Probability))))
      ),
      column(2,
             selectInput("Climatic_condition",
                         "Climatic Condition:",
                         c("Select",
                           unique(as.character(data_frame$Climatic_condition))))
      ),
      column(2,
             selectInput("Season",
                         "Season:",
                         c("Select",
                           unique(as.character(data_frame$Season))))
      ),
     actionButton(inputId = "filter", label = "Filter Data"),
     
# Create a new rows for the map & table.
    fluidRow(
      leafletOutput(outputId = "map"),
      DT::dataTableOutput("table"))
  ),
# Button to download data
  fluidRow(
   column(3,
        shinyjs::hidden(
          downloadButton("downloadData", "Download")))
),),
)

# Define server logic 
server <- function(input, output) {
  getData <- eventReactive(eventExpr = input$filter, valueExpr = {
    if (input$Analyte != "Select") {
      data_frame <- data_frame[data_frame$Analyte == input$Analyte,]
    }
    if (input$Index != "Select") {
      data_frame <- data_frame[data_frame$Index == input$Index,]
    }
    if (input$Biointegrity_goal != "Select") {
      data_frame <- data_frame[data_frame$Biointegrity_goal == input$Biointegrity_goal,]
    }
    if (input$Probability != "Select") {
      data_frame <- data_frame[data_frame$Probability == input$Probability,]
    }
    if (input$Climatic_condition != "Select") {
      data_frame <- data_frame[data_frame$Climatic_condition == input$Climatic_condition,]
    }
    if (input$Season != "Select") {
      data_frame <- data_frame[data_frame$Season == input$Season,]
    }
  })

# render table based on filter
  output$table <- DT::renderDataTable({DT::datatable(
    getData(),
    options = list(
      scrollX =TRUE,
      scrollY = "175px",
      dom = 'lrtip')
    )
  })
  
#render map based on filter
  output$map <- renderLeaflet({
    od <- getData()#make a dataframe from filtered data for joining
    df2 <- dplyr::inner_join(nhd, od, by = "COMID", copy = TRUE) #join filtered data to the spatial data via COMID
    binpal <- colorNumeric("magma", df2$Threshold_avg, reverse = TRUE) #set symbology parameters
    leaflet(df2) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$Stamen.TonerLabels,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addPolylines(color = ~binpal(Threshold_avg), 
                   weight = 2,
                   opacity = 100,
                   popup = ~paste("<b>COMID:</b>", COMID,
                                  "<br><b>GNIS Name:</b>", GNIS_NAME.x,
                                  "<br><b>E Average:</b>", E_avg,
                                  "<br><b>Threshold Average:</b>",Threshold_avg)) %>%
      addLegend("bottomright", pal = binpal, values = ~Threshold_avg,
                title = "Threshold <br> Average",
                opacity = 1,
                bins = 4,
      )
  })

#Downloadable csv of filtered dataset
    shinyjs::onclick("filter",
                      shinyjs::show(id = "downloadData"))
    output$downloadData <- downloadHandler(
     filename = function() {
       paste("RB8_Threshold_Data", ".csv", sep = "")
     },
     content = function(file) {
       write.csv(getData(), file, row.names = FALSE)
     }
   )
}
# Run the application 
shinyApp(ui = ui, server = server)
