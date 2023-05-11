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
#d1 <- as.data.frame(load("SA_Thresh_FINAL2.RData", envir = globalenv()))
load("SA_Thresh_Final2.RData")
#data_frame <- data_frame
#data_frame <- data_frame
#fix problem with new datset
#d1 <- as.data.frame(load("SA_Thresh_FINAL3.RData", envir = globalenv()))
nhd <- st_read("NHD_Ca.geojson")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                  useShinyjs(),
                  tabsetPanel(
                    
                    tabPanel("Project Description",
                             tags$div(
                               h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
                               hr(tags$sub("4/24/23 Version: 1")),
                               p("Salinization is a growing threat to aquatic life in streams in the Santa Ana region by disrupting organisms’ physiological processes and increasing sensitivity to other contaminants. Plans to increase wastewater recycling, as well as continued reliance on water diverted from the Colorado River, are likely to increase ionic concentrations in streams with urban or agricultural land use."),
                               
                               p("Because stream salinity can vary due to natural factors, such as geology and climate, we developed models to predict natural background levels of ionic parameters. Because these models are dynamic, they can reflect changes in natural levels associated with variation in season and annual precipitation. Application of the models to streams in the Santa Ana watershed show considerable spatial variation, with the lowest salinity levels typically being observed in the high elevation headwaters of the Santa Ana, San Bernardino, San Jacinto, and San Gabriel mountains. Deviations from modeled expectations can be used to identify streams where salinization has occurred. We found evidence of widespread salinization areas with urban or agricultural land use, such as the lower elevations of coastal Orange County and the Inland Empire."),
                               
                               p("Biological response models based on biointegrity indices (specifically the California Stream Condition Index [CSCI] for benthic invertebrates and the Algal Stream Condition Indices [ASCIs]) showed that elevated ionic concentrations were associated with poor biological conditions. These models can support the identification of thresholds for ionic parameters that provide a high level of probability of protecting stream biointegrity. We identified reach-specific thresholds for all studied parameters (except Magnesium). These thresholds could be adjusted to account for season, as well as for drought or years with high levels of precipitation. These thresholds can be used to assess stressors on sites, prioritize sites for restoration or additional investigation, or in causal assessments."), 
                             )
                    ),
                    
                    
                    tabPanel("Visualize Data",
                             tags$div(
                               h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
                               hr(tags$sub("4/24/23 Version: 1")),
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
                             ),
                    ),
                    
      tabPanel("Query Data",
               tags$div(
                 h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
               ),
                fluidRow(
                  column(
                   12,
                   fluidRow(
                     column(
                       2,
                        selectInput("Analyte2",
                                    "Analyte:",
                                    c("Select",
                                      unique(as.character(data_frame$Analyte))),
                                    multiple = TRUE)
                 ),
                 column(2,
                        selectInput("Index2",
                                    "Index:",
                                    c("Select",
                                      unique(as.character(data_frame$Index))),
                                    multiple = TRUE)
                 ),
                 column(2,
                        selectInput("Biointegrity_goal2",
                                    "Biointegrity Goal:",
                                    c("Select",
                                      unique(as.character(data_frame$Biointegrity_goal))),
                                    multiple = TRUE)
                 ),
                 column(2,
                        selectInput("Probability2",
                                    "Probability:",
                                    c("Select",
                                      unique(as.character(data_frame$Probability))),
                                    multiple = TRUE)
                 ),
                 column(2,
                        selectInput("Climatic_condition2",
                                    "Climatic Condition:",
                                    c("Select",
                                      unique(as.character(data_frame$Climatic_condition))),
                                    multiple = TRUE)
                 ),
                 column(2,
                        selectInput("Season2",
                                    "Season:",
                                    c("Select",
                                      unique(as.character(data_frame$Season))),
                                    multiple = TRUE)
                 ),
                 fluidRow(
                 column(2,
                   #offset = 2,
                   #align = "right",
                   actionButton(inputId = "filter2", label = "Filter Data"),
                   )
                )
              )
            ),
                        #Create a new rows for the map & table.
                        fluidRow(DT::dataTableOutput("table2")
      ),
    ),
  ),             
      tabPanel("Download Datasets",
               tags$div(
                     h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
                     p("Download full datasets here"),
                     p("Coming Soon"),
                     tags$a(href="https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/Part3_intion_thresholds_shinyapp_summary_COMID.csv", "Thresholds Dataset"),
                             )
                    ),
),
)

# Define server logic 
#for the visualization tab
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
  
  # render visualization table based on filter
  output$table <- DT::renderDataTable({DT::datatable(
    getData(),
    # options = list(
    #   scrollX =TRUE,
    #   scrollY = "175px",
    #   dom = 'lrtip')
  )
  }) 
  
  #for the download tab  
  getData2 <- eventReactive(eventExpr = input$filter2, valueExpr = {
  #getData2 <- eventReactive(input$filter2, {
    if (!is.null(input$Analyte2) && input$Analyte2 != "Select") {
      data_frame <- data_frame[data_frame$Analyte %in% input$Analyte2,]
    }
    if (!is.null(input$Index2) && input$Index2 != "Select") {
      data_frame <- data_frame[data_frame$Index %in% input$Index2,]
    }
    if (!is.null(input$Biointegrity_goal2) && input$Biointegrity_goal2 != "Select") {
      data_frame <- data_frame[data_frame$Biointegrity_goal %in% input$Biointegrity_goal2,]
    }
    if (!is.null(input$Probability_goal2) && input$Probability_goal2 != "Select") {
      data_frame <- data_frame[data_frame$Probability_goal %in% input$Probability_goal2,]
    }
    if (!is.null(input$Climatic_condition2) && input$Climatic_condition2 != "Select") {
      data_frame <- data_frame[data_frame$Climatic_condition %in% input$Climatic_condition2,]
    }
    if (!is.null(input$Season2) && input$Season2 != "Select") {
      data_frame <- data_frame[data_frame$Season %in% input$Season2,]
    }
  })
  # render download table based on filter
  output$table2 <- DT::renderDataTable({DT::datatable(
    getData2(),
    options = list(
      scrollX =TRUE,
      scrollY = "500px",
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
