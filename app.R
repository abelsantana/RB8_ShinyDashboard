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
load("sa_thresh_final2.RData")
nhd <- st_read("NHD_Ca.geojson")

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                  useShinyjs(),
                  tabsetPanel(
                    tabPanel("Project Description",
                             tags$div(
                               h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
                               hr(tags$sub("6/13/23 Version: 2")),
                               p("This dashboard is intended to help managers interpret models and identify aquatic life thresholds for ionic parameters for wadeable streams in the Santa Ana watershed. "),
                               
                               p("Salinization is a growing threat to aquatic life in streams in the Santa Ana region by disrupting organisms’ physiological processes and increasing sensitivity to other contaminants. Plans to increase wastewater recycling, as well as continued reliance on water diverted from the Colorado River, are likely to increase ionic concentrations in streams with urban or agricultural land use."),
                              p("Because stream salinity can vary due to natural factors, such as geology and climate, we developed models to predict natural background levels of ionic parameters. Because these models are dynamic, they can reflect changes in natural levels associated with variation in season and annual precipitation. Application of the models to streams in the Santa Ana watershed show considerable spatial variation, with the lowest salinity levels typically being observed in the high elevation headwaters of the Santa Ana, San Bernardino, San Jacinto, and San Gabriel mountains. Deviations from modeled expectations can be used to identify streams where salinization has occurred. We found evidence of widespread salinization areas with urban or agricultural land use, such as the lower elevations of coastal Orange County and the Inland Empire"),
                              p("Biological response models based on biointegrity indices (specifically the California Stream Condition Index [CSCI] for benthic invertebrates and the Algal Stream Condition Indices [ASCIs]) showed that elevated ionic concentrations were associated with poor biological conditions. These models can support the identification of thresholds for ionic parameters that provide a high level of probability of protecting stream biointegrity. We identified reach-specific thresholds for all studied parameters (except Magnesium). These thresholds could be adjusted to account for season, as well as for drought or years with high levels of precipitation. These thresholds can be used to assess stressors on sites, prioritize sites for restoration or additional investigation, or in causal assessments."),
                               
                               p("Details about this study are provided in a report to the Regional Water Quality Control Board, Santa Ana Region: Assessing the Influence of Salinization on Aquatic Life in Santa Ana Region Wadeable Streams (SCCWRP Technical Report #1324). For additional information, contact Raphael Mazor (raphaelm@sccwrp.org) or Jan Walker (janw@sccwrp.org) 
"), 
                              p("Expected updates coming July 31, 2023"),
                             )
                    ),
                    
                    
                    tabPanel("Visualize Data",
                             tags$div(
                               tags$h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
                               tags$p("This dashboard is intended to help support waterboard staff identify thresholds for ionic parameters based on biological response models. Users should select one item from each drop-down menu, and then push the filter button. A map showing average thresholds for each segment in the Santa Ana watershed will be rendered, along with a table containing the plotted data."),
                               
                               tags$h4("Parameters:", style = "text-align: left;"),
                               tags$ul(
                                 tags$li(
                                   "Analyte",
                                   tags$ul(
                                     tags$li("Ions: chloride, sulfate, sodium, calcium, and magnesium"),
                                     tags$li("Integrated measures: TDS, hardness, alkalinity, and specific conductivity")
                                   )
                                 ),
                                 tags$li(
                                   "Biointegrity index",
                                   tags$ul(
                                     tags$li("California Stream Condition Index: CSCI for benthic macroinvertebrates"),
                                     tags$li("Algal Stream Condition Index",
                                             tags$ul(
                                               tags$li("ASCI_D for diatoms"),
                                               tags$li("ASCI_H for diatoms and soft-bodied algae")
                                             )
                                     )
                                   )
                                 ),
                                 tags$li(
                                   "Biointegrity goal used to identify intact or altered condition",
                                   tags$ul(
                                     tags$li("Ref30 – 30th percentile"),
                                     tags$li("Ref10 – 10th percentile"),
                                     tags$li("Ref01 – 1st percentile")
                                   )
                                 ),
                                 tags$li(
                                   "Probability of attaining the biointegrity goal",
                                   tags$ul(
                                     tags$li("0.8"),
                                     tags$li("0.9"),
                                     tags$li("0.95")
                                   )
                                 ),
                                 tags$li(
                                   "Climatic condition calculated by categorizing the years 2001-2019 into thirds based on annual precipitation",
                                   tags$ul(
                                     tags$li("All conditions"),
                                     tags$li("Dry"),
                                     tags$li("Normal"),
                                     tags$li("Wet")
                                   )
                                 ),
                                 tags$li(
                                   "Season calculated as whether the sample was measured in months between April and September or not",
                                   tags$ul(
                                     tags$li("All months"),
                                     tags$li("April-Sept"),
                                     tags$li("Oct-March")
                                   )
                                 ),
                               ),
                               
                               tags$p(
                                 "For each segment, we report n (the number of months fitting the selected criteria), the minimum, maximum, average and standard deviation of E (i.e., the predicted natural background level of the parameter in the stream segment), and threshold. The download button will download a CSV file of the resulting rows, which may be joined to an NHD+ shapefile based on the unique stream segment identifier (COMID). Users interested in seeing results for individual flow-lines may click on the map to retrieve mean threshold and expected values.",style = "text-align: left;"),
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
                                                  "Season",
                                                  c("Select",
                                                    unique(as.character(data_frame$Season))))
                               ),
                               column(2,
                                      selectInput("result",
                                                  "Result",
                                                  choices = c("E_min", "E_max", "E_avg", "E_SD", "Threshold_min", "Threshold_max", "Threshold_avg", "Threshold_SD"))
                               ),
                                 #column(3,
                                        #actionButton(inputId = "filter", label = "Filter Data")),
                               
                               # Create a new rows for the map & table.
                               fluidRow(
                                 column(width = 12,
                                 div(actionButton(inputId = "filter", label = "Filter Data"), style = "padding:15px"),      
                                 div(leafletOutput(outputId = "map"), style = "padding:20px;"),
                                 div(DT::dataTableOutput("table"), style = "padding: 10px;"))
                             )
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
                 p("This query tab is intended for users to query the data for more specific data download. This is the same data used to create the maps in the previous tab. The query tool will allow you to make multiple selections for each parameter."),
                 
               ),
                fluidRow(
                  column(
                   12,
                   fluidRow(
                     column(2,
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
                   actionButton(inputId = "filter2", label = "Filter Data"),
                   )
                )
              )
            ),
          #Create a new rows for the table.
          fluidRow(
            column(
                   DT::dataTableOutput("table2"), width = 12)
      )
    ),
    # Button to download data
    fluidRow(
      column(3,
             shinyjs::hidden(
               downloadButton("downloadData2", "Download")))
    ),
  ),             
      tabPanel("Download Datasets",
               tags$style(
                 HTML(
                   "
      .section-title {
        font-size: 20px;
        font-weight: bold;
        margin-top: 10px;
      }

      .subsection-title {
        font-size: 18px;
        font-weight: bold;
        margin-top: 5px;
      }

      .data-description {
        font-size: 16px;
        margin-top: 5px;
        text-decoration: underline;
      }

      .data-link {
        margin-top: 5px;
      }

      .data-dictionary {
        margin-top: 5px;
      }
      "
                 )
               ),
               tags$div(
                 h1("Salinization thresholds for the Santa Ana Watershed", align = "center"),
                 tags$p(
                   "We provide the full datasets used for each analysis in our report to the Regional Water Quality Control Board, Santa Ana Region: Assessing the Influence of Salinization on Aquatic Life in Santa Ana Region Wadeable Streams (SCCWRP Technical Report #1324). Each dataset corresponds to a section of the report."
                 ),
                 #tags$a(href = "link_to_report", "Download Report"),
                 p("Report Coming soon"),
                 tags$div(
                   class = "section-title",
                   "PART 1"
                 ),
                 tags$div(
                   class = "subsection-title",
                   "Inputs"
                 ),
                 tags$div(
                   class = "data-description",
                     "Dynamic climate data"),
                 
                 tags$p("1-, 2-, 3-, 6-, and 12-month antecedent precipitation totals and mean temperatures for every COMID in California for the years 2001 to 2019. These data were used as predictors in models of natural background levels of ionic parameters."),
                 #tags$a(href = "link_to_report", "Dynamic climate data"),
                 p("Data coming soon"),
                   tags$div(
                     class = "data-description",
                     "Observed chemistry data"),
                 
                     tags$p(  "Nation-wide ionic parameter data. These data were used to calibrate models of natural background levels of ionic parameters."
                       ),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/California_Chemistry_Obs_RefNoRef.xlsx", "Observed chemistry data"),
                 tags$div(
                   class = "data-description",
                   "StreamCat"),
                    tags$p("These data were used as predictors in models of natural background levels of ionic parameters. StreamCat is available from ",
                   tags$a(href = "https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset", "here"),
                   "."
                 ),
                 tags$div(
                   class = "subsection-title",
                   "Outputs"
                 ),
                 tags$div(
                   class = "data-description",
                   "California predictions"),
                 tags$p("Expected predictions for all California COMIDs for each analyte, one row per month from 2001 to 2019."),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/TDS.zip", "TDS, "),                 
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Sulfate.zip", "Sulfate, "),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Specific_Conductivity.zip", "Specific Conductivity, "),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Sodium.zip", "Sodium, "),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Magnesium.zip", "Magnesium, "),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Hardness.zip", "Hardness, "),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Chloride.zip", "Chloride, "),
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Alkalinity.zip", "Alkalinity"),
                 
                 tags$div(
                   class = "section-title",
                   "PART 2"),
                 tags$div(
                   class = "subsection-title",
                   "Inputs"),
                 tags$div(
                   class = "data-description",
                   "Biological data"),
                    tags$p("CSCI and ASCI scores for sites in California, and accompanying water quality data. These data were used to calibrate biological response models.",
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/Final_dataset_4-26-23-2.xlsx", "Download here"),
                 "."
                    ),
                 tags$div(
                   class = "subsection-title",
                   "Outputs"
                 ),
                 tags$div(
                   class = "data-description",
                   "Santa Ana Thresholds"),
                 tags$p("Summaries of thresholds (i.e., min, max, mean, and standard deviation) for every COMID in the Santa Ana basin under different climatic and seasonal conditions.",
                 tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/RB8IONApp/SA_thresholds_shinyapp_summary_COMID.zip", "Download here"),
                 "."
                 ),
                 tags$div(
                   class = "section-title",
                   "PART 3"),
                 tags$div(
                   class = "subsection-title",
                   "Outputs"
                 ),
                 tags$div(
                   class = "data-description",
                   "Integrated thresholds"),
                 tags$p("Thresholds for integrated parameters (i.e., TDS and specific conductivity) for use as proxies of individual ionic parameters (i.e., calcium, chloride, sulfate, and sodium). Thresholds are summarized (i.e., min, max, mean, and standard deviation) for every COMID in the Santa Ana basin under different climatic and seasonal conditions.",
                        tags$a(href = "https://ftp.sccwrp.org/pub/download/PROJECTS/SCCWRP_Bio/Part3_intion_thresholds_shinyapp_summary_COMID.zip", "Download here"),
                        "."
                 ),                 
                  )
                    ),
)
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
     options = list(
       scrollX =TRUE,
       scrollY = "175px",
       dom = 'lrtip')
  )
  }) 
  
  #for the download tab  
  getData2 <- eventReactive(eventExpr = input$filter2, valueExpr = {
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
      scrollX = TRUE,
      scrollY = TRUE,
      dom = 'lrtip')
  )
  })
  
  #render map based on filter
  output$map <- renderLeaflet({
    od <- getData()#make a dataframe from filtered data for joining
    df2 <- dplyr::inner_join(nhd, od, by = "COMID", copy = TRUE) #join filtered data to the spatial data via COMID
    if (is.numeric(df2[[input$result]])) {
      binpal <- colorNumeric("magma", df2[[input$result]], reverse = TRUE)
    } else {
      binpal <- colorFactor("magma", df2[[input$result]])
    }
    #binpal <- colorNumeric("magma", df2[[input$result]], reverse = TRUE) #set symbology parameters
    leaflet(df2) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addProviderTiles(providers$Stamen.TonerLabels,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addPolylines(color = ~binpal(df2[[input$result]]), 
                   weight = 2,
                   opacity = 100,
                   popup = ~paste("<b>COMID:</b>", COMID,
                                  "<br><b>GNIS Name:</b>", GNIS_NAME.x,
                                  "<br>", input$result, ":", df2[[input$result]])) %>%
      addLegend("bottomright", pal = binpal, values = ~df2[[input$result]],
                title = input$result,
                opacity = 1,
                bins = 4,
      )
  })
  
  #Downloadable csv of filtered dataset
  shinyjs::onclick("filter",
                   shinyjs::show(id = "downloadData"))
  output$downloadData <- downloadHandler(
    filename = "RB8_Visualize_Data.csv",
    content = function(file) {
      write.csv(getData(), file, row.names = FALSE)
    }
  )
 
  shinyjs::onclick("filter2",
                   shinyjs::show(id = "downloadData2"))
  output$downloadData2 <- downloadHandler(
    filename = "RB8_Query_Data.csv",
    content = function(file) {
     write.csv(getData2(), file, row.names = FALSE)
    }
 )
}
# Run the application 
shinyApp(ui = ui, server = server)
