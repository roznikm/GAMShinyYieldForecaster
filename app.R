if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")

counties_df <- st_read('shpfiles/counties_df.shp')
counties_df <- st_transform(counties_df,'+proj=longlat +datum=WGS84')
national <- read_csv('nationalForecast.csv')

pal <- colorNumeric("plasma", domain = counties_df$pred)

ui <- navbarPage(strong("GAM Corn Yield Forecaster"), id="nav",
             tabPanel("County-level", icon = icon("map-marked"), 
                      div(class = 'outer',
                      tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css")
                      ),
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            leafletOutput("mymap",  width="100%", height="100%"),
                            absolutePanel(id='controls', class = "panel panel-default", 
                                          fixed=TRUE, draggable=TRUE, 
                                          top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          h2("August corn yield forecasts and final yields"),
                                          radioButtons("mapType", "Actual yield or Forecast yield",
                                                       c("Forecast"="pred", "Actual"="Value")), 
                                          selectInput("year", "Choose a year", 
                                                      choice =  sort(unique(counties_df$year), decreasing = TRUE),
                                                      selected=2019),
                                          plotOutput("plot1", height = 200),
                                          plotOutput("plot2", height = 200)
                                          )
                          )
                      ),
             
             tabPanel("National-level", icon = icon("globe-americas"), 
                      h2("National forecast comparison"),
                      DT::dataTableOutput("mytable")
                      ),
             
             tabPanel("About", icon = icon("box-open"), 
                      fluidRow(
                        column(8, offset=2,
                               tags$div(class='header',
                                        tags$h2("GAM Corn Yield Forecaster"),
                                          tags$p("GAM Corn Yield Forecaster is a machine-learning 
                                          based crop yield forecasting platform. 
                                          The model uses satellite image data and weather data to
                                          forecast corn yields at the county-level, and national-level.
                                          Crop yields are forecasted twice per month."),
                                        tags$h2("Forecasting methodology"),
                                          tags$p("The model uses spline fit flexible functional forms on discrete
                                          weather and satellite derived variables. This helps relate the variables
                                          to crop yield in a flexible way. The methodology can be extended to
                                          other crops beyond corn such as soybeans, wheat, canola, and others."),
                                        tags$h2("About the author"), 
                                          tags$p(
                                          HTML(paste0("Mitchell Roznik is a PhD candidate from the University of Manitoba, Canada. 
                                          Mitch conducts research in crop insurance and agribusiness. His expertise is in
                                          applying geospatial data to solve problems in agriculture. Feel free to contact
                                          him if you have any questions at mroznik1@gmail.com. The code
                                          is available on ", tags$a(href="https://github.com/roznikm/GAMShinyYieldForecaster", "github.")))
                                        )
                               )
                               
                          )
                      )
                      
              )
       )
                
  


server <- function(input, output, session){
  df_map <- reactive({
    counties_df[(counties_df$year == input$year), ]
  })
  
  labels <- reactive({
    if(input$mapType == 'pred') {
      sprintf(
        "<strong>%s, %s</strong><br/>%g bu/acre",
        df_map()$NAME,str_to_title(df_map()$State), df_map()$pred
      ) %>% lapply(htmltools::HTML) 
    } else {
      sprintf(
        "<strong>%s, %s</strong><br/>%g bu/acre",
        df_map()$NAME,str_to_title(df_map()$State), df_map()$Value
      ) %>% lapply(htmltools::HTML) 
    }
  })
  
  output$mymap <- renderLeaflet({
    df_map_tibble <- as_tibble(df_map())
    df_map_vec<- pull(df_map_tibble, input$mapType)
    leaflet() %>% 
      setView(lng = -94.0589, lat = 39.3601, zoom = 5) %>% 
        addTiles() %>%
          addPolygons(data=df_map(),
                      fillColor = ~pal(df_map_vec),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = labels(),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
          ) %>%   addLegend("bottomright", pal = pal, 
                            values = df_map_vec,
                            title='Corn Yield')
  })
  
  output$plot1 <- renderPlot({
    df_map_tibble <- as_tibble(df_map())
    ggplot(df_map_tibble, aes(x=pred)) + geom_density( alpha=0.05) + theme_bw() +
      geom_vline(xintercept = mean(df_map_tibble$pred), linetype='dashed') +
      xlab('Forecasted Corn Yield') + ylab('Density')
  })

  output$plot2 <- renderPlot({
    df_map_tibble <- as_tibble(df_map())
    ggplot(df_map_tibble, aes(x=Value)) + geom_density( alpha=0.05) + theme_bw() +
      geom_vline(xintercept = mean(df_map_tibble$pred), linetype='dashed') +
      xlab('Actual Corn Yield') + ylab('Density')
  })

  output$mytable = DT::renderDataTable({
    national
  })
  
}

shinyApp(ui = ui, server = server)
