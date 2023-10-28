#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(networkD3)
library(Recca)
library(pins)
library(leaflet)
library(shinythemes)
library(networkD3)
library(leaflet)


pinboard_folder <- file.path("C:/Users/4951h/Desktop/Energy Visualizer/Energy/data")
pinboard <- pins::board_folder(pinboard_folder, versioned = TRUE)
agg_eta_pfu_df <- pins::pin_read(board = pinboard, name = "agg_eta_pfu", version = "20230619T051304Z-f653c")
psut_df <- pins::pin_read(board = pinboard, name = "psut", version = "20230618T131003Z-4c70f")


page1 <- tabPanel(
  title = "Page1",
  titlePanel("Page1"),
  "Created with R Shiny",
  br(),
  "2023 October",
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country", 
                  choices = psut_df["Country"],
                  selected = "WRLD"),
      selectInput("year", "Select a Year", 
                  choices = psut_df["Year"], selected = 2005)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)



page2 <- tabPanel(
  title ="Page2",
  titlePanel("Page2"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      htmlOutput("sankeyPlot", inline = FALSE),
      tableOutput("ago1971")
    )
  )
)

ui <- navbarPage(
  title = "Energy Visualizer",
  theme = shinytheme('sandstone'),
  page1,
  page2
)





# Define server logic required to draw a histogram
server <- function(input, output) {

  
  data1 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country, Year == input$year)
    R_ago_1971 <- ago1971$R[[1]] |> unlist() |> as.matrix()
    return(R_ago_1971)
  })
  
  data2 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country, Year == input$year)
    U_ago_1971 <- ago1971$U[[1]] |> unlist() |> as.matrix()
    return(U_ago_1971)
  })
  
  data3 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country, Year == input$year)

    V_ago_1971 <- ago1971$V[[1]] |> unlist() |> as.matrix()
    return(V_ago_1971)
  })
  
  data4 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country, Year == input$year)
    
    Y_ago_1971 <- ago1971$Y[[1]] |> unlist() |> as.matrix()
    
    return(Y_ago_1971)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2)  # Set initial view
  })
  
  output$Plot <- renderPlot({
    
    agg_eta_pfu_df |> 
      dplyr::filter(Country == "NGA", IEAMW == "Both", Energy.type == "E", 
                    Product.aggregation == "Specified", 
                    Industry.aggregation == "Specified", 
                    GrossNet == "Gross") |> 
      ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = eta_pu)) + 
      ggplot2::geom_line()
  })
  
  

  output$sankeyPlot <- renderUI({Recca::make_sankey(R = data1(),
                                                    U = data2(), 
                                                    V = data3(), 
                                                    Y = data4())})
  
}

# Run the application 
shinyApp( ui,  server)
