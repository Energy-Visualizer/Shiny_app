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


pinboard_folder <- file.path("C:/Users/Keren/Documents/Shiny_app/Data")
pinboard <- pins::board_folder(pinboard_folder, versioned = TRUE)
agg_eta_pfu_df <- pins::pin_read(board = pinboard, name = "agg_eta_pfu", version = "20230619T051304Z-f653c")
psut_df <- pins::pin_read(board = pinboard, name = "psut", version = "20230915T185731Z-c48a0")



page1 <- tabPanel(
  title = "Global Map",
  titlePanel("Interactive Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country", 
                  choices = psut_df["Country"],
                  selected = psut_df["Country"]),
      selectInput("year", "Select a Year", 
                  choices = psut_df["Year"], selected = psut_df["Year"])
    
    ),
    mainPanel(
      leafletOutput("map"),
      htmlOutput("sankeyPlot2", inline = FALSE)
    )
  )
)



page2 <- tabPanel(
  title ="In-depth View",
  tags$h1("Efficiency Graph"),
  # Sidebar with a slider input for number of bins
    # Show a plot of the generated distribution
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(3, h4("Energy Type"), radioButtons("energy_type", "", choices = c("E", "X"))),
        column(3, h4("Product Aggregation"), radioButtons("product_aggregation", "", choices = c("Specified", "Despecified", "Grouped"))),
        column(3, h4("Industry Aggregation"), radioButtons("industry_aggregation", "", choices = c("Specified", "Despecified", "Grouped"))),
        column(3, h4("IEAMW"), radioButtons("ieamw", "", choices = c("IEA", "MW", "Both"))),
        column(3, h4("Gross/Net"), radioButtons("gross_net", "", choices = c("Gross", "Net")))
    )
    ),
    mainPanel(
      plotOutput("Plot"),
      tags$h1("Sankey Diagram"), 
      htmlOutput("sankeyPlot", inline = FALSE),
      textOutput("gggg")
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
  
  
  eff1 <- reactive({
    effi1 <- input$energy_type
    return(effi1)
  })
  
  eff2 <- reactive({
    effi2 <- input$product_aggregation
    return(effi2)
  })
  
  eff3 <- reactive({
    effi3 <- input$industry_aggregation
    return(effi3)
  })
  
  eff4 <- reactive({
    effi4 <- input$ieamw
    return(effi4)
  })
  
  eff5 <- reactive({
    effi5 <- input$gross_net
    return(effi5)
  })
  
  eff6 <- reactive({
    effi6 <- input$country
    return(effi6)
  })
  
  eff7 <- reactive({
    agg_eta_pfu_df1 <- agg_eta_pfu_df |> 
      dplyr::filter(Country == input$country, IEAMW == input$ieamw, Energy.type == input$energy_type, 
                    Product.aggregation == input$product_aggregation, 
                    Industry.aggregation == input$industry_aggregation, 
                    GrossNet == input$gross_net)
    return(agg_eta_pfu_df1)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2)  # Set initial view
  })
  
  output$Plot <- renderPlot({
    
    # agg_eta_pfu_df |> 
    #   dplyr::filter(Country == eff6(), IEAMW == eff4(), Energy.type == eff1(), 
    #                 Product.aggregation == eff2(), 
    #                 Industry.aggregation == eff3(), 
    #                 GrossNet == eff5()) 
    eff7() |> ggplot2::ggplot(mapping = ggplot2::aes(x = Year, y = eta_pu)) + ggplot2::geom_line()
  })
  
  output$gggg <- renderText({
    c(eff6(),eff1(),eff2(),eff3(),eff4(),eff5())
  })

  output$sankeyPlot <- renderUI({Recca::make_sankey(R = data1(),
                                                    U = data2(), 
                                                    V = data3(), 
                                                    Y = data4())})
  output$sankeyPlot2 <- renderUI({Recca::make_sankey(R = data1(),
                                                    U = data2(), 
                                                    V = data3(), 
                                                    Y = data4())})
}

# Run the application 
shinyApp( ui,  server)

