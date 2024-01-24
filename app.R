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
library(reshape2)


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
        column(width = 6, h4("Product Aggregation"), radioButtons("product_aggregation", "", choices = c("Specified", "Despecified", "Grouped"))),
        column(width = 6, h4("Industry Aggregation"), radioButtons("industry_aggregation", "", choices = c("Specified", "Despecified", "Grouped"))),
      )
    ),
    mainPanel(
      plotOutput("Plot"),
      tags$h1("Sankey Diagram"), 
      htmlOutput("sankeyPlot", inline = FALSE),
    )
  )
)
ago1971 <- psut_df |> dplyr::filter(Country == psut_df["Country"], Year == psut_df["Year"])
R_ago_1971 <- ago1971$R[[1]] |> unlist() |> as.matrix()
ago1971 <- psut_df |> dplyr::filter(Country == psut_df["Country"], Year == psut_df["Year"])
Y_ago_1971 <- ago1971$Y[[1]] |> unlist() |> as.matrix()

page3 <- tabPanel(
  title ="Slices",
  tags$h1("Sankey"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country2", "Select a Country", 
                  choices = psut_df["Country"],
                  selected = psut_df["Country"]),
      selectInput("year2", "Select a Year", 
                  choices = psut_df["Year"], selected = psut_df["Year"]),
      radioButtons("radio", "Select Category:",
                   choices = c("Final demand sector", "Resource sector", "Final demand energy carriers", "Resource energy carriers"),
                   selected = "Final demand sector"),
      selectizeInput("options", 
                     label = "Select Options:",
                     choices = c(colnames(Y_ago_1971),rownames(R_ago_1971),colnames(R_ago_1971),rownames(Y_ago_1971)),
                     multiple = TRUE)
    ),
    mainPanel(
      htmlOutput("sankeyPlot3", inline = FALSE),
      verbatimTextOutput("eff8_output")
    )
  )
)

ui <- navbarPage(
  title = "Energy Visualizer",
  theme = shinytheme('sandstone'),
  page1,
  page2,
  page3
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #matrices for regular sankeys
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
  
  #matrices for slices
  #New sankey made
  
  
  data5 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country2, Year == input$year2)
    R_ago_1971 <- ago1971$R[[1]] |> unlist() |> as.matrix()
    return(R_ago_1971)
  })
  
  data6 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country2, Year == input$year2)
    U_ago_1971 <- ago1971$U[[1]] |> unlist() |> as.matrix()
    return(U_ago_1971)
  })
  
  data7 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country2, Year == input$year2)
    V_ago_1971 <- ago1971$V[[1]] |> unlist() |> as.matrix()
    return(V_ago_1971)
  })
  
  data8 <- reactive({ 
    ago1971 <- psut_df |> dplyr::filter(Country == input$country2, Year == input$year2)
    Y_ago_1971 <- ago1971$Y[[1]] |> unlist() |> as.matrix()
    return(Y_ago_1971)
  })
  
  
  
  #Faceted efficiency graphs  
  eff7 <- reactive({
    agg_eta_pfu_df2 <- melt(agg_eta_pfu_df, id = c("Country", "Method", "Energy.type", "Year", "IEAMW", "Chopped.mat", "Chopped.var", "Product.aggregation", "Industry.aggregation", "GrossNet","EX.p", "EX.f", "EX.u")
    )
    
    agg_eta_pu_all_continents <- agg_eta_pfu_df2 |>
      dplyr::filter(Country == input$country,
                    Method == "PCM",
                    Year >= 1971,
                    IEAMW == "Both",
                    Chopped.mat == "None",
                    Chopped.var == "None",
                    Product.aggregation == input$product_aggregation,
                    Industry.aggregation == input$industry_aggregation,
                    GrossNet == "Gross"
      )
    return(agg_eta_pu_all_continents)
  })
  
  eff8 <- reactive({
  ago1971 <- psut_df |> dplyr::filter(Country == input$country2, Year == input$year2)
  R_ago_1971 <- ago1971$R[[1]] |> unlist() |> as.matrix()
  ago1971 <- psut_df |> dplyr::filter(Country == input$country2, Year == input$year2)
  Y_ago_1971 <- ago1971$Y[[1]] |> unlist() |> as.matrix()
  
  observe({
    updateSelectInput(session, "options",
                      choices = getOptions(input$radio))
  })
  
  getOptions <- function(category) {
    # Define logic to get options based on the selected category
    if (category == "Final demand sector") {
      return(unique(colnames(Y_ago_1971)))
    } else if (category == "Resource sector") {
      return(unique(rownames(R_ago_1971)))
    } else if (category == "Final demand energy carriers") {
        return(unique(rownames(Y_ago_1971)))
    } else {
      return(unique(colnames(R_ago_1971)))
    }
  }
  })
  
  #Interactive map view
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2)  # Set initial view
  })
  
  #Efficiency graph representation
  output$Plot <- renderPlot({
    eff7() |>
      ggplot2::ggplot(mapping = ggplot2::aes(x = Year,
                                             y = value),
                      colour = Country,
                      linetype = Country,
                      linewidth = Country) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = c(1980, 2000, 2020)) +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                  breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1),
                                  labels = scales::label_percent(suffix = "")) +
      ggplot2::labs(x = NULL,
                    y = expression("Energy and exergy efficiency ("*eta*") [%]"),
                    colour = NULL,
                    linetype = NULL,
                    linewidth = NULL) +
      ggplot2::facet_grid(rows = ggplot2::vars(variable),
                          cols = ggplot2::vars(Energy.type),
                          labeller = ggplot2::label_parsed) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(linewidth = 1)  # Adjust the size of the line segments in the legend only
        )
      )
  }) 
  
  #Sankey portrayal
  output$sankeyPlot <- renderUI({Recca::make_sankey(R = data1(),
                                                    U = data2(), 
                                                    V = data3(), 
                                                    Y = data4())})
  output$sankeyPlot2 <- renderUI({Recca::make_sankey(R = data1(),
                                                     U = data2(), 
                                                     V = data3(), 
                                                     Y = data4())})
  output$sankeyPlot3 <- renderUI({Recca::make_sankey(R = data5(),
                                                     U = data6(), 
                                                     V = data7(), 
                                                     Y = data8())})
  output$eff8_output <- renderPrint({
    eff8()
  })
  
}

# Run the application
shinyApp( ui,  server)
