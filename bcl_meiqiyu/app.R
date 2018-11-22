library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  # title
  titlePanel("BC Liquor Store prices"),
  
  # siderbar layout
  sidebarLayout( 
    sidebarPanel(
      
      br(),br(),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      checkboxGroupInput("typeInput", "Product type",
                  choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                  selected = "WINE"),
      uiOutput("countryOutput")
    ),


    
  # main panel layout
  
  mainPanel(
    img(src="bcliquor.jpg",width = "400", height="180"),
    br(),
    h3(textOutput("num")),
    br(),
    tabsetPanel(
        tabPanel("Plot",
                 plotOutput("hist")),
        tabPanel("Table",
                 radioButtons("sortInput","Arrange by:",
                              choices = c("Price:Low to High","Alcohol Content:Low to High",
                                          "Sweetness:Low to High"),selected = NULL),
                 DT::dataTableOutput("table"))
      )
    )
  )
)




server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    if(input$sortInput=="Price:Low to High") {
      bcl <- bcl %>% 
        arrange(Price)
    }
    
    if(input$sortInput=="Sweetness:Low to High") {
      bcl <- bcl %>% 
        arrange(Sweetness)
    }
    
    if(input$sortInput=="Alcohol Content:Low to High") {
      bcl <- bcl %>% 
        arrange(Alcohol_Content)
    }
      
    else 
      bcl
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type %in% input$typeInput,
             Country == input$countryInput)
    
    
})
  
  
  output$num<-renderText({
    num_options<-nrow(filtered())
    paste0("We found ", num_options, " options for you")
  })
  
  output$hist <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    
    filtered() %>% 
      ggplot(aes(x=Alcohol_Content))+
      geom_histogram(aes(fill=Type))+
      scale_x_continuous(breaks = seq(0, 100, 20),limits = c(0,100))+
      labs(x="Alcohol Content",
           y="Quantities of Available Products")
  })

  output$table <- renderDataTable({
    if(is.null(filtered())){
      return(NULL)
    }else{
      return(filtered())
    }
      
  })
}
  


shinyApp(ui = ui, server = server)
