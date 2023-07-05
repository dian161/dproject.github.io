library(shiny)

ui <- fluidPage(
  titlePanel("Check Box and Ratio Button widgets"),
  fluidRow(
    column(width = 2,
           h3("Single checkbox"),
           checkboxInput("checkboxInputValue", label = "Choice A"),
           textOutput("checkboxOutputValue")
    ),
    column(width = 3,
           checkboxGroupInput("checkboxGroupInputValue",
                              h3("Variables to show:"),
                              choices = c("Mileage"="mpg",
                                          "Cylinders"="cyl",
                                          "Transmission"="am",
                                          "Gears"="gear"),
                              selected = "mpg")
    ),
    column(width = 3,
           textOutput("checkboxGroupOutputValue")
    ),
    column(width = 3,
           tableOutput("tableData")
    )
  ),
  
  fluidRow(
    column(width = 2,
           radioButtons("radioButtonInputValue", h3("Radio buttons"),
                        choices = c("Choice 1" = "norm", "Choice 2" = "unif", "Choice 3" = "exp"),
                        selected = "norm")
    ),
    column(width = 6,
           plotOutput("radioButtonOutputPlot")
    ),
    column(width = 2,
           textOutput("radioButtonOutputValue")
    )
  )
)

server <- function(input, output){
  output$checkboxOutputValue <- renderText({
    paste("Check is selected?", input$checkboxInputValue)
  })
  output$tableData <- renderTable({mtcars[1:8, c(input$checkboxGroupInputValue), drop = FALSE]},
                                  rownames = TRUE)
  
  output$checkboxGroupOutputValue <- renderText({
    attribute <- paste(input$checkboxGroupInputValue, collapse = ",")
    paste("Your selected attributes are:", attribute)
  })
  output$radioButtonOutputValue <- renderText({
    paste(input$radioButtonInputValue, "distribution plotted.")
  })
  
  output$radioButtonOutputPlot <- renderPlot({
    distFunc <- switch (input$radioButtonInputValue,
                        norm = rnorm,
                        unif = runif,
                        exp = rexp,
                        rnorm)
    hist(distFunc(500))
  })
}

shinyApp(ui, server)
