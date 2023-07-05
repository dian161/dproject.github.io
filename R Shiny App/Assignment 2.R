library(shiny)
library(datasets)
library(DT)
library(htmltools)
library(base)

ui=fluidPage(
  titlePanel("Miles Per Gallon"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select","Variable:", choices = c("Cylinders",
                                                    "Transmission",
                                                    "Gears")),
      checkboxInput("checkboxInputValue1", label = "Show outliers"),
      checkboxInput("checkboxInputValue2", label = "Show header of the selected columns"),
      tableOutput("tableData")
    ),
    mainPanel(
      textOutput("selectOutputValue"),
      plotOutput("selectInputOutputValue"),
      dataTableOutput("table"),
    )
  )
)

server = function(input, output) {
  selectInput <- reactive({
    switch(input$select,
           "Cylinders" = "cyl",
           "Transmission" = "am",
           "Gears" = "gear")})
  output$tableData = renderTable({mtcars[1:6, c("mpg", selectInput()), drop = FALSE]},
                                 rownames = FALSE)
  output$selectOutputValue = renderText({
    paste("mpg~", selectInput())
  })
  output$selectInputOutputValue = renderPlot({
    boxplot(mpg~., data = mtcars[, c("mpg", selectInput())], outline = input$checkboxInputValue1, xlab = selectInput(), ylab = "mpg")})
  output$table <- renderDataTable({datatable(head(mtcars), 
                                             rownames = FALSE, style = "bootstrap",
                                             options = list(lengthChange = TRUE, autoWidth = TRUE))
  })
}

shinyApp(ui,server)
