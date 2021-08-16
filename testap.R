ui <- fluidPage(
  numericInput("x", "x", value = 0),
  selectInput("trans", "transformation",
              choices = c("square", "log", "square-root")
  ),
  textOutput("out")
)

server <- function(input, output, session) {
  output$out <- renderText({
    if (input$x < 0 && input$trans %in% c("log", "square-root")) {
      validate("x can not be negative for this transformation")
    }

    switch(input$trans,
           square = input$x ^ 2,
           "square-root" = sqrt(input$x),
           log = log(input$x)
    )
  })
}


# Create Shiny app ----
shinyApp(ui, server)
