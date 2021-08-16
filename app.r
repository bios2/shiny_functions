library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(

  header = dashboardHeader(title = "Composition des communautes", titleWidth = 350),

  sidebar = dashboardSidebar(disable = TRUE),

  body = dashboardBody(
    withMathJax(),
    fluidRow(
      column(width = 4,
             box(width = NULL,
                 status = "primary",
                 title  = "Choose your model",
                 solidHeader = T,

                 radioButtons("eq", "model type",
                              c("Linear" = "linear",
                                "Log-linear" = "loglinear",
                                "Michaelis-Menton" = "mich",
                                "Monod" = "monod")),

                 textOutput(outputId = "matheqn"),


                 # Input: Slider for parameter alpha
                 sliderInput("alpha",
                             "alpha",
                             value = 0,
                             min = -10,
                             max = 200),

                 # Input: Slider for parameter beta
                 sliderInput("beta",
                             "beta",
                             value = 0,
                             min = -10,
                             max = 100)
             )
      ),

      column(width = 8,
             box(width = 4,
                 status = "primary",
                 h4("Model", align = "center"),
                 plotOutput(outputId = "data")
             ),
             # Input: Select the model type
             box(width = 4,
                 status = "primary",
                 h4("Residuals", align = "center"),
                 plotOutput(outputId = "PDF")
             )
      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

    # Load data
    hemlock <- read.table("hemlock.txt", header = T)
    x <- seq(min(hemlock[,1]), max(hemlock[,1]), length.out = 1000)

    functional_form <-  reactive({
      switch(input$eq,
             linear = function(xvar) input$alpha + input$beta * xvar,
             loglinear = function(xvar) input$alpha + input$beta * log(xvar),
             mich = function(xvar) input$alpha * xvar/(input$alpha/input$beta + xvar)
             )
    }
    )

    math_expression <- reactive({
        switch(input$eq,
               linear = '$$
               y = \\alpha + \\beta x
               $$',
               loglinear = function(xvar) input$alpha + input$beta * log(xvar),
               mich = function(xvar) input$alpha * xvar/(input$alpha/input$beta + xvar)
        )
    })

    output$matheqn <- renderText(math_expression())

    # Make the plots
    output$data <- renderPlot({
        plot(hemlock[,1], hemlock[,2], xlab = "Light availability (%)", ylab = "Annual growth (mm/yr)",
        cex.axis = 1.5, cex.lab = 1.5)

        lines(x, functional_form()(x), lwd = 2)
    })

    # PDF
    output$PDF <- renderPlot({

      resid <- hemlock[,2] - functional_form()(hemlock[,1])

      h <- hist(resid, breaks=10, xlab="Observed - Predicted", main="")
      xvec <- seq(min(resid),max(resid),length=100)
      pdf  <- dnorm(xvec, mean=mean(resid), sd=sd(input$sd))
      pdf <- pdf*diff(h$mids[1:2])*length(resid)
      lines(xvec, pdf, lwd=2)
    })

}

# Create Shiny app ----
shinyApp(ui, server)
