library(shiny)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  header = dashboardHeader(title = "Hemlock data", titleWidth = 350),

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
                                "Michaelis-Menton" = "mich")),

                 helpText("
              $$
              \\begin{align}
              \\mu &= \\alpha + \\beta x \\\\
              \\mu &= \\alpha + \\beta \\text{ln}(x) \\\\
              \\mu &= \\frac{\\alpha x}{\\alpha / \\beta + x} \\\\
              y &\\sim \\text{Normal}(\\mu, \\sigma)
              \\end{align}
               $$
              "),


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
                             max = 100),

                 sliderInput("sd",
                             "sigma",
                             value = 1,
                             min = .1,
                             max = 200)
             )
      ),

      column(width = 4,
             box(width = NULL,
                 status = "primary",
                 h4("Model", align = "center"),
                 plotOutput(outputId = "data")
             )
      ),
      column(width = 4,
             # Input: Select the model type
             box(width = NULL,
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

    # Make the plots
    output$data <- renderPlot({
        plot(hemlock[,1], hemlock[,2], xlab = "Light availability (%)", ylab = "Annual growth (mm/yr)",
        cex.axis = 1.5, cex.lab = 1.5)

        lines(x, functional_form()(x), lwd = 2)
    })

    expected_residual <- reactive({
      function(x) dnorm(x, mean=0, sd = input$sd)
    })

    # PDF
    output$PDF <- renderPlot({

      resid <- hemlock[,2] - functional_form()(hemlock[,1])

      h <- hist(resid, breaks=10,
                xlab="Observed - Predicted",
                xlim = c(-200,200),
                main="", freq = FALSE)

      curve(dnorm(x, mean = mean(resid), sd = sd(resid)),
            add = TRUE, col = "red")

      curve(expected_residual()(x), add = TRUE, col = "blue")
    })

}

# Create Shiny app ----
shinyApp(ui, server)
