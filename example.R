# install.packages("pak")
pak::pak("hadley/elmer")

chat <- elmer::chat_openai(
  model = "gpt-4o",
  system_prompt = "Be terse but professional."
)

chat$chat("When was the R language created?")

chat$chat("
  What is the difference between a tibble and a data frame?
  Answer with a bulleted list
")

chat$chat("What is the latest version of TrueNAS Scale?")

chat$chat("How many R packages are available on CRAN?")

chat$chat("Can you provide a simple shiny app which makes use of the elmer package?")

library(shiny)
library(elmer)

ui <- fluidPage(
  titlePanel("Simple Linear Model with Elmer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X variable:", choices = names(mtcars), selected = "wt"),
      selectInput("yvar", "Select Y variable:", choices = names(mtcars), selected = "mpg")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      verbatimTextOutput("lmSummary")
    )
  )
)

server <- function(input, output) {
  model <- reactive({
    elmer(input$yvar ~ input$xvar, data = mtcars)
  })

  output$scatterPlot <- renderPlot({
    plot(mtcars[[input$xvar]], mtcars[[input$yvar]],
         xlab = input$xvar, ylab = input$yvar, main = "Scatter Plot")
    abline(model(), col = "blue")
  })

  output$lmSummary <- renderPrint({
    summary(model())
  })
}

shinyApp(ui = ui, server = server)

chat$chat('Modify the sidebar to allow for chatgpt with elmer')





library(shiny)
library(openai)

# Set your API key
Sys.setenv(OPENAI_API_KEY = "your_api_key_here")

ui <- fluidPage(
  titlePanel("Shiny App with ChatGPT"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Enter your message:", ""),
      actionButton("submit", "Send")
    ),
    mainPanel(
      verbatimTextOutput("gpt_response")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    if (nzchar(input$user_input)) {
      response <- openai::create_completion(
        model = "gpt-4o",
        prompt = input$user_input,
        max_tokens = 100
      )

      output$gpt_response <- renderText({
        response$choices[[1]]$text
      })
    }
  })
}

shinyApp(ui = ui, server = server)
