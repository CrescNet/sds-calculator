#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(childsds)

calculateSdsValues <- function(
  data,
  reference,
  column.names = list(
    age    = 'Age',
    sex    = 'Sex',
    height = 'Height',
    weight = 'Weight',
    bmi    = 'BMI'
  ),
  male   = 'male',
  female = 'female'
) {
  try({
    age    <- data[[column.names$age]]
    sex    <- data[[column.names$sex]]
    height <- data[[column.names$height]]
    weight <- data[[column.names$weight]]
    ref    <- eval(parse(text = reference))
  }, silent = TRUE)

  try({
    data[[paste(column.names$height, 'SDS')]] <- sds(
      value = height,
      age   = age,
      sex   = sex, male = male, female = female,
      item  = 'height',
      ref   = ref
    )
    data[[paste(column.names$height, 'P')]] <- pnorm(data[[paste(column.names$height, 'SDS')]]) * 100
  }, silent = TRUE)

  try ({
    data[[paste(column.names$weight, 'SDS')]] <- sds(
      value = weight,
      age   = age,
      sex   = sex, male = male, female = female,
      item  = 'weight',
      ref   = ref
    )
    data[[paste(column.names$weight, 'P')]] <- pnorm(data[[paste(column.names$weight, 'SDS')]]) * 100
  }, silent = TRUE)

  try({
    if (length(data[[column.names$bmi]]) == 0) {
      data[[column.names$bmi]] <- weight / (height / 100) ^ 2
    }
    data[[paste(column.names$bmi, 'SDS')]] <- sds(
      value = data[[column.names$bmi]],
      age   = age,
      sex   = sex, male = male, female = female,
      item  = 'bmi',
      ref   = ref
    )
    data[[paste(column.names$bmi, 'P')]] <- pnorm(data[[paste(column.names$bmi, 'SDS')]]) * 100
  }, silent = TRUE)

  return(data)
}

ui <- fluidPage(
  titlePanel('SDS Calculator for Excel Sheets'),

  # Description of the app
  'Please choose an Excel file (XLSX) and modify the configurations as needed. A table will be displayed with a preview of the resulting data.',
  br(),
  'When you are satisfied, click on "Generate" to save the resulting Excel file.',

  hr(),

  fluidRow(
    column(3, fileInput('excel', 'Choose an XLSX File', accept = '.xlsx')),
    column(3, selectInput('reference', 'Choose a reference', c('Kromeyer-Hauschild' = 'kro.ref', 'WHO' = 'who.ref', 'KiGGS' = 'kiggs.ref', 'AGA' = 'aga_15.ref')))
  ),

  hr(),

  uiOutput('data_set_config')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  data <- reactive({
    req(input$excel)
    file <- input$excel
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == 'xlsx', 'Please choose an XLSX file'))
    read.xlsx(input$excel$datapath, sep.names = ' ')
  })

  preview <- reactive({
    req(data(), input$reference)
    calculateSdsValues(
      head(data()),
      input$reference,
      male   = input$male_string,
      female = input$female_string,
      column.names = list(
        age    = input$age_col,
        sex    = input$sex_col,
        height = input$height_col,
        weight = input$weight_col,
        bmi    = input$bmi_col
      )
    )
  })

  output$preview <- renderTable({
    req(preview())
    preview()
  })

  output$data_set_config <- renderUI({
    req(data())
    tagList(
      fluidRow(
        column(3, varSelectInput('sex_col', 'Sex Column', data())),
        column(3, varSelectInput('age_col', 'Age Column (years)', data())),
        column(3, varSelectInput('height_col', 'Height Column (cm)', data())),
        column(3, varSelectInput('weight_col', 'Weight Column (kg)', data()))
      ),
      fluidRow(
        column(3, textInput('bmi_col', 'BMI Column', value = 'BMI')),
        column(3, textInput('male_string', 'Male value', value = 'male')),
        column(3, textInput('female_string', 'Female value', value = 'female')),

        column(3, style = "margin-top: 25px", downloadButton('generate', 'Generate'))
      ),
      hr(),
      tags$h3('Preview'),
      tableOutput('preview')
    )
  })

  output$generate <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$excel$name), '_SDS.xlsx')
    },
    content = function(file) {
      data <- calculateSdsValues(
        data(),
        input$reference,
        male   = input$male_string,
        female = input$female_string,
        column.names = list(
          age    = input$age_col,
          sex    = input$sex_col,
          height = input$height_col,
          weight = input$weight_col,
          bmi    = input$bmi_col
        )
      )
      write.xlsx(data, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

