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

calculateSdsValues <- function (
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
    data[[paste(column.names$bmi, 'P')]] <- pnorm(data[[column.names$bmi_sds]]) * 100
  }, silent = TRUE)

  return(data)
}

ui <- fluidPage(
  titlePanel('SDS Calculator for Excel Sheets'),

  # Description of the app
  'Please choose an Excel file (XLSX) and modify the configurations as needed. A table will be displayed with a preview of the resulting data.',
  br(),
  'When you are satisfied, click on "Save".',

  hr(),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('excel', 'Choose an XLSX File', accept = '.xlsx'),
      selectInput('reference', 'Choose a reference', c('Kromayer-Hauschild' = 'kro.ref', 'WHO' = 'who.ref', 'KiGGS' = 'kiggs.ref', 'AGA' = 'aga_15.ref')),
      uiOutput('data_set_config')
    ),

    mainPanel(
      tableOutput('preview')
    )
  )
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
      textInput('bmi_col', 'BMI Column', value = 'BMI'),
      varSelectInput('age_col', 'Age Column', data()),
      varSelectInput('sex_col', 'Sex Column', data()),
      textInput('male_string', 'Male value', value = 'male'),
      textInput('female_string', 'Female value', value = 'female'),
      varSelectInput('height_col', 'Height Column', data()),
      varSelectInput('weight_col', 'Weight Column', data()),
      actionButton('apply_config', 'Apply')
    )
  })

  observeEvent(input$apply_config, {
    req(input$age_col, input$height_col, input$weight_col)

  })
}

# Run the application
shinyApp(ui = ui, server = server)

