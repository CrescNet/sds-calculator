library(shiny)
library(openxlsx)

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
    if (length(data[[column.names$bmi]]) == 0) {
      data[[column.names$bmi]] <- data[[column.names$weight]] / (data[[column.names$height]] / 100) ^ 2
    }
  }, silent = TRUE)

  recodeSex <- list()
  recodeSex[[male]]   <- 'male'
  recodeSex[[female]] <- 'female'

  for (measurement in c('height', 'weight', 'bmi')) {
    try({
      if (length(data[[column.names[[measurement]]]]) == 0) {
        next
      }
      data[[paste(column.names[[measurement]], 'SDS')]] <- sds(
        x   = data[[column.names$age]],
        y   = data[[column.names[[measurement]]]],
        sex = data[[column.names$sex]],
        measurement = measurement,
        refName     = reference,
        recodeSex   = recodeSex
      )
      data[[paste(column.names[[measurement]], 'P')]] <- pnorm(data[[paste(column.names[[measurement]], 'SDS')]]) * 100
    }, silent = TRUE)
  }

  return(data)
}


shinyServer(function(input, output) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  validInput <- reactive({
    req(input$excel)
    ext <- tools::file_ext(input$excel$datapath)
    validate(need(ext == 'xlsx', 'Please choose an XLSX file'))
    TRUE
  })

  sheetNames <- reactive({
    req(validInput())
    getSheetNames(input$excel$datapath)
  })

  data <- reactive({
    req(validInput(), input$sheetName)
    read.xlsx(input$excel$datapath, sheet = input$sheetName, sep.names = ' ')
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

  output$sheetNameSelection <- renderUI({
    req(sheetNames())
    selectInput('sheetName', 'Sheet name', sheetNames())
  })

  output$preview <- renderTable({
    req(preview())
    preview()
  })

  output$data_set_config <- renderUI({
    req(data())
    tagList(
      hr(),
      fluidRow(
        column(3, varSelectInput('sex_col', 'Sex column', data())),
        column(3, varSelectInput('age_col', 'Age column (years)', data())),
        column(3, varSelectInput('height_col', 'Height column (cm)', data())),
        column(3, varSelectInput('weight_col', 'Weight column (kg)', data()))
      ),
      fluidRow(
        column(3, textInput('bmi_col', 'BMI column', value = 'BMI')),
        column(3, textInput('male_string', 'Male value', value = 'male')),
        column(3, textInput('female_string', 'Female value', value = 'female')),

        column(3, style = 'margin-top: 25px', downloadButton('generate', 'Generate'))
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
})
