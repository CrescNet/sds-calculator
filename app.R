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
    ref    <- eval(parse(text = reference))
  }, silent = TRUE)

  try({
    if (length(data[[column.names$bmi]]) == 0) {
      data[[column.names$bmi]] <- data[[column.names$weight]] / (data[[column.names$height]] / 100) ^ 2
    }
  }, silent = TRUE)

  for (object in c('height', 'weight', 'bmi')) {
    try({
      data[[paste(column.names[[object]], 'SDS')]] <- sds(
        value  = data[[column.names[[object]]]],
        age    = data[[column.names$age]],
        sex    = data[[column.names$sex]],
        male   = male,
        female = female,
        item   = object,
        ref    = ref
      )
      data[[paste(column.names[[object]], 'P')]] <- pnorm(data[[paste(column.names[[object]], 'SDS')]]) * 100
    }, silent = FALSE)
  }

  return(data)
}

ui <- fluidPage(
  titlePanel('SDS Calculator for Excel Sheets'),

  # Description of the app
  'Please choose an Excel file (XLSX) and modify the configurations as needed.',
  'A table will be displayed with a preview of the resulting data.',
  br(),
  'When you are satisfied, click "Generate" to save the resulting Excel file.',
  br(),

  tags$h3('Disclaimer'),
  'This tool is not approved as a medicinal product for clinical use, and should be used for research purposes only.',

  hr(),

  fluidRow(
    column(3, fileInput('excel', 'Choose an XLSX File', accept = '.xlsx')),
    column(
      3,
      selectInput(
        'reference',
        'Choose a reference',
        c(
          'Kromeyer-Hauschild' = 'kro.ref',
          'WHO' = 'who.ref',
          'KiGGS' = 'kiggs.ref',
          'AGA' = 'aga_15.ref',
          'Ethiopia' = 'ethiop.ref',
          'Flanders, Belgium' = 'belgium.ref',
          'Portugal' = 'portug.ref',
          'UK WHO' = 'ukwho.ref',
          'China' = 'zong13.ref',
          'CDC' = 'cdc.ref'
        )
      )
    ),
    column(6, style = "margin-top: 25px", tagList('The R package ', a('childsds', href = 'https://cran.r-project.org/package=childsds'), 'is used to calculated the SDS values.'))
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
        column(3, varSelectInput('sex_col', 'Sex column', data())),
        column(3, varSelectInput('age_col', 'Age column (years)', data())),
        column(3, varSelectInput('height_col', 'Height column (cm)', data())),
        column(3, varSelectInput('weight_col', 'Weight column (kg)', data()))
      ),
      fluidRow(
        column(3, textInput('bmi_col', 'BMI column', value = 'BMI')),
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

