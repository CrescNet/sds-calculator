library(shiny)

shinyUI(fluidPage(
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
    column(3, fileInput('excel', 'Choose an XLSX file', accept = '.xlsx')),
    column(9, uiOutput('sheetNameSelection'))
  ),

  fluidRow(
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
    column(
      6,
      style = "margin-top: 25px",
      tagList('The R package ', a('childsds', href = 'https://cran.r-project.org/package=childsds'), 'is used to calculate the SDS values.')
    ),
  ),

  uiOutput('data_set_config'),

  tags$footer(
    hr(),
      tags$small('Source code of this Shiny app is available at: ',
      a('GitHub', href = 'https://github.com/CrescNet/sds-calculator')
    ), align = 'center'
  )
))
