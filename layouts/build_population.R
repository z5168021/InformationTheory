build_population <-function()
  fluidPage(theme = 'build.css',
    div(class='build_title',h2('Build Your Own Population')),
    div(class = 'build_header',
        helpText('Build a custom haplotype population to use with our calculators. Once finished, download your population by clicking the Generate File button.'),

    ),
    useShinyFeedback(),
    div(class = 'input_container',
        textInput('filename', label = 'Enter the name of your population')
    ),

    column(6,
     div(class = 'build_pop',
         h1('Population Overview'),
         br(),
         fluidRow(
           column(6,  useShinyFeedback(),
                  textInput('original_seq', label = "Enter Nucleotide Sequence")),
           column(6,
                  numericInput("original_num", label = "Number of occurrences",
                               value = 1, min = 1)),
         ),
         uiOutput('new_seq'),
         actionButton('new', 'Add New Haplotype',tags$i(class = 'fa fa-plus')),
         downloadButton('gen', 'Generate File')
     )
    ),

    column(6,
      div(class = 'right',
        plotOutput('preview_pop'))
    ),

  )