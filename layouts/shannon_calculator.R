
library(shiny)
library(plotly)

single_view <- div(
  br(),
  br(),
  div(style = "margin:20px; display:block; width:80%;",
      uiOutput('filled_equation'),
      plotlyOutput("plot"),
      uiOutput('download_but'),
      br(), br()
  )

  
)
comparative_view <-div(
  br(),
  br(),
  br(), br(),
  DT::dataTableOutput("profiles", width = 750),
  actionButton('get_graphs', 'Plot Results'),
  actionButton('remove_all', 'Remove all profiles'),
    br(), br(),
  plotlyOutput("comparative_plot"),
  uiOutput('download_info'),
    br(), br(),

)


shannon_calculator <- function()
  fluidPage(theme = "shanhet.css",
    uiOutput('equation'),
    sidebarLayout(
      sidebarPanel (
             titlePanel("Population"),
             tabsetPanel(type ="pills", id = "population_type",
                         tabPanel("Select Population", br(),br(),
                                  br(),tags$label("Choose a sample population"),
                                  br(),tags$div(class='selector',
                                  br(),tags$select(class="drop_down",id = "population",
                                              tags$option(class = 'choice', "evenDistribution"),
                                              tags$option(class = 'choice', "randomDistribution"),
                                              tags$option(class = 'choice', "SingleOutlier"),
                                              tags$option(class = 'choice', "uniform")
                                  ))),
                         tabPanel("Supply Population", br(),
                                  useShinyFeedback(),
                                  fileInput(inputId = "user_file",
                                  label = "Import your own population Data",
                                  placeholder = "Select a file selected")), br(),br()),
             br(),actionButton(inputId = "view_pop", label = "View Population"),
             
             div(class = 'title_but', 
                 titlePanel("Population Parameters"),
                 actionButton("settings", "Settings", tags$i(class = 'fa fa-cog'))
             ),
             br(),
             br(),
             sliderInput(inputId = "Ne",
                         label = "Effective Population Size (Ne)",
                         min = 1, 
                         max =100,
                         value = 0),
             sliderInput(inputId = "t",
                         label = "Number of Generations (t)",
                         min = 1,
                         max = 100,
                         value = 1),
             sliderInput("graph_ne",
                         label = "Graph shan-het values for Generation Range",
                         min = 1, 
                         max =100,
                         value = c(1,100)),
             actionButton("btn_calculate", "Calculate")),
      
      mainPanel(
             titlePanel("Results"),
             tabsetPanel(type = "tabs", id = "analysis_mode",
                         tabPanel("Single Population", single_view),
                         tabPanel("Comparative View", comparative_view))
             
             
      )
    )
  )

