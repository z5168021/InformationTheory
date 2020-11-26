library(shiny)
library(plotly)

SNP_view <-fluidPage(
  br(), br(), br(), br(),
  sidebarLayout(
    sidebarPanel (
      titlePanel("Sample Data Input"),
      sliderInput(inputId = "num_a", 
                  label = "Number of A:",
                  min = 0, 
                  max = 100, 
                  value = 0),
      
      sliderInput(inputId = "num_c", 
                  label = "Number of C:",
                  min = 0, 
                  max = 100, 
                  value = 0),
      
      sliderInput(inputId = "num_g", 
                  label = "Number of G:",
                  min = 0, 
                  max = 100, 
                  value = 0),
      
      sliderInput(inputId = "num_t", 
                  label = "Number of T:",
                  min = 0, 
                  max = 100, 
                  value = 0),
      actionButton(inputId = 'qd_settings', 'Settings', tags$i(class = 'fa fa-cog'))
    ),
    mainPanel(
      titlePanel('Results'),
      tabsetPanel(type = 'tabs', id = 'view',
                  tabPanel('Single View',
                           fluid = TRUE, 
                           id = 'mode',
                           br(), br(), br(),
                           fluidRow(
                             column(8, uiOutput('entropy_equations')),
                             column(4, uiOutput('qd_equations'))
                           ),
                           br(),
                           fluidRow(uiOutput('ln0_explanation')),
                           br(), br(),
                           plotlyOutput('single_plot'),
                           uiOutput('snp_single')
                           ),
                           
                  tabPanel('Comparative View',
                           fluid = TRUE,  
                           br(), br(), br(),
                           fluidRow(
                             actionButton("btn_add", "Add"),
                             actionButton("btn_response", "Show Response"),
                             actionButton("btn_reset", "Reset")
                           ),
                           br(),
                           DT::dataTableOutput("responses", width = 300),
                           br(),
                           plotlyOutput("SNP_comp_plot"),
                           uiOutput('snp_comp')
                  )                    
      )
    )
  )
)


haplo_view <-fluidPage(
  br(), br(), br(), br(),
  sidebarLayout(
    sidebarPanel(
      titlePanel("Population"),
      tabsetPanel(type ="pills", id = "haplo_pop_type",
                  tabPanel("Select Population", br(),
                           tags$label("Choose a sample population"),
                           tags$div(class='selector',
                                    tags$select(class="drop_down",id = "sample_haplo_pop",
                                                tags$option(class = 'choice', "evenDistribution"),
                                                tags$option(class = 'choice', "randomDistribution"),
                                                tags$option(class = 'choice', "SingleOutlier"),
                                                tags$option(class = 'choice', "uniform")
                                    ))),
                           
                  tabPanel("Supply Population", br(),  
                           fileInput(inputId = "user_haplo_pop",
                                     label = "Import your own population Data",
                                     placeholder = "Select a file selected"))),
      actionButton(inputId = "view_haplo_pop",
                   label = "View Population"
      )      
    ),
    mainPanel(
      titlePanel('Results'),
      tabsetPanel(type = 'tabs', id = 'view',
                  tabPanel('single view', 
                           fluid = TRUE,
                           br(), br(), br(),
                           plotlyOutput('single_haplo_plot'),
                           uiOutput('hap_single')),
                  tabPanel('comparative view', 
                           fluid = TRUE,  
                           br(), br(), br(),
                           fluidRow(
                             actionButton("btn_add_pop", "Add population"),
                             actionButton("btn_gen_pop", "Generate"),
                             actionButton("btn_reset_pop", "Reset")
                           ),                           
                           br(),
                            DT::dataTableOutput("population_list", width = 300),
                           br(),
                           plotlyOutput("comparative_haplo_plot"),
                           uiOutput('hap_comp')
                  )                    
      )
    )
  )
)

help_view <-fluidPage(
  uiOutput('helpText')
)


calculator <- function() {
  calculator <- fluidPage(theme = "qd_profile.css",
                          help_view,
                          tabsetPanel(type = 'tabs', id = 'mode',
                                      tabPanel('SNP', fluid = TRUE, SNP_view),
                                      tabPanel('Haplotype', fluid = TRUE, haplo_view)
                          )
  )
}


