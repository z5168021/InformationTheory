library(shiny)
library(shinyBS)

source("./R/get_fasta.R")
uni <- fluidPage(
  titlePanel("Uniform"),
  p('This type of distribution suggests that the population is symmetrical and all possible values are equally likely to occur.'),
  plotOutput('uniform_plot'),
  bsCollapse(id = "pop1", open = 'Panel 1',
             bsCollapsePanel("View Fasta file", 
                             get_fasta('uniform'), style = "info")
             ),
  downloadButton('u_download', 'Download'),
  br()
  )
even <- fluidPage(
  titlePanel("Even Distribution"),
  p('This type of distribution suggests that there is a symmetric and equal division and sharing of values of data.'),
  plotOutput('evenDistribution_plot'),
  bsCollapse(id = "pop1",
             bsCollapsePanel("View Fasta file", get_fasta('evenDistribution')
                             , style = "info")
  ),
  downloadButton('ed_download', 'Download'),
  br(),
)
single <- fluidPage(
  
  titlePanel("Single Outlier"),
  p('This type of distribution suggests that there is a data point which has a significant difference in value when comparing other sets of data, often excluded to avoid any error during statistical analysis.'),
  plotOutput('singleOutlier_plot'),
  bsCollapse(id = "pop1", open = 'Panel 1',
             bsCollapsePanel("View Fasta file", 
                             get_fasta('singleOutlier'), style = "info")
             
  ),
  downloadButton('s_download', 'Download'),
  br(),
)
random <- fluidPage(
  titlePanel("Random Distribution"),
  p('This type of distribution suggests that all values that are distributed are asymmetric and randomly scattered with no visible patterns or trends. '),
  plotOutput('randomDistribution_plot'),
  bsCollapse(id = "pop1",
             bsCollapsePanel("View Fasta file", 
                             get_fasta('randomDistribution'), style = "info")
             
  ),
  downloadButton('r_download', 'Download'),
  br()
)
      

populations <- function(){
  fluidPage(
    column(6,uni),
    column(6,even),
    column(6,single),
    column(6, random)
  )
}