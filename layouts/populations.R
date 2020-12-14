library(shiny)
library(shinyBS)

source("./R/get_fasta.R")
uni <- fluidPage(
  titlePanel("Uniform"),
  p('This sample population contains a single unique sample to produce a uniform distribution. There are 35 occurrences of the sequence AATGTGGATATT.'),
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
  p('This sample population contains equal occurrences of three different haplotypes to produce an even distribution. The different sequences occur equally with 12 counts each.'),
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
  p('This sample population contains four different haplotypes. Three occur with the same counts while a single sequence occurs at a much lower frequency of once. This distribution contains the single outlier haplotype AATTTCGATGAA.'),
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
  p('This sample population contains many different haplotypes with varying counts to produce a random distribution.'),
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