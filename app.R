#running application
library(shiny)
library(shiny.router)
library(ggplot2)
library(RColorBrewer)
library(rmarkdown)
library(shinyFeedback)
library(knitr)
library(reshape2)
library(DT)

source("./layouts/calculator.R")
source("./layouts/shannon_calculator.R")
source("./layouts/page.R")
source("./layouts/help_page.R")
source("./layouts/populations.R")
source("./layouts/build_population.R")
source("./layouts/team.R")
source("./R/calculate_shannon.R")
source("./R/get_fasta.R")
source("./R/get_population_data.R")
source("./R/check_fasta_file.R")
source("./R/calculate_qdProfile.R")
source("./R/haplotype_profiles.R")
source("./R/get_varient.R")
source("./R/qdProfile_for_list.R")

### GLOBAL VARIABLES ### 
base_fields <- c("num_a", "num_c", "num_g", "num_t")
base_names <- c('Number of A', 'Number of C', 'Number of G', 'Number of T')
number_pops <- reactiveValues(a=0)

# Set up Page layouts.
root_page <- page("Teaching How Genetic Variation Changes Over Time", 
                  htmlTemplate("./www/homescreen.html"))
other_page <- page( HTML(paste0(tags$sup("q"), "D profile")), calculator())
shannon_cal <-page("Shannon Molecular Variation Over Time", shannon_calculator())
how_to_page <-page("Instructional Videos", htmlTemplate("./www/help.html"))
populations <- page("Sample Populations", populations())
build_page <-page("Build a Population", build_population())
team_page <- page("Our Team", htmlTemplate("./www/team.html"))

#Defining server logic for the pages
other_callback <- function(input, output, session){
  
  
  observeEvent(input$qd_settings,{
    showModal(modalDialog(
      title = "Modify maximum values of each base",
      div(style="display:inline-block; vertical-align:top;",
          fluidRow(
            column(8, 
                   numericInput("new_a", label = "Maximum number of A", 
                                value = 100)),
            br(),
            column(4,
                   
                   actionButton("btn_new_a", label = "Update", size = "extra-small"))
          ),
          fluidRow(
            column(8, 
                   numericInput("new_c", label = "Maximum number of C", 
                                value = 100)),
            br(),
            column(4,
                   
                   actionButton("btn_new_c", label = "Update", size = "extra-small"))
          ),
          fluidRow(
            column(8, 
                   numericInput("new_g", label = "Maximum number of G", 
                                value = 100)),
            br(),
            column(4,
                   
                   actionButton("btn_new_g", label = "Update", size = "extra-small"))
          ),
          fluidRow(
            column(8, 
                   numericInput("new_t", label = "Maximum number of T", 
                                value = 100)),
            br(),
            column(4,
                   
                   actionButton("btn_new_t", label = "Update", size = "extra-small"))
          )
      )
    ))
    
    observeEvent(input$btn_new_a,{
      updateSliderInput(session, inputId = 'num_a', max = input$new_a)
    })
    observeEvent(input$btn_new_c,{
      updateSliderInput(session, inputId = 'num_c', max = input$new_c)
    })
    observeEvent(input$btn_new_g,{
      updateSliderInput(session, inputId = 'num_g', max = input$new_g)
    })
    observeEvent(input$btn_new_t,{
      updateSliderInput(session, inputId = 'num_t', max = input$new_t)
    })
  })
  
  output$snp_single <- renderUI({
    list(
      br(),
      h2("Download results"),
      checkboxInput('include_n', 'Include Nucleotide Distribution', value = FALSE),
      checkboxInput('include_g', 'Include Graph', value = FALSE),
      radioButtons('format', 'Document format', c('HTML', 'Word'),
                   inline = TRUE),
      downloadButton("get_snp_single", "Download")
    )
  })
  
  output$snp_comp <-renderUI({
    list(
      br(),
      h2("Download results"),
      checkboxInput('snp_g', 'Include Graph', value = FALSE),
      checkboxInput('include_t', 'Include Data table', value = FALSE),
      radioButtons('format', 'Document format', c('HTML', 'Word'),
                   inline = TRUE),
      downloadButton("get_snp_comp", "Download")
    )
  })
  
  output$hap_single <- renderUI({
    list(
      br(),
      h2("Download results"),
      checkboxInput('haps_g', 'Include Graph', value = FALSE),
      checkboxInput('include_p', 'Include Population count', value = FALSE),
      radioButtons('format', 'Document format', c('HTML', 'Word'),
                   inline = TRUE),
      downloadButton("get_hap_single", "Download")
    )
  })
  output$hap_comp <-renderUI({
    list(
      br(),
      h2("Download results"),
      checkboxInput('hapc_g', 'Include Graph', value = FALSE),
      checkboxInput('hapc_t', 'Include population list', value = FALSE),
      radioButtons('for4', 'Document format', c('HTML', 'Word'),
                   inline = TRUE),
      downloadButton("get_hap_comp", "Download")
    )
  })
  
  output$get_snp_single <-downloadHandler(
    filename = function(){
      paste('my-report', sep = '.',switch(
        input$format, HTML = 'html', Word = 'word'
      ))
    },
    content = function(file){
      values =  calculate_qdProfile(input$num_a, 
                                    input$num_c, 
                                    input$num_g, 
                                    input$num_t)
      mode <- 'SNP_single'
      data <-  values
      pop <- input$include_n
      sequences <- NULL
      seq_count <- c(input$num_a, 
                     input$num_c, 
                     input$num_g, 
                     input$num_t)
      cols <-NULL
      src <- normalizePath('qdProfile.RMD')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.copy(src, 'qdProfile.RMD', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('qdProfile.RMD', 
                               params = list(
                                 mode = mode,
                                 graph = input$include_g,
                                 data = data,
                                 pop = pop,
                                 seq = sequences,
                                 count = seq_count,
                                 cols = cols
                               ),
                               switch(
                                 input$format, 
                                 HTML = html_document(), Word = word_document()
                               ))
      file.rename(out, file)
      
    }
  )
  
  output$get_snp_comp <- downloadHandler(
    filename = function(){
      paste('my-report', sep = '.',switch(
        input$format, HTML = 'html', Word = 'word'
      ))
    },
    content = function(file){
      entropy_values <- data.frame(entropy_0 = vector(), entropy_1 = vector(), entropy_2 = vector())
      for (row in 1:nrow(responses)) {
        
        # calculate their entropy values
        entrop_0 <- calculate_entropy_0(row)
        entrop_1 <- calculate_entropy_1(row)
        entrop_2 <- calculate_entropy_2(row)
        
        # store entropy values
        curr <- data.frame(entrop_0, entrop_1, entrop_2)
        entropy_values <- rbind(entropy_values, curr)
        
      }
      
      # Append num_alleles to entropy values
      entropy_values$num_alleles <- calculate_num_alleles()
      
      # Sort entropy_values by num_alleles
      entropy_values <- entropy_values[order(entropy_values$num_alleles),]
      
      # Get dataframe of number of alleles in each sample
      alleles <- calculate_num_alleles()
      
      # Join the column
      entropy_values$num_alleles <- alleles
      
      # convert to q-profile values
      q_profile <- data.frame(qd_0 = vector(), qd_1 = vector(), qd_2 = vector())
      
      for (row in 1:nrow(entropy_values)) {
        
        # calculate qd-values
        qd_0 <- entropy_values[row, "num_alleles"]
        qd_1 <- exp(entropy_values[row, "entrop_1"])
        qd_2 <- 1/(1-entropy_values[row, "entrop_2"])
        
        # store as row in q_profile
        qd_values <- data.frame(qd_0, qd_1, qd_2)
        q_profile <- rbind(q_profile, qd_values)
        
      }
      type = 'SNP'
      data <- q_profile
      profiles <- responses
      mode <- ''
      
      src <- normalizePath('qdProfileMulti.RMD')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.copy(src, 'qdProfileMulti.RMD', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('qdProfileMulti.RMD', 
                               params = list(
                                 type = type,
                                 profiles = profiles,
                                 table = input$include_t,
                                 graph = input$snp_g,
                                 data = data,
                                 mode = mode
                               ),
                               switch(
                                 input$format, 
                                 HTML = html_document(), Word = word_document()
                               ))
      file.rename(out, file)
    }
  )
  
  output$get_hap_single <- downloadHandler(
    filename = function(){
      paste('my-report', sep = '.',switch(
        input$format, HTML = 'html', Word = 'word'
      ))
    },
    content = function(file){
      if (input$haplo_pop_type == "Select Population") {
        supplied = FALSE
        population = input$sample_haplo_pop
      }else{
        if (is.null(input$user_haplo_pop)){
          shinyFeedback::showFeedbackDanger(
            inputId = 'user_haplo_pop',
            text = 'Please provide a File'
          )
          return()
        }
        if (check_fasta_file(input$user_haplo_pop) == FALSE){
          shinyFeedback::showFeedbackWarning(
            inputId = 'user_haplo_pop',
            text = 'File is not in Fasta Format'
          )
          return()
        }
        hideFeedback('user_haplo_pop')
        
        supplied = TRUE
        population = input$user_haplo_pop
        population = population$datapath
      }
      
      datdat <- get_population_data(population, supplied)
      datdat <- datdat[-1]
      
      sequences <- names(datdat)
      seq_count <- as.numeric(as.character(datdat))
      
      num = length(seq_count)
      if (num < 3) num = 3
      cols <- brewer.pal(num, "BuGn")
      
      # Entropy q = 0
      entropy_0 <- length(sequences) - 1
      
      # Entropy q = 1
      entropy_1 <- 0
      
      for (count in seq_count) {
        proportion <- count/sum(seq_count)
        if (proportion != 0) {
          entropy_1 <- entropy_1 + (proportion * log(proportion, base = exp(1)))
        }
      }
      entropy_1 <- -1 * entropy_1
      
      # Entropy q = 2
      entropy_2 <- 0
      for (count in seq_count) {
        proportion <- count/sum(seq_count)
        if (proportion != 0) {
          entropy_2 <- entropy_2 + proportion^2
        }
      }
      entropy_2 <- 1 - entropy_2 
      
      # qD-value q = 0
      qd_0 <- length(seq_count)
      
      # qD-value q = 1
      qd_1 <- exp(entropy_1)
      
      # qD-value q = 2
      qd_2 <- 1/(1-entropy_2)
      
      data <- c(qd_0, qd_1, qd_2)
      mode <- 'hap_single'
      pop <- input$include_p
      
      
      src <- normalizePath('qdProfile.RMD')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.copy(src, 'qdProfile.RMD', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('qdProfile.RMD', 
                               params = list(
                                 mode = mode,
                                 graph = input$haps_g,
                                 data = data,
                                 pop = pop,
                                 seq = sequences,
                                 count = seq_count,
                                 cols = cols
                               ),
                               switch(
                                 input$format, 
                                 HTML = html_document(), Word = word_document()
                               ))
      file.rename(out, file)
      
    }
  )
  
  
  output$get_hap_comp <- downloadHandler(
    filename = function(){
      paste('my-report', sep = '.',switch(
        input$for4, HTML = 'html', Word = 'word'
      ))
    },
    content = function(file){
      profiles <- population_list
      type = 'haplotypes'
      data <- qdProfile_for_list(population_list)
      
      src <- normalizePath('qdProfileMulti.RMD')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      file.copy(src, 'qdProfileMulti.RMD', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('qdProfileMulti.RMD', 
                               params = list(
                                 type = type,
                                 profiles = profiles,
                                 table = input$hapc_t,
                                 graph = input$hapc_g,
                                 data = data,
                                 mode = 'mode'
                               ),
                               switch(
                                 input$for4, 
                                 HTML = html_document(), Word = word_document()
                               ))
      file.rename(out, file)
    }
  )
  
  
  output$ln0_explanation <- renderUI({
    if (find_S()-1 >= 0) {
      withMathJax(
        helpText("$^*$ When considering limits, 0ln(0) tends to 0. We've chosen to display this for clarity.")
      )
    }
  })
  
  output$entropy_equations <- renderUI({
    if (find_S()-1 >= 0) {
      withMathJax(
        helpText('$$^0H = ', find_S(),' - 1', '= ', find_S()-1, '$$'),
        helpText('$$^1H^* = -',
                 round(input$num_a/find_proportion(), digits = 3),'ln(', round(input$num_a/find_proportion(), digits = 3),')',
                 ' - ', round(input$num_c/find_proportion(), digits = 3),'ln(', round(input$num_c/find_proportion(), digits = 3),')',
                 ' - ', round(input$num_g/find_proportion(), digits = 3),'ln(', round(input$num_g/find_proportion(), digits = 3),')',
                 ' - ', round(input$num_t/find_proportion(), digits = 3),'ln(', round(input$num_t/find_proportion(), digits = 3),')',
                 ' = ',  round(calculate_1H(), digits = 3),
                 '$$'),
        helpText('$$^2H = 1 - (', 
                 round(input$num_a/find_proportion(), digits = 3), '^2 + ', 
                 round(input$num_c/find_proportion(), digits = 3), '^2 + ',
                 round(input$num_g/find_proportion(), digits = 3), '^2 + ',
                 round(input$num_t/find_proportion(), digits = 3), '^2',
                 ') = ', round(calculate_2H(), digits = 3),
                 '$$')
      )
      
    }
  })
  
  output$qd_equations <- renderUI({
    if (find_S()-1 >= 0) {
      withMathJax(
        helpText('$$^0D = ', find_S(), '$$'),
        helpText('$$^1D = e^{', (round(calculate_1H(), digits = 3)), '} = ', round(exp(calculate_1H()), digits = 3) ,'$$'),
        helpText('$$^2D = 1/(1 - ',
                 round(calculate_2H(), digits = 3),
                 ') = ', round(1/(1-calculate_2H()), digits = 3),
                 '$$')
      )
      
    }
  })
  
  calculate_2H <- function() {
    entrop_2 <- 0
    
    proportion_a <- input$num_a/find_proportion()
    if (proportion_a != 0) {
      entrop_2 <- entrop_2 + proportion_a^2
    }
    proportion_c <- input$num_c/find_proportion()
    if (proportion_c != 0) {
      entrop_2 <- entrop_2 + proportion_c^2
    }
    proportion_g <- input$num_g/find_proportion()
    if (proportion_g != 0) {
      entrop_2 <- entrop_2 + proportion_g^2
    }
    proportion_t <- input$num_t/find_proportion()
    if (proportion_t != 0) {
      entrop_2 <- entrop_2 + proportion_t^2
    }
    
    return (1 - entrop_2)
  }
  
  calculate_1H <- function() {
    entrop_1 <- 0
    
    proportion_a <- input$num_a/find_proportion()
    if (proportion_a != 0) {
      entrop_1 <- entrop_1 + (proportion_a * log(proportion_a, base = exp(1)))
    }
    
    proportion_c <- input$num_c/find_proportion()
    if (proportion_c != 0) {
      entrop_1 <- entrop_1 + (proportion_c * log(proportion_c, base = exp(1)))
    }
    
    proportion_g <- input$num_g/find_proportion()
    if (proportion_g != 0) {
      entrop_1 <- entrop_1 + (proportion_g * log(proportion_g, base = exp(1)))
    }
    
    proportion_t <- input$num_t/find_proportion()
    if (proportion_t != 0) {
      entrop_1 <- entrop_1 + (proportion_t * log(proportion_t, base = exp(1)))
    }
    
    entrop_1 <- -1 * entrop_1
  }
  
  
  find_proportion <- function() {
    return (input$num_a + input$num_c + input$num_g + input$num_t)
  }
  
  # Determine number of alleles in each sample
  find_S <- function() {
    count_S = 0
    if (input$num_a != 0) {
      count_S = count_S + 1
    }
    if (input$num_c != 0) {
      count_S = count_S + 1
    }
    if (input$num_g != 0) {
      count_S = count_S + 1
    }
    if (input$num_t != 0) {
      count_S = count_S + 1
    }
    return(count_S)
  }
  
  output$single_plot <- renderPlotly({
    
    profiles = c('q=0', 'q=1', 'q=2')
    
    if (find_S()-1 >= 0) {
      values =  calculate_qdProfile(input$num_a, 
                                    input$num_c, 
                                    input$num_g, 
                                    input$num_t) 
      curr_plot <- plot_ly(x = profiles,
                           y = values, 
                           type = 'bar', 
                           text = values, 
                           textposition = 'auto', 
                           name = 'q-Profile')
      
      curr_plot <- curr_plot %>% layout(title = 'Q-Profile of Sample',
                                        yaxis = list(title = '<sup>q</sup>D Value')) 
      
    }
    
    
  })
  
  # BLAH BLAH
  observeEvent(input$view_haplo_pop, {
    if (input$haplo_pop_type == "Select Population") {
      supplied = FALSE
      pop = input$sample_haplo_pop
    }
    else {
      if (is.null(input$user_haplo_pop)){
        shinyFeedback::showFeedbackDanger(
          inputId = 'user_haplo_pop',
          text = 'Please provide a File'
        )
        return()
      }
      if (check_fasta_file(input$user_haplo_pop) == FALSE){
        shinyFeedback::showFeedbackWarning(
          inputId = 'user_haplo_pop',
          text = 'File is not in Fasta Format'
        )
        return()
      }
      hideFeedback('user_haplo_pop')
      supplied = TRUE
      population = input$user_haplo_pop
      pop = population$name
    }
    showModal(modalDialog(
      title = paste("Population Overview of", pop),
      easyClose = FALSE,
      plotOutput("view_pop_plot", hover = "population_data"),
      footer = tagList(
        if(supplied == FALSE) downloadButton("view_file", "Download Fasta File"),
        actionButton("exit_view_modal", "Close")
      )
    ))
    
    output$view_pop_plot <-renderPlot({
      if (input$haplo_pop_type == "Select Population") {
        supplied = FALSE
        pop = input$sample_haplo_pop
      }
      else {
        if (is.null(input$user_haplo_pop)){
          shinyFeedback::showFeedbackDanger(
            inputId = 'user_haplo_pop',
            text = 'Please provide a File'
          )
          return()
        }
        if (check_fasta_file(input$user_haplo_pop) == FALSE){
          shinyFeedback::showFeedbackWarning(
            inputId = 'user_haplo_pop',
            text = 'File is not in Fasta Format'
          )
          return()
        }
        hideFeedback('user_haplo_pop')
        supplied = TRUE
        pop = input$user_haplo_pop
        pop = pop$datapath
      }
      data <- get_population_data(pop, supplied)
      data <- data[-1]
      seqs <- names(data)
      values <- as.numeric(as.character(data))
      num = length(values)
      if (num < 3) num = 3
      cols <- brewer.pal(num, "BuGn")
      barplot(values, names= seqs, xlab = "Sequence", ylab = "Count",
              main = "Population Genotypes", col = cols)
    })
    
    output$view_file <- downloadHandler(
      filename = function() {
        paste('data', input$sample_haplo_pop, '.fasta', sep = '')
      },
      content = function(file){
        write.table(get_fasta(input$sample_haplo_pop), file, row.names = FALSE, 
                    col.names = FALSE, quote = FALSE)
      }
    )
    
    # Finicky
    
    observeEvent(input$exit_view_modal, {
      
      removeModal()
    })
  })
  
  observeEvent(input$btn_gen_pop,{
    x_val <- c('q=0', 'q=1', 'q=2')
    
    q_profile <- qdProfile_for_list(population_list)
    
    output$comparative_haplo_plot <- renderPlotly({
      x_labels <- c('q=0', 'q=1', 'q=2')
      comp_plot <- plot_ly(x = x_labels,
                           y = unlist(q_profile[1,]),
                           type = 'bar',
                           textposition = 'auto',
                           orientation = 'v',
                           name = 'sample_1'
      )
      
      
      
      for(i in 2:nrow(q_profile)) {
        comp_plot <- add_trace(comp_plot,
                               x = x_labels,
                               y = unlist(q_profile[i,]),
                               type = 'bar',
                               orientation = 'v',
                               name = paste0('sample_', i)
        )
      }
      comp_plot
      comp_plot <- comp_plot %>% layout(title = 'Q-Profile of Samples',
                                        yaxis = list(title = '<sup>q</sup>D Value'))
      
    })
    
    
  })
  
  # Functionality for reset button 
  observeEvent(input$btn_reset_pop, {
    if (exists("population_list")) {
      population_list <<- NULL
      session$reload()
    }
  })
  
  # Function for resetting population_list 
  resetPopulationList <-function() {
    population_list <<- NULL
    session$reload()
  }
  
  # Remove population_list when app closes
  onStop(resetPopulationList)
  
  savePopData <- function(pop_name, type) {
    
    data <- data.frame(pop_name = text, type = integer)
    data <- rbind(data, data.frame(pop_name, type))
    if (exists("population_list")) {
      population_list <<- rbind(population_list, data)
    } else {
      population_list <<- data
    }
  }
  
  loadPopData <- function() {
    if (exists("population_list")) {
      population_list
    }
  }
  
  output$population_list <- DT::renderDataTable({
    input$btn_add_pop
    loadPopData()
  })
  
  # Whenever a field is filled, aggregate all form data
  # When the Add button is clicked, save the form data
  observeEvent(input$btn_add_pop, {
    if (input$haplo_pop_type == "Select Population") {
      supply_type <- FALSE # FALSE
      name <- input$sample_haplo_pop
    }
    else {
      if (is.null(input$user_haplo_pop)){
        shinyFeedback::showFeedbackDanger(
          inputId = 'user_haplo_pop',
          text = 'Please provide a File'
        )
        return()
      }
      if (check_fasta_file(input$user_haplo_pop) == FALSE){
        shinyFeedback::showFeedbackWarning(
          inputId = 'user_haplo_pop',
          text = 'File is not in Fasta Format'
        )
        return()
      }
      hideFeedback('user_haplo_pop')
      supply_type <- TRUE # TRUE
      name <- input$user_haplo_pop
      name <- name$datapath
    }
    
    savePopData(name, supply_type)
    
  })
  
  output$helpText <- renderUI({
    withMathJax(
      div( class = 'helptext_container', tags$h2('Method and equations: '),
           helpText('The method employed uses \\(^qD\\)-value as a measure of genetic diversity and further uses multiple \\(^qD\\)-values to reflect differences between rare or common variants.'), 
           helpText('The \\(^qD\\)-value graphically compared form the q-profile and this representation is most informative measure.'),
           helpText('To produce q-profiles, we require sample data for alleles.These samples 
               will be used to calculate the values for entropy qH and their number equivalents in \\(^qD\\).
               These equations can be used to provide a rich summary of diversity.Equations and further background can be found ', HTML(paste0(tags$a(href="https://reader.elsevier.com/reader/sd/pii/S0169534717302550?token=04B44342A3AF7F66DB3A74DB59D0F0433CC25B931C23705DA9C4FE6D9960BDC22FD38AACC7D96BAB5E79C6D22C65FAC9", "here.")))),
           helpText('The equations for calculating Entropy \\(^qH\\) are:'),
           helpText('$$^0H=S-1$$'),
           helpText('$$^1H=-\\sum_{i=1}^Sp_ilnp_i$$'),
           helpText('$$^2H=1-\\sum_{i=1}^Sp_i^2$$'),
           helpText('where \\(S\\) is the number of alleles and \\(p_i\\) is the allelic proprotion for \\(A\\), \\(C\\), \\(G\\) or \\(T\\)'),
           helpText('The equations for calculating their equivalents in \\(^qD\\) are:'),
           helpText('$$^0D=S$$'),
           helpText('$$^1D=e^{1_H}$$'),
           helpText('$$^2D=1/(1-{^2H})$$')
      )
    )
  })
  
  loopResponse <- function() {
    
    
    # Loop through rows to calculate entropy for each sample
    entropy_values <- data.frame(entropy_0 = vector(), entropy_1 = vector(), entropy_2 = vector())
    for (row in 1:nrow(responses)) {
      
      # calculate their entropy values
      entrop_0 <- calculate_entropy_0(row)
      entrop_1 <- calculate_entropy_1(row)
      entrop_2 <- calculate_entropy_2(row)
      
      # store entropy values
      curr <- data.frame(entrop_0, entrop_1, entrop_2)
      entropy_values <- rbind(entropy_values, curr)
      
    }
    
    # Append num_alleles to entropy values
    entropy_values$num_alleles <- calculate_num_alleles()
    
    # Sort entropy_values by num_alleles
    entropy_values <- entropy_values[order(entropy_values$num_alleles),]
    
    # Get dataframe of number of alleles in each sample
    alleles <- calculate_num_alleles()
    
    # Join the column
    entropy_values$num_alleles <- alleles
    
    
    # convert to q-profile values
    q_profile <- data.frame(qd_0 = vector(), qd_1 = vector(), qd_2 = vector())
    
    for (row in 1:nrow(entropy_values)) {
      
      # calculate qd-values
      qd_0 <- entropy_values[row, "num_alleles"]
      qd_1 <- exp(entropy_values[row, "entrop_1"])
      qd_2 <- 1/(1-entropy_values[row, "entrop_2"])
      
      # store as row in q_profile
      qd_values <- data.frame(qd_0, qd_1, qd_2)
      q_profile <- rbind(q_profile, qd_values)
    }
    
    output$SNP_comp_plot <- renderPlotly({
      x_labels <- c('q=0', 'q=1', 'q=2')
      comp_plot <- plot_ly(x = x_labels,
                           y = unlist(q_profile[1,]),
                           type = 'bar',
                           textposition = 'auto',
                           orientation = 'v',
                           name = 'sample_1'
      )
      comp_plot <- comp_plot %>% layout(title = 'Q-Profile of Samples',
                                        yaxis = list(title = '<sup>q</sup>D Value'))
      
      for(i in 2:nrow(q_profile)) {
        comp_plot <- add_trace(comp_plot,
                               x = x_labels,
                               y = unlist(q_profile[i,]),
                               type = 'bar',
                               orientation = 'v',
                               name = paste0('sample_', i)
        )
      }
      comp_plot
    })
    
    
  }
  
  # Determine number of alleles in each sample
  calculate_num_alleles <- function() {
    col_alleles <- vector() # CHANGE THIS TO A DATAFRAME SO WE CAN COMBINE TWO DATAFRAMES
    for (row in 1:nrow(responses)) {
      num_alleles <- 0
      for (base in base_fields) {
        if (responses[row, base] > 0) {
          num_alleles <- num_alleles + 1
        }
      }
      col_alleles <- c(col_alleles, num_alleles)
    }
    return(col_alleles)
  }
  
  # Calculate q=0
  calculate_entropy_0 <- function(row) {
    entrop_0 <- 0
    for (base in base_fields) {
      if (responses[row, base] != 0) {
        entrop_0 <- sum(entrop_0, 1)
      }
    }
    entrop_0 <- entrop_0 - 1
    return(entrop_0)
  }
  
  # Calculate q=1
  calculate_entropy_1 <- function(row) {
    entrop_1 <- 0
    for (base in base_fields) {
      proportion <- responses[row, base]/(input$num_a + input$num_c + input$num_g + input$num_t)
      if (proportion != 0) {
        entrop_1 <- entrop_1 + (proportion * log(proportion, base = exp(1)))
      }
    }
    entrop_1 <- -1 * entrop_1
    return(entrop_1)
  }
  
  # Calculate q=2
  calculate_entropy_2 <- function(row) {
    entrop_2 <- 0
    for (base in base_fields) {
      proportion <- responses[row, base]/(input$num_a + input$num_c + input$num_g + input$num_t)
      if (proportion != 0) {
        entrop_2 <- entrop_2 + proportion^2
        
      }
    }
    entrop_2 <- 1 - entrop_2 
    return(entrop_2)
  }
  
  observeEvent(input$btn_response, {
    loopResponse()
  })
  
  # Functionality for reset button 
  observeEvent(input$btn_reset, {
    if (exists("responses")) {
      responses <<- NULL
      session$reload()
    }
  })
  
  # Function for resetting responses 
  resetResponse <-function() {
    responses <<- NULL
    session$reload()
  }
  
  # Remove responses when app closes
  onStop(resetResponse)
  
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
      responses
    }
  }
  
  # When the Add button is clicked, save the form data
  observeEvent(input$btn_add, {
    saveData(formData())
  })
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(base_fields, function(x) input[[x]])
    data
  })
  
  # Show the previous responses
  # (update with current response when Add is clicked)
  output$responses <- DT::renderDataTable({
    input$btn_add
    loadData()
  })
  
  output$single_haplo_plot <- renderPlotly({
    if (input$haplo_pop_type == 'Select Population') {
      supplied = FALSE
      population = input$sample_haplo_pop
      
    } else {
      if (is.null(input$user_haplo_pop)){
        shinyFeedback::showFeedbackDanger(
          inputId = 'user_haplo_pop',
          text = 'Please provide a File'
        )
        return()
      }
      if (check_fasta_file(input$user_haplo_pop) == FALSE){
        shinyFeedback::showFeedbackWarning(
          inputId = 'user_haplo_pop',
          text = 'File is not in Fasta Format'
        )
        return()
      }
      hideFeedback('user_haplo_pop')
      
      supplied = TRUE
      population = input$user_haplo_pop
      population = population$datapath
      
    }
    
    if (!is.null(population)) {
      datdat <- get_population_data(population, supplied)
      datdat <- datdat[-1]
      
      sequences <- names(datdat)
      seq_count <- as.numeric(as.character(datdat))
      
      # Entropy q = 0
      entropy_0 <- length(sequences) - 1
      
      # Entropy q = 1
      entropy_1 <- 0
      
      for (count in seq_count) {
        proportion <- count/sum(seq_count)
        if (proportion != 0) {
          entropy_1 <- entropy_1 + (proportion * log(proportion, base = exp(1)))
        }
      }
      entropy_1 <- -1 * entropy_1
      
      # Entropy q = 2
      entropy_2 <- 0
      for (count in seq_count) {
        proportion <- count/sum(seq_count)
        if (proportion != 0) {
          entropy_2 <- entropy_2 + proportion^2
        }
      }
      entropy_2 <- 1 - entropy_2 
      
      
      # qD-value q = 0
      qd_0 <- length(seq_count)
      
      # qD-value q = 1
      qd_1 <- exp(entropy_1)
      
      # qD-value q = 2
      qd_2 <- 1/(1-entropy_2)
      
      y_val <- c(qd_0, qd_1, qd_2)
      x_val <- c('q=0', 'q=1', 'q=2')
      
      haplo_plot <- plot_ly(
        x = x_val, y = y_val, type = 'bar', text = y_val, textposition = 'auto'
      )
      
      haplo_plot <- haplo_plot %>% layout(title = 'Q-Profile of Sample',
                                          yaxis = list(title = '<sup>q</sup>D Value'))
    }
    
  })
  
  
  
}

shannon_callback <- function(input, output, session){
  
  output$equation <- renderUI({
    withMathJax(
      tags$h2("Method and equations used"),
      helpText('The equations and its background can ONLY be found in this', HTML(paste0(tags$a(href="https://link.springer.com/article/10.1007/s12686-018-1079-z", "paper.")))),
      helpText('It is suggested that Shannon’s information is advantageous over heterozygosity as a means of determining genetic diversity, hence proving the development of Shannon’s information as a new tool for molecular ecology necessary. Whilst making a  comparison to heterozygosity, the new ‘Shan-Het’ equation can make predictions regarding the loss of genetic diversity when dealing with finite populations, as these populations pose the risk of losing genetic variability, ultimately effecting their survival.'),
      helpText('The Shan-Het equation is:'),
      helpText('$$^1H_t = ^1H_0(1 - \\frac{1}{2 \\times N_e})^t$$'),
      helpText('where \\(^1H_0=-\\sum_{i}^s{p_i ln (p_i)}\\) is base shannon 
            entropy, \\(N_e\\) is the population size and \\(t\\) is the number
            of generations. This equation can be used to asses the change in 
            genetic information over time.')
    )
  })
  
  observeEvent(input$settings,{
    showModal(modalDialog(
      title = "Update population parameter maxes",
      div(style="display:inline-block; vertical-align:top;",
          fluidRow(
            column(8, 
                   numericInput("ne_max", label = "Maximum Effective Population", 
                             value = 100)),
            column(4,
                   
                   actionButton("ne_max_but", label = "Update", size = "extra-small"))
          )),
          fluidRow(
            column(8, 
                   numericInput("t_max", label = "Maximum Number of Generations", 
                                value = 100)),
            column(4,
                 
                   actionButton("t_max_but", label = "Update", size = "extra-small"))
          )),
    )
    
    observeEvent(input$ne_max_but,{
      updateSliderInput(session, inputId = 'Ne', max = input$ne_max)
      updateSliderInput(session, inputId = 'graph_ne', max = input$ne_max)
    })
    
    observeEvent(input$t_max_but, {
      updateSliderInput(session, inputId = 't', max = input$t_max)
    })
  })
  
  observeEvent(input$user_file,
               shinyFeedback::feedbackSuccess(
                 "user_file",
                 check_fasta_file(input$user_file) == TRUE,
                 "Upload success"
               ))
  
  observeEvent(input$user_file,
               shinyFeedback::feedbackWarning(
                 "user_file",
                 is.null(input$user_file),
                 "please provide a file"
               ))
  
  observeEvent(input$user_file,
               shinyFeedback::feedbackWarning(
                 "user_file",
                 check_fasta_file(input$user_file) == FALSE,
                 "File is not a Fasta File"
               ))
  
  observeEvent(input$view_pop, {
    if (input$population_type == "Select Population") {
      supplied = FALSE
      pop = input$population
    }
    else {
      if (is.null(input$user_file)){
        shinyFeedback::showFeedbackDanger(
          inputId = 'user_file',
          text = 'Please provide a File'
        )
        return()
      }
      hideFeedback('user_file')
      if (check_fasta_file(input$user_file) == FALSE){
        shinyFeedback::showFeedbackWarning(
          inputId = 'user_file',
          text = 'File is not in Fasta Format'
        )
        return()
      }
      hideFeedback('user_file')
      supplied = TRUE
      population = input$user_file
      pop = population$name
    }
    showModal(modalDialog(
      title = paste("Population Overview of", pop),
      easyClose = FALSE,
      plotOutput("pop_plot", hover = "population_data"),
      footer = tagList(
        if(supplied == FALSE) downloadButton("view_file", "Download Fasta File"),
        actionButton("exit_modal", "Close")
      )
    ))
  })
  
  output$pop_plot <-renderPlot({
    if (input$population_type == "Select Population") {
      supplied = FALSE
      pop = input$population
    }
    else {
      supplied = TRUE
      pop = input$user_file
      pop = pop$datapath
    }
    data <- get_population_data(pop, supplied)
    data <- data[-1]
    seqs <- names(data)
    values <- as.numeric(as.character(data))
    num = length(values)
    if (num < 3) num = 3
    cols <- brewer.pal(num, "BuGn")
    barplot(values, names= seqs, xlab = "Sequence", ylab = "Count",
         main = "Population Genotypes", col = cols)
  })
  
  output$view_file <- downloadHandler(
    filename = function() {
      paste('data', input$population, '.fasta', sep = '')
    },
    content = function(file){
      write.table(get_fasta(input$population), file, row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
    }
  )
  
  observeEvent(input$exit_modal, {
    removeModal()
  })
  
  observeEvent(input$btn_calculate, {
    if (input$analysis_mode == 'Single Population'){
      if (input$population_type == "Select Population") {
        supplied = FALSE
        population = input$population
        pop_name = population
      }
      else {
        if (is.null(input$user_file)){
          shinyFeedback::showFeedbackDanger(
            inputId = 'user_file',
            text = 'Please provide a File'
          )
          return()
        }
        hideFeedback('user_file')
        if(is.null(input$user_file)||check_fasta_file(input$user_file) == FALSE) return()
        supplied = TRUE
        population = input$user_file
        pop_name =population$name
        population = population$datapath
      }
      range = input$graph_ne
      t <- c(range[1]:range[2])
      y <- sapply(range[1]:range[2], calculate_shannon, population = population, 
                  ne = as.integer(input$Ne), user_supplied = supplied)
      shanhet <- calculate_shannon(population, as.integer(input$Ne),
                                   as.integer(input$t), user_supplied = supplied)
      graph <- plot(t,y, xlab = "Generations", ylab = "Shan-Het value")
      
      info <- data.frame(
        Population = pop_name,
        Size = input$Ne,
        Generations = input$t,
        Shanhet = shanhet
      )
      
      output$filled_equation <- renderUI({
        withMathJax(
          helpText('\\(^1H_0(1 - \\frac{1}{2 * ',input$Ne, '})^{',input$t,'} = \\)', 
                   calculate_shannon(population, as.integer(input$Ne),
                                     as.integer(input$t), user_supplied = supplied))
        )
      }) 
      
     
      output$plot <- renderPlotly({
        t <- c(range[1]:range[2])
        y <- sapply(range[1]:range[2], calculate_shannon, population = population, 
                    ne = as.integer(input$Ne), user_supplied = supplied)
        shanhet <- calculate_shannon(population, as.integer(input$Ne),
                                     as.integer(input$t), user_supplied = supplied)
        #plot(t,y, xlab = "Generations", ylab = "Shan-Het value")
        plot_ly( 
                x = range[1]:range[2], 
                y = y, 
                type = 'scatter', 
                mode = 'line',
                name = "name")
      })
      
      output$download_but <- renderUI({
        list(
          br(),
          h2("Download results"),
          checkboxInput('include_pop', 'Include Population count', value = FALSE),
          checkboxInput('include_graph', 'Include Graph', value = FALSE),
          uiOutput("graph_range"),
          radioButtons('format', 'Document format', c('HTML', 'Word'),
                       inline = TRUE),
          downloadButton("download", "Download")
        )
        
      })
      
      output$graph_range <-renderUI ({
        if(!input$include_graph)return ()
        sliderInput("range", "Generations Range:",min = 0, 
                    max = 100, value = c(1,100))
      })
      
      output$download <- downloadHandler(
        filename = function() {
          paste('my-report', sep = '.',switch(
            input$format, HTML = 'html', Word = 'word'
          ))
        },
        content = function(file) {
          values=NULL
          seqs=NULL
          cols=NULL
          range = input$range
          t <- c(range[1]:range[2])
          y <- sapply(range[1]:range[2], calculate_shannon, population = population, 
                      ne = as.integer(input$Ne), user_supplied = supplied)
          if (input$include_pop == TRUE){
            if (input$population_type == "Select Population") {
              supplied = FALSE
              pop = input$population
            }
            else {
              supplied = TRUE
              pop = input$user_file
            }
            data <- get_population_data(pop, supplied)
            data <- data[-1]
            seqs <- names(data)
            values <- as.numeric(as.character(data))
            num = length(values)
            if (num < 3) num = 3
            cols <- brewer.pal(num, "BuGn")
            
          }
          
          src <- normalizePath('report.RMD')
          
          # temporarily switch to the temp dir, in case you do not have write
          # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          
          file.copy(src, 'report.RMD', overwrite = TRUE)
          
          library(rmarkdown)
          out <- rmarkdown::render('report.RMD', 
            params = list(
              info = info,
              t = t,
              y = y,
              pop = input$include_pop,
              graph = input$include_graph,
              values=values,
              names=seqs,
              cols=cols
            ),
            switch(
              input$format, 
              HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
    }
    else {
      if (!input$population_type == "Select Population"){
        if (is.null(input$user_file)){
          shinyFeedback::showFeedbackDanger(
            inputId = 'user_file',
            text = 'Please provide a File'
          )
          return()
        }
        hideFeedback('user_file')
        if(is.null(input$user_file)||check_fasta_file(input$user_file) == FALSE) return()
      }
      
      number_pops$a = number_pops$a +1
      saveData(formData())
    }
  })
  
  table_labels <- c("supplied", "file", "t", "Ne", "shan")
  
  reset_response <-function() {
    profiles <<- NULL
    session$reload()
  }
  
  observeEvent(input$remove_all,{
    if (exists("profiles")){
      reset_response()
      number_pops$a=0
    }
  })
  
  onStop(reset_response)
  
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("profiles")) {
      profiles <<- rbind(profiles, data)
    } else {
      profiles <<- data
    }
  }
  
  loadData <- function() {
    if (exists("profiles")) {
      profiles[,c("file", "t", "Ne", "shan")]
    }
  }
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    if (input$population_type == "Select Population") {
      shanhet <-calculate_shannon(input$population, 
                                  as.integer(input$Ne),
                                  as.integer(input$t), 
                                  user_supplied = FALSE)
      values = c(supplied=FALSE, file=input$population, 
                 t=input$t, Ne=input$Ne, shan = shanhet)
    }
    else {
      population = input$user_file
      shanhet <-calculate_shannon(population$datapath, 
                                  as.integer(input$Ne),
                                  as.integer(input$t), 
                                  user_supplied = TRUE)
      values = c(supplied=TRUE, file = population$datapath, 
                 t=input$t, Ne=input$Ne, shan = shanhet)
      
    }
    data <- sapply(table_labels, function(x) values[[x]])
    data
  })
  
  output$profiles <- DT::renderDataTable({
    input$btn_calculate
    loadData()
  })
  
  observeEvent(input$get_graphs,{
    print(profiles)
    if (is.null(profiles)) return()
    range = input$graph_ne
    t <- c(range[1]:range[2])
    shan_values <-data.frame()
    for(row in 1:nrow(profiles)){
      y <- sapply(range[1]:range[2], calculate_shannon, 
                  population = profiles[row, "file"], 
                  ne = as.integer(profiles[row, "Ne"]), 
                  user_supplied = profiles[row, "supplied"])
      value <- data.frame(t(sapply(y,c)))
      shan_values <- rbind(shan_values, value)
    }
    
    output$comparative_plot <- renderPlotly({
      name <- paste0(profiles[1,"file"], " where Ne = ", profiles[1, "Ne"])
      multi_plot <- plot_ly(data = shan_values, 
                            x = range[1]:range[2], 
                            y = unlist(shan_values[1,]), 
                            type = 'scatter', 
                            mode = 'line',
                            name = name)
        multi_plot <- layout(multi_plot, 
                             xaxis = list(title = "Generation"),
                             yaxis = list(title = "Shan-Het Value"))
        row <-2
        while(row <= nrow(shan_values)){
          name <- paste0(profiles[row,"file"], " where Ne = ", profiles[row, "Ne"])
          multi_plot <- add_trace(multi_plot, 
                                  data = shan_values, 
                                  x = range[1]:range[2], 
                                  y = unlist(shan_values[row,]), 
                                  type = 'scatter', 
                                  mode = 'line',
                                  name = name)
          row <- row+1
        }
        multi_plot
    })
    
    output$download_info <- renderUI({
      list(
        h2("Download results"),
        checkboxInput('include_table', 'Include Table data', value = FALSE),
        checkboxInput('include_multi_graph', 'Include Graph', value = FALSE),
        uiOutput("graph_range2"),
        radioButtons('format2', 'Document format', c('HTML', 'Word'),
                     inline = TRUE),
        downloadButton('download_multi', 'Download Results')
      )
    })
    
    output$graph_range2 <-renderUI ({
      if(!input$include_multi_graph)return ()
      sliderInput("range2", "Generations Range:",min = 0, 
                  max = 100, value = c(1,100))
    })
    
    output$download_multi <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.',switch(
          input$format2, HTML = 'html', Word = 'word'
        ))
      },
      content = function(file) {
        profiles = profiles
        range = input$range2
        t <- (1:2)
        t <- (range[1]:range[2])
        shan_values <-data.frame()
        for(row in 1:nrow(profiles)){
          y <- sapply(range[1]:range[2], calculate_shannon, 
                      population = profiles[row, "file"], 
                      ne = as.integer(profiles[row, "Ne"]), 
                      user_supplied = profiles[row, "supplied"])
          value <- data.frame(t(sapply(y,c)))
          shan_values <- rbind(shan_values, value)
        }
        
        
        src <- normalizePath('multiplot.RMD')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        file.copy(src, 'multiplot.RMD', overwrite = TRUE)
        
        library(rmarkdown)
        out <- rmarkdown::render('multiplot.RMD', 
                                 params = list(
                                   profiles = profiles,
                                   shan_values = shan_values,
                                   t=t,
                                   table = input$include_table,
                                   graph = input$include_multi_graph
                                 ),
                                 switch(
                                   input$format2, 
                                   HTML = html_document(), Word = word_document()
                                 ))
        file.rename(out, file)
      }
    )
    
  })
  
}

population_callback <-function(input, output, session){
  
  # Even distribution
  output$evenDistribution_plot <- renderPlot({
      data <- get_population_data('evenDistribution', FALSE)
      data <- data[-1]
      seqs <- names(data)
      values <- as.numeric(as.character(data))
      num = length(values)
      if (num < 3) num = 3
      cols <- brewer.pal(num, "BuGn")
      barplot(values, names= seqs, xlab = "Sequence", ylab = "Count",
              main = "Population Haplotypes", col = cols)
 })
  
  output$ed_download <- downloadHandler(
    filename = function() {
      'evenDistribution.fasta'
    },
    content = function(file){
      write.table(get_fasta('evenDistribution'), file, row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
    }
  )
  
  # Random distribution
  output$randomDistribution_plot <- renderPlot({
    supplied = FALSE
    data <- get_population_data('randomDistribution', supplied)
    data <- data[-1]
    seqs <- names(data)
    values <- as.numeric(as.character(data))
    num = length(values)
    if (num < 3) num = 3
    cols <- brewer.pal(num, "BuGn")
    barplot(values, names= seqs, xlab = "Sequence", ylab = "Count",
            main = "Population Haplotypes", col = cols)
  })
  
  output$r_download <- downloadHandler(
    filename = function() {
      'randomDistribution.fasta'
    },
    content = function(file){
      write.table(get_fasta('randomDistribution'), file, row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
    }
  )
  # Single Outlier
  output$singleOutlier_plot <- renderPlot({
    supplied = FALSE
    data <- get_population_data('singleOutlier', supplied)
    data <- data[-1]
    seqs <- names(data)
    values <- as.numeric(as.character(data))
    num = length(values)
    if (num < 3) num = 3
    cols <- brewer.pal(num, "BuGn")
    barplot(values, names= seqs, xlab = "Sequence", ylab = "Count",
            main = "Population Haplotypes", col = cols)
  })
  
  output$s_download <- downloadHandler(
    filename = function() {
      'singleOutlier.fasta'
    },
    content = function(file){
      write.table(get_fasta('singleOutlier'), file, row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
    }
  )
  
  # uniform
  output$uniform_plot <- renderPlot({
    supplied = FALSE
    data <- get_population_data('uniform', supplied)
    data <- data[-1]
    seqs <- names(data)
    values <- as.numeric(as.character(data))
    num = length(values)
    if (num < 3) num = 3
    cols <- brewer.pal(num, "BuGn")
    barplot(values, names= seqs, xlab = "Sequence", ylab = "Count",
            main = "Population Haplotypes", col = cols)
  })
  
  output$u_download <- downloadHandler(
    filename = function() {
      'uniform.fasta'
    },
    content = function(file){
      write.table(get_fasta('uniform'), file, row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
    }
  )
}

build_callback <- function(input, output, session){
  
  observeEvent(input$original_seq, {
    shinyFeedback::feedbackWarning(
      "original_seq",
      !grepl("^[AGTCacgt]*$", input$original_seq),
      "Input must be a nucleotide sequence"
    )
  })
  
  sequences<-reactiveValues(
    input_info = list()
  )
  
  observeEvent(input$new, {
    print(input$filename)
    if (input$filename ==''){
      shinyFeedback::showFeedbackDanger(
        inputId = 'filename',
        text = 'Please enter a filename'
      )
      return ()  
    }
    hideFeedback('filename')
    if (input$original_seq ==''){
      shinyFeedback::showFeedbackDanger(
        inputId = 'original_seq',
        text = 'Please enter a Nucleotide Sequence'
      )
      return()
    }
    hideFeedback('original_seq')
    if ( !grepl("^[AGTCacgt]*$", input$original_seq)) return()
    count = isolate(length(sequences$input_info)) + 1
    varient = get_varient(input$original_seq, count)
    if (is.null(varient)) return()
    isolate({
      for (i in seq_along(sequences$input_info)){
        id <- sequences$input_info[[i]][1]
        sequences$input_info[[i]][3] <- input[[id]]
        tid <-sequences$input_info[[i]][2]
        sequences$input_info[[i]][4] <- input[[tid]]
      }
    })
      
    isolate({
      newid <- paste0('num', count)
      seq_id <- paste0('seq', count)
      sequences$input_info <-c(
        sequences$input_info, list(c(newid, seq_id, 1, varient))
      )
    })
    sequences
    
    
    
  })
  output$preview_pop <- renderPlot({
    if(input$original_seq != ''){
      values <- input$original_num
      seqs <- input$original_seq
      for (i in seq_along(sequences$input_info)){
        values <- c(values, as.integer(sequences$input_info[[i]][3]))
        seqs <- c(seqs, sequences$input_info[[i]][4])
      }
      barplot(values, names.arg = seqs, 
              main = 'Sample Population Distribution', ylab = 'Count', 
              xlab = 'Sequences')
    }
  })
  output$new_seq <-renderUI({
    lapply(sequences$input_info, function(a)
      fluidRow(
        column(6, textInput(a[2], label = "Nucleotide Sequence", value = a[4])), 
        column(6, numericInput(a[1], label = "Number of occurances",  value = a[3], min = 1))
      )
    )
  })
  
  output$gen <- downloadHandler(
    filename = function() {
      paste(input$filename, 'fasta', sep = '.')
    },
    content = function(file){
      string_content <- convert_to_fasta(input$filename, input$original_seq, 
                                         input$original_num, isolate(sequences$input_info))
      write.table(string_content, file, row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
    }
  )
  
  
  
  
}

# Creates router. We provide routing path, a UI as
router <- make_router(
  default = route("/", root_page, NA),
  route("populations", populations, population_callback),
  route("profiler", other_page, other_callback),
  route("Shannon", shannon_cal, shannon_callback),
  route("build_population", build_page, build_callback),
  route("how", how_to_page, NA),
  route("ourTeam", team_page, NA),
  page_404 = page404("You opened non existing bookmark!")
  
)

# Create User interface 
ui <- shinyUI(fluidPage(theme = "all_pages.css",
  useShinyFeedback(),
  router_ui()
))


# Plug router into Shiny server.
server <- shinyServer(function(input, output, session) {
  router(input, output, session)
  session$reload
})

# Run server in a standard way.
shinyApp(ui, server)

