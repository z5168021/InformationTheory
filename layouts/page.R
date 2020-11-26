library(shiny)
library(shiny.router)


#This generates menu in user interface with links.
menu <- (
  tags$ul(class = 'nav-bar',
    tags$li(class = 'dropdown', 
        tags$a(class='drop',href = route_link("/"), 'About ', tags$i(class='fa fa-caret-down')),
        tags$div(class = 'dropdown-content',
             a(class = "item", href = route_link("/"), "Background"),
             a(class = "item", href = route_link('how'), 'Instructional Videos'),
             a(class = "item", href = route_link("populations"), "Sample Populations"),
             a(class = 'item', href = route_link("ourTeam"), "Our Team")
    )),
    tags$li(class = 'nav', a(class = 'item', href = route_link('build_population'),'Build a population')),
    tags$li(class = 'nav', a(class = "item", href = route_link("profiler"), HTML(paste0(tags$sup("q"), "D profile")))),
    tags$li(class = 'nav', a(class = "item", href = route_link('Shannon'),"Variation Over Time")),
    
  )
)



# This creates UI for each page.
page <- function(title, content) {
  tags$script("MathJax.Hub.Config({
    tex2jax: {
    inlineMath: [['$','$'], ['\\(','\\)']],
    processEscapes: true
    }
    });"
  )
  fluidRow(theme = 'all_pages.css',
    div(class = 'header_panel',
        titlePanel( windowTitle = title, div(tags$img(class = 'header', src='unsw-logo.png', height = 56), title))        ),
    menu,
    div(tags$body(class = 'content', content)),

     div( class = 'footer_container', tags$footer("Copyright Team Sherwin 2020") )
  )
}
