# library(shiny)

if(!require(shiny)){
  install.packages("shiny", repos = "http://cran.us.r-project.org")
  library(shiny)
}

tab_title <- 'Pair programming pairing app '
app_version <- '0.5.0'


ui <- page_fillable(
  title = "tab_title",
  tags$head(tags$link( rel = "stylesheet", type = "text/css", href = "style.css")),

      layout_columns(
        card(
          card_header("Load Classroom Data"),
            header="Load your classroom history or create classroom:",
            tagList(
              textAreaInput('pasteStudentNames', 'Paste comma separated student names to create a new classroom'),
            ),
          ),
        )
)


server <- function(input, output, session) {
  print("@@@@@@@@@ server startd")

}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = 7909))
