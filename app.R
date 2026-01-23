library(dplyr)
library(shiny)
library(bslib)
library(rclipboard)
library(here)
source(here::here('pairs_engine', 'Pair_maker.R'))


tab_title <- 'Pairs Maker: Pair Programming & Groupwork'
app_version <- '0.6.7'
app_title <- HTML(paste0(
  tab_title,
  span(paste0('v', app_version), style = 'font-size: .5em; color: #9B59B6')
))

ui <- page_fillable(
  shinyjs::useShinyjs(),
  rclipboard::rclipboardSetup(),
  title = tab_title,
  titlePanel(app_title),
  tags$head(tags$link( rel = "stylesheet", type = "text/css", href = "style.css")),
      layout_columns(
        card(
          card_header("Load Classroom Data"),
            header="Load your classroom history or create classroom:",
            tagList(
              fileInput(
                'class_history_file', 'Choose Attendance File', accept = '.csv'),
              p("--- and/or also: ---"),
              textAreaInput('paste_student_names_add', 'Paste comma separated student names to ADD TO or CREATE a classroom'),
              actionButton('add_to_classroom_from_copy_pasted_names', 'Add New Students', class = "btn-success"),
            ),
          ),
        card(
          card_header("Check Attendance"),
          fluidRow(
            column(6,actionLink("select_all_attending", "Tick all as Present")),
            column(6,actionLink("select_all_not_attending", "Tick all as Absent"))
          ),
          checkboxGroupInput(
              "checkboxes_with_attendance",
              "Attendance:",
              c(
              )
            ),
            textInput('name_of_session', 'Name your session:', value="unnamed-session"),
            numericInput( "group_size","Group Size:",value = 2,min = 1,max = 20),
            actionButton('make_groups_from_attendance', 'Make Groups!', class = "btn-success"),

          ),

        card("Get Generated Groups",
             checkboxInput("simplified_pairs_display", "Show Simplified pairs (without size, fitness nor time)", TRUE),
             tableOutput('generated_groups_for_session'),
             uiOutput('accept_and_download_button')
          )
        )
)


server <- function(input, output, session) {
  pair_maker <-  Pair_maker$new()

  output$accept_and_download_button <- renderUI({
    # req(input$make_groups_from_attendance)
    hideDownloadButton <- is.null(pair_maker$most_recent_candidate())
    if(hideDownloadButton){
      div()
    }else{
      downloadButton('accept_groups_for_session', 'Accept Groups and Download!', class = "btn-success")
    }
  })

  select_all_not_attendingAction <- observe({
    req( input$select_all_not_attending)
    all_potential_attendees <- pair_maker$get_all_potential_attendees()
    updateCheckboxGroupInput(session, "checkboxes_with_attendance",
                             choices = all_potential_attendees,
                             selected = c()
    )
  })

  select_all_attending_action <- observe({
    req( input$select_all_attending)
    all_potential_attendees <- pair_maker$get_all_potential_attendees()
    updateCheckboxGroupInput(session, "checkboxes_with_attendance",
                             choices = all_potential_attendees,
                             selected = all_potential_attendees
    )
  })

  update_attendance_options_trigger_file <- observe({
    req( input$class_history_file$datapath)
    pair_maker$load_history_from_file(input$class_history_file$datapath, input$class_history_file$name)
    update_attendance_options()
  })

  add_attendance_options_trigger_text <- observeEvent(
    input$add_to_classroom_from_copy_pasted_names,
    {
      pair_maker$add_students(input$paste_student_names_add)
      update_attendance_options()
    }
  )

  update_attendance_options <- function(){
    all_potential_attendees <- pair_maker$get_all_potential_attendees()
    updateCheckboxGroupInput(session, "checkboxes_with_attendance",
                             choices = all_potential_attendees,
                             selected = c()
    )
  }

  updateSessionName <- observe({
    req(input$class_history_file$datapath)
    update_session_name()
  })

  update_session_name <- function(){
    updateTextInput(session, "name_of_session",
                    value = history_operations$session_name_with_format("unnamed-session")
    )
  }


  createNewPairs <- observeEvent(
    input$make_groups_from_attendance,
    {
      if(!is.null(input$checkboxes_with_attendance)){
        pair_maker$make_pairs_for_session(input$checkboxes_with_attendance, input$name_of_session, group_size = input$group_size)
      }
  })

  output$generated_groups_for_session <- renderTable({
    req(input$make_groups_from_attendance) # react to button pressed
    req( pair_maker$most_recent_candidate )
    pair_maker$get_most_recent_candidate(simplified = input$simplified_pairs_display)
  })

  output$accept_groups_for_session <- downloadHandler(
    filename = function() {
      print("pair_maker$filename")
      print(pair_maker$filename)
      pair_maker$filename
    },
    content = function(connection_to_downloaded_file) {
      update_session_name()
      pair_maker$approve_most_recent_groups_candidate()
      pair_maker$save_history_to_file(connection_to_downloaded_file)
      # write.csv(history_to_save, filename, row.names = FALSE)

      # print("connection_to_downloaded_file")
      # print(connection_to_downloaded_file)
      # write.csv(pair_maker$history, connection_to_downloaded_file, row.names = FALSE)
    }
  )
 }

# Run the application
shinyApp(ui = ui, server = server, options = list(port = 7909))
