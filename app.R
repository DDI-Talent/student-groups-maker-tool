options(
  repos = c(CRAN = "https://cloud.r-project.org")
)
library(bslib)
library(shiny)
# library(cpp11) # leave this here,  otherwise it does not get bundled and github actions fail
library(R6)
library(magrittr)
library(tibble)
library(dplyr)
library(tidyr)
source('pairs_engine/module.R')

tab_title <- 'Pair programming pairing app '
app_version <- '0.5.0'
app_title <- HTML(paste0(
  tab_title,
  span(paste0('v', app_version), style = 'font-size: .5em; color: #9B59B6')
))

ui <- page_fillable(
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
              p("or"),
              textAreaInput('pasteStudentNames', 'Paste comma separated student names to create a new classroom'),
              actionButton('createClassroomFromCopyPastedNames', 'Create New Classroom', class = "btn-success"),
            ),
          ),
        card(
          card_header("checkAttendance"),
            uiOutput('make_student_selection'),
            checkboxGroupInput(
              "checkboxes_with_attendance",
              "Attendance:",
              c(
              )
            ),
            textInput('name_of_session', 'Name your session:', value="unnamed_session"),
            numericInput( "groupSize","Group Size:",value = 2,min = 1,max = 20),
            actionButton('make_groups_from_attendance', 'Make Groups!', class = "btn-success"),

            # tableOutput('loaded_history')
          ),

        card("Groups For Today",
             tableOutput('generated_groups_for_session'),
             uiOutput('accept_and_download_button')
          )
        )

)


server <- function(input, output, session) {
  source("./pairs_engine/module.R")
  pairMaker <- reactiveVal( PairMaker$new() )

  # loadDataFromFile <- observe({
  #   print("loadDataFromFile 1")
  #   req(input$class_history_file)
  #   pairMaker()$load_history_from_file(input$class_history_file$datapath, input$class_history_file$name)
  # })

  observe({
    output$loaded_history <- renderTable({
      req(input$class_history_file$datapath)
      pairMaker()$history
    })
  })

  output$accept_and_download_button <- renderUI({
    req(input$make_groups_from_attendance)
    hideDownloadButton <- is.null(pairMaker()$most_recent_candidate)
    if(hideDownloadButton){
      div()
    }else{
      downloadButton('accept_groups_for_session', 'Accept Groups and Download!', class = "btn-success")
    }
  })


  # enableDownloadOnceGroupsAreCreated <- observe({
  #   print("enableDownloadOnceGroupsAreCreated 1")
  #   req(input$make_groups_from_attendance) # react to button pressed
  #   if(!is.null(pairMaker()$most_recent_candidate )){
  #     updateActionButton(accept_groups_for_session, disabled = FALSE)
  #     print("make_groups_from_attendance 3")
  #   }
  # })


  output$generated_groups_for_session <- renderTable({
    # req(input$class_history_file$datapath)
    pairMaker()$history
  })


  updateAttendanceOptionsTriggerFile <- observe({
    req( input$class_history_file$datapath)
    # print("updateAttendanceOptions 1a")
    pairMaker()$load_history_from_file(input$class_history_file$datapath, input$class_history_file$name)
    updateAttendanceOptions()
  })

  updateAttendanceOptionsTriggerText <- observe({
    req( input$createClassroomFromCopyPastedNames)
    # print("updateAttendanceOptions 1b")
    pairMaker()$innitialise_class(input$pasteStudentNames)
    updateAttendanceOptions()
  })


  updateAttendanceOptions <- function(){
    all_potential_attendees <- pairMaker()$get_all_potential_attendees()
    # print("updateAttendanceOptions 3")
    updateCheckboxGroupInput(session, "checkboxes_with_attendance",
                             choices = all_potential_attendees,
                             selected = c()
    )
  }

  updateSessionName <- observe({
    req(input$class_history_file$datapath)
    updateTextInput(session, "name_of_session",
                             value = history_operations$session_name_with_format("unnamed_session")
    )
  })


  createNewPairs <- observe({
    # print("make_groups_from_attendance 1")
    req(input$make_groups_from_attendance) # react to button pressed
    if(!is.null(input$checkboxes_with_attendance)){
      new_pairs <- pairMaker()$make_pairs_for_session(input$checkboxes_with_attendance, input$name_of_session, group_size = input$groupSize)
      # print("make_groups_from_attendance 3")
    }
  })


  # createNewClassroom <- observe({
  #   print("createClassroomFromCopyPastedNames 1")
  #   req(input$createClassroomFromCopyPastedNames) # react to button pressed
  #   pairMaker()$innitialise_class(input$pasteStudentNames)
  #   print("createClassroomFromCopyPastedNames 2")
  #   print(pairMaker()$history)
  #
  # })




  output$generated_groups_for_session <- renderTable({
    req(input$make_groups_from_attendance) # react to button pressed
    invalidateLater(3000)
#     UGH! TODO. this is a nasty hack. make it properly by making hostory reactive within the object
    pairMaker()$most_recent_candidate
  })

  output$accept_groups_for_session <- downloadHandler(
    filename = function() {
      pairMaker()$filename
    },
    content = function(con) {
      pairMaker()$approve_most_recent_groups_candidate()
      pairMaker(pairMaker())
      write.csv(pairMaker()$history, con)
    }
  )
  # checkboxInput("checkbox", "Checkbox", FALSE),


  # shinyjs::disable(selector = "a[data-value='pairing_tab']")

  # present_students <- selectAttendingStudents('presence_tab')

  #  observe({
  #   pp_sess <- present_students()$pp_session
  #   attendance_complete <- is.null(pp_sess$week) || is.null(pp_sess$session) || nzchar(pp_sess$session)
  #  if (attendance_complete) {
  #     shinyjs::enable(selector = "a[data-value='pairing_tab']")
  #   } else {
  #     shinyjs::disable(selector = "a[data-value='pairing_tab']")
  #   }
  # })

  # pairPresentStudents('pairing_tab', present_students)
}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = 7909))
