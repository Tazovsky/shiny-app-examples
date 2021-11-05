library(shiny)

radioButtons <- function(id) {
  ns <- NS(id)
  uiOutput(ns("btns"))
}

radioButtonsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$btns <- renderUI({
        shiny::radioButtons(session$ns("btns"), "Radio choices", LETTERS[1:3], LETTERS[1])
      })
      list(session = session)
    }
  )
}

testModule <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    radioButtons(ns("radio"))
  )
}

testModuleServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      mod_out <- radioButtonsServer("radio")

      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      list(
        count = count,
        session = session,
        submodule = list(session = mod_out$session)
      )
    }
  )
}

ui <- fluidPage(
  fluidRow(
    testModule("test_module"),
    actionButton("updateRadios" , label = "Update radio buttons")
  )

)

server <- function(input, output, session) {

  module_out <- testModuleServer("test_module")

  observeEvent(input$updateRadios, {
    sub_session <- module_out$submodule$session

    updateRadioButtons(sub_session,
                       inputId = "btns", # this ID corresponds to `radioButtons`
                       selected = LETTERS[3])

  })

}

shinyApp(ui, server)
