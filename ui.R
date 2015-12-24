library(shiny)
shinyUI(fluidPage(theme = "boot.css",
  headerPanel("Text Compression"),
  sidebarPanel(
    textInput(inputId="text1", label = "Enter the string to be encoded"),
    numericInput('id1', 'Enter support',0),
    actionButton("goButton", "Go!")
  ),
  mainPanel(
    h3('Encoded Text'),
    verbatimTextOutput("encmessage"),
    h3('Code Table'),
    dataTableOutput('codetab'),
    h3('Decoded Text'),
    verbatimTextOutput("decmessage")
  )
))