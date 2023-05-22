library(shiny)

shinyUI(fluidPage(
  titlePanel(
    "Analiza tu chat de WhatsApp",
    windowTitle = "Social Network Analysis con WhatsApp"
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Cargar chat",
        fileInput(
          "archivo",
          "Selecciona un archivo:"
        ),
        dataTableOutput("chat")
      ),
      tabPanel(
        "FCA",
        plotOutput("fc_conceptos", width = "150%", height = "800px"),
        verbatimTextOutput("fc_implicaciones")
      ),
      tabPanel(
        "Text Mining",
        plotOutput("nummensajes"),
        plotOutput("longmensaje"),
        plotOutput("emojisusados"),
        plotOutput("emojisusadosuser"),
        plotOutput("palabrasusadas"),
        plotOutput("palabrasusadasuser")
      ),
      tabPanel(
        "Reglas de Asociacion",
        verbatimTextOutput("arules")
      )
    )
  )
))
