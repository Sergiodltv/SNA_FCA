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
        "Text Mining",
        plotOutput("tiempo"),
        plotOutput("nummensajes"),
        plotOutput("longmensaje"),
        plotOutput("numpalabras"),
        plotOutput("palabrasusadas"),
        plotOutput("palabrasusadasuser"),
        plotOutput("emojisusados"),
        plotOutput("emojisusadosuser"),
        plotOutput("sentimientosemoji")
        
      ),
      tabPanel(
        "Reglas de Asociacion",
        verbatimTextOutput("arules")
      ),
      tabPanel(
        "FCA",
        tabsetPanel(
          tabPanel(
            "Conceptos",
            plotOutput("fc_conceptos", width = "150%", height = "800px")),
          tabPanel(
            "Implicaciones",
            verbatimTextOutput("fc_implicaciones")
          )
        )
      )
    )
  )
))
