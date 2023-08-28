library(shiny)
library(shinythemes)

shinyUI(navbarPage(
  title = "WhatsApp Chat Analysis",
  theme = shinytheme("united"),
  windowTitle = "Social Network Analysis con WhatsApp",
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
    plotOutput("sentimientosemoji"),
    plotOutput("sentimientoslexico"),
    plotOutput("emociones"),
    plotOutput("emocionesuser")
    
  ),
  tabPanel(
    "Reglas de Asociacion",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput("checkboxRA", "Seleccione los usuarios sobre los que se crearan las reglas de asociación:",
                           choices = NULL)
      ),
      mainPanel(
        verbatimTextOutput("arules")
      )
    )
  ),
  tabPanel(
    "FCA",
    tabsetPanel(
      tabPanel(
        "Conceptos",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            checkboxGroupInput("checkboxFCAConcepts", "Seleccione los usuarios sobre los que se crearan los conceptos e implicaciones de FCA:",
                               choices = NULL)
          ),
          mainPanel(
            plotOutput("fc_conceptos", width = "150%", height = "1500px")
          )
        )
      ),
      tabPanel(
        "Implicaciones",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            checkboxGroupInput("checkboxFCAImplications", "Seleccione los usuarios sobre los que se crearan los conceptos e implicaciones de FCA:",
                               choices = NULL)
          ),
          mainPanel(
            verbatimTextOutput("fc_implicaciones")
          )
        )
      )
    )
  )
))