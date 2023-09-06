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
    tabsetPanel(
      tabPanel(
        "Duración",
        plotOutput("tiempo"),
        downloadButton("descarga1", "Descargar gráfico")
        ),
      tabPanel(
        "Mensajes",
        plotOutput("nummensajes"),
        downloadButton("descarga2", "Descargar gráfico"),
        plotOutput("longmensaje"),
        downloadButton("descarga3", "Descargar gráfico")
      ),
      tabPanel(
        "Palabras",
        plotOutput("numpalabras"),
        downloadButton("descarga4", "Descargar gráfico"),
        plotOutput("palabrasusadas"),
        downloadButton("descarga5", "Descargar gráfico"),
        plotOutput("palabrasusadasuser"),
        downloadButton("descarga6", "Descargar gráfico")
      ),
      tabPanel(
        "Emojis",
        plotOutput("emojisusados", width = "50%"),
        downloadButton("descarga7", "Descargar gráfico"),
        plotOutput("emojisusadosuser", width = "50%"),
        downloadButton("descarga8", "Descargar gráfico")
      ),
      tabPanel(
        "Sentimientos",
        plotOutput("sentimientosemoji"),
        downloadButton("descarga9", "Descargar gráfico"),
        plotOutput("sentimientoslexico"),
        downloadButton("descarga10", "Descargar gráfico")
      ),
      tabPanel(
        "Emociones",
        plotOutput("emociones"),
        downloadButton("descarga11", "Descargar gráfico"),
        plotOutput("emocionesuser"),
        downloadButton("descarga12", "Descargar gráfico")
      )
    )  
  ),
  tabPanel(
    "Reglas de Asociacion",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        checkboxGroupInput("checkboxRA1", "Seleccione los usuarios sobre los que se crearan las reglas de asociación:",
                           choices = NULL)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Resumen",
            verbatimTextOutput("resumen")
          ),
          tabPanel(
            "Reglas",
            sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("checkboxRA2", "Seleccione los atributos sobre los que se filtraran las reglas de asociación:",
                                   choices = NULL)
              ),
              mainPanel(
                verbatimTextOutput("reglas")
              )
            )
          )
        )
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
            checkboxGroupInput("checkboxFCAConcepts", "Seleccione los usuarios sobre los que se crearan los conceptos de FCA:",
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
            width = 3,
            verbatimTextOutput("prueba"),
            checkboxGroupInput("checkboxFCAImplications", "Seleccione los usuarios sobre los que se crearan las implicaciones de FCA:",
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