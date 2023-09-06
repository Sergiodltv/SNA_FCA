library(shiny)
library(fcaR)
library(rwhatsapp)
library(dplyr)
library(lubridate)
library(tidyverse)
library(arules)
library(ggplot2)
library(ggimage)
library(stopwords)
library(tidytext)
library(textdata)
library(rvest)
library(knitr)
library(RColorBrewer)
library(syuzhet)

shinyServer(function(input, output, session) {
  
  #Cargamos y modificamos el dataframe del chat
  chat <- reactive({
    req(input$archivo)
    chat <- rwa_read(input$archivo$datapath)
    
    chat <- chat %>% 
      select(time, author, text, emoji, emoji_name)
    
    colnames(chat) <- c("fecha", "autor", "texto", "emoji", "nombre_emoji")
    
    chat <- chat %>% 
      mutate(multimedia = str_detect(texto, "omitid")) %>% 
      mutate(ncaracteres = nchar(texto))
    
    chat$texto[chat$texto == ""] <- NA
    chat$texto[str_detect(chat$texto, "Eliminaste")] <- NA
    chat$texto[str_detect(chat$texto, "Se eliminó")] <- NA
    chat$texto[str_detect(chat$texto, "cambió el asunto")] <- NA
    chat$texto[str_detect(chat$texto, "Se añadió")] <- NA
    chat$texto[str_detect(chat$texto, "Iniciaste una videollamada")] <- NA
    chat$texto[str_detect(chat$texto, "inició una videollamada")] <- NA
    chat$texto[str_detect(chat$texto, "Los mensajes y las llamadas están cifrados de extremo a extremo")] <- NA
    chat$texto[str_detect(chat$texto, "creó el grupo")] <- NA
    chat$texto[str_detect(chat$texto, "te añadió")] <- NA
    chat$texto[str_detect(chat$texto, "cambió el ícono")] <- NA
    chat <- na.omit(chat)
    
    chat$autor <- as.character(chat$autor)
    chat$fecha <- as.Date(chat$fecha)
    
    autor <- as.character(na.omit(unique(chat$autor)))
    
    num <- length(autor)
    
    for(i in 1:num){
      chat$autor[str_detect(chat$autor, autor[i])] <- str_c("Usuario ", i)
    }
    
    return(chat)
  })
  
  #Creamos un dataframe para los emojis
  chat_emoji <- reactive({
    chat <- chat() %>% 
      unnest(emoji) %>% 
      mutate(emoji = str_sub(emoji, end = 1)) %>% 
      count(autor, emoji, sort = TRUE) %>%
      arrange(desc(n)) %>% 
      mutate(emoji_url = map_chr(emoji, 
                                 ~paste0("https://abs.twimg.com/emoji/v2/72x72/",
                                         as.hexmode(utf8ToInt(.x)), ".png")))
    
    return(chat)
  })
  
  #Creamos un dataframe para los mensajes
  mensajes <- reactive({
    chat <- chat()
    
    mensajes <- chat %>% 
      group_by(autor) %>% 
      summarise(nummensajes = n(),
                longmensaje = mean(ncaracteres))
    
    autor <- as.character(na.omit(unique(chat$autor)))
    num <- length(autor)
    
    for(i in 1:num){
      if(is.na(match(str_c("Usuario ", i), mensajes$autor))){
        nueva_fila <- data.frame(autor = str_c("Usuario ", i),
                                 nummensajes = 0,
                                 longmensaje = 0)
        mensajes <- rbind(mensajes, nueva_fila)
      }
    }
    
    return(mensajes)
  })
  
  #Creamos un dataframe para saber el numero de emojis
  emojis <- reactive({
    chat <- chat()
    chat_emoji <- chat_emoji()
    
    emojis <- chat_emoji %>% 
      group_by(autor) %>% 
      summarise(num = sum(n))
    
    autor <- as.character(na.omit(unique(chat$autor)))
    num <- length(autor)
    
    for(i in 1:num){
      if(is.na(match(str_c("Usuario ", i), emojis$autor))){
        nueva_fila <- data.frame(autor = str_c("Usuario ", i),
                                 num = 0)
        emojis <- rbind(emojis, nueva_fila)
      }
    }
    
    return(emojis)
  })
  
  #Creamos un dataframe con la multimedia
  multimedia <- reactive({
    chat <- chat()
    
    multimedia <- chat %>% 
      group_by(autor) %>% 
      summarise(num = sum(multimedia))
    
    autor <- as.character(na.omit(unique(chat$autor)))
    num <- length(autor)
    
    for(i in 1:num){
      if(is.na(match(str_c("Usuario ", i), multimedia$autor))){
        nueva_fila <- data.frame(autor = str_c("Usuario ", i),
                                 num = 0)
        multimedia <- rbind(multimedia, nueva_fila)
      }
    }
    
    return(multimedia)
  })
  
  #Creamos un dataframe con el numero de palabras por autor
  numpalabras <- reactive({
    chat <- chat() %>%  
      unnest_tokens(input = texto, output = word) %>% 
      count(autor)
    
    return(chat)
  })
  
  #Creamos un elemento reactivo tipo lista para saber cuales son las palabras que no otorgan significado en nuestro chat
  palabras <- reactive({
    palabras <- c(stopwords("es"), "adjunto" , "2022", "audio", "opus",
                  "03", "04", "05", "11", "12", "14", "15", "13", "16", "17",
                  "imagen", "omitida", "omitido", "webp", "01", "02", "06", "07", "08", 
                  "09", "10", "18", "19", "20", "21", "22", "23", "24", "25", 
                  "26", "27", "28", "29", "00", "sticker", "photo", "jpg", "2021",
                  "30", "31", "33", "vm.tiktok.com", "puto", "puta", "pene", "cabrón", "pito")
    
    return(palabras)
  })
  
  #Creamos un dataframe para los sentimientos con emojis
  emoji_sentimientos <- reactive({
    chat <- chat()
    chat_emojis <- chat_emoji()
    
    url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
    doc <- read_html(url_base)
    
    tabla_emojis <- doc %>% 
      html_node("#myTable") %>% 
      html_table() %>% 
      as_tibble()
    
    sentimiento_emoji <- tabla_emojis %>% 
      select(1,6:9) %>% 
      set_names("char", "negativo","neutral","positivo","sent.score")
    
    sent_emojis <- chat_emojis %>% 
      inner_join(sentimiento_emoji, by=c("emoji"="char")) 
    
    sent_emojis_usuarios <- sent_emojis %>% 
      group_by(autor) %>% 
      summarise(positivo=mean(positivo),
                negativo=mean(negativo),
                neutral=mean(neutral),
                balance=mean(sent.score)) %>% 
      arrange(desc(balance))
    
    return(sent_emojis_usuarios)
  })
  
  #Creamos un dataframe con los sentimientos con lexico afinn
  sentlexico <- reactive({
    palabras <- palabras()
    
    lexico <- get_sentiments("afinn")
    
    chat <- chat() %>% 
      unnest_tokens(input = texto, output = palabra) %>% 
      filter(!palabra %in% palabras) %>% 
      select(autor, palabra) %>% 
      inner_join(lexico, by=c("palabra"="word")) %>% 
      count(autor, value) %>% 
      group_by(autor) %>% 
      mutate(mean=n/sum(n)) %>% 
      ungroup()
    
    chat <- chat %>% 
      mutate(mean = ifelse(value<0, -mean, mean)) %>% 
      group_by(autor) %>% 
      mutate(balance = sum(mean)) %>% 
      ungroup()
    
    return(chat)
  })
  
  #Creamos un dataframe con las emociones con lexico nrc
  emocioneslexico <- reactive({
    palabras <- palabras()
    
    lexico <- get_sentiments("nrc")
    
    chat <- chat() %>% 
      unnest_tokens(input = texto, output = palabra) %>% 
      filter(!palabra %in% palabras) %>%
      inner_join(lexico, by=c("palabra"="word")) %>% 
      filter(!sentiment %in% c("negative", "positive")) %>% 
      count(sentiment) %>% 
      arrange(desc(n))
    
    return(chat)
  })
  
  #Creamos un dataframe con las emociones con lexico nrc por usuario
  emocioneslexicousuario <- reactive({
    palabras <- palabras()
    
    lexico <- get_sentiments("nrc")
    
    chat <- chat() %>% 
      unnest_tokens(input = texto, output = palabra) %>% 
      filter(!palabra %in% palabras) %>%
      inner_join(lexico, by=c("palabra"="word")) %>% 
      filter(!sentiment %in% c("negative", "positive")) %>% 
      count(sentiment, autor, sort = TRUE) %>% 
      arrange(autor, desc(n))
    
    return(chat)
  })
  
  #Creamos un elemento reactivo para saber cuantos usuarios hay en el chat
  autores <- reactive({
    chat <- chat()
    
    autor <- as.character(na.omit(unique(chat$autor)))
    
    return(autor)
  })
  
  atributos <- reactive({
    return(colnames(df()))
  })
  
  #Metodo para hacer un checkbox con el numero de usuarios de cada chat cargado
  observe({
    autores <- autores()
    
    updateCheckboxGroupInput(session, "checkboxRA1", choices = autores)
  })
  
  observe({
    atributos <- atributos()
    
    updateCheckboxGroupInput(session, "checkboxRA2", choices = atributos)
  })
  
  observe({
    autores <- autores()
    
    updateCheckboxGroupInput(session, "checkboxFCAConcepts", choices = autores)
  })
  
  observe({
    autores <- autores()
    
    updateCheckboxGroupInput(session, "checkboxFCAImplications", choices = autores)
  })
  
  #Creamos un dataframe para poder aplicar arules y FCA
  df <- reactive({
    mensajes <- mensajes()
    emojis <- emojis()
    multimedia <- multimedia()
    numpalabras <- numpalabras()
    
    meanmensajes <- mean(mensajes$nummensajes)
    meanlongitud <- mean(mensajes$longmensaje)
    meanemojis <- mean(emojis$num)
    meanmultimedia <- mean(multimedia$num)
    meanpalabras <- mean(numpalabras$n)
    
    df <- data.frame(
      "Pocos.mensajes" = mensajes$nummensajes < meanmensajes,
      "Muchos.mensajes" = mensajes$nummensajes >= meanmensajes,
      "Pocas.palabras" = numpalabras$n < meanpalabras,
      "Muchas.palabras" = numpalabras$n >= meanpalabras,
      "Poca.longitud" = mensajes$longmensaje < 70,
      "Mucha.longitud" = mensajes$longmensaje >= 70,
      "Pocos.emojis" = emojis$num < meanemojis,
      "Muchos.emojis" = emojis$num >= meanemojis,
      "Poca.multimedia" = multimedia$num < meanmultimedia,
      "Mucha.multimedia" = multimedia$num >= meanmultimedia
    )
    
    rownames(df) <- unique(chat()$autor)
    
    return(df)
  })
  
  #Creamos el contexto formal para aplicar FCA
  fcConceptos <- reactive({
    req(input$checkboxFCAConcepts)
    
    df <- df()[rownames(df()) %in% input$checkboxFCAConcepts]
    
    fc <- FormalContext$new(df)
    fc$find_concepts()
    fc$find_implications()
    
    return(fc)
  })
  
  fcImplicaciones <- reactive({
    req(input$checkboxFCAImplications)
    #req(input$checkboxFCAImplications1)
    #req(input$checkboxFCAImplications2)
    
    df <- df()[rownames(df()) %in% input$checkboxFCAImplications]
    
    fc <- FormalContext$new(df)
    fc$find_concepts()
    fc$find_implications()
    #fc$implications$filter(lhs = "Pocos.mensajes", "Pocos.emojis", "Poca.multimedia")
    
    return(fc)
  })
  
  #Creamos las transacciones con arules para aplicar Reglas de Asociacion
  chat_arules1 <- reactive({
    req(input$checkboxRA1)
    
    df <- df()[rownames(df()) %in% input$checkboxRA1]
    
    chat_arules <- as(df, "transactions")
    
    return(chat_arules)
  })
  
  chat_arules2 <- reactive({
    req(input$checkboxRA1)
    req(input$checkboxRA2)
    
    df <- df()[rownames(df()) %in% input$checkboxRA1, colnames(df()) %in% input$checkboxRA2]
    
    chat_arules <- as(df, "transactions")
    
    return(chat_arules)
  })
  
  #DESCARGA DE GRAFICOS
  descargar_grafico <- function(id, plot_func) {
    downloadHandler(
      filename = function() {
        paste("grafico", id, ".jpg")
      },
      content = function(file) {
        ggsave(file, plot = plot_func(), width = 12)
      }
    )
  }
  
  #CARGAR CHAT
  output$chat <- renderDataTable({
    chat()
  })
  
  #TEXT MINING
  #Duracion de la conversacion
  tiempo <- reactive({
    chat <- chat() %>%
      mutate(fecha = as.Date(fecha)) %>% 
      count(fecha, name = "mensajes")
    
    ggplot(chat, aes(x=fecha, y=mensajes))+
      geom_line() + ggtitle("Mensajes a lo largo del tiempo") + 
      xlab("Fecha") + ylab("Número de mensajes") + 
      theme_minimal()
  })
  
  output$tiempo <- renderPlot({
    tiempo()
  })
  
  output$descarga1 <- descargar_grafico(1, tiempo)
  
  #Numero de mensajes
  nummensajes <- reactive({
    mensajes <- mensajes()
    
    ggplot(mensajes, aes(x=nummensajes, y=autor, fill = factor(autor))) + 
      geom_col(show.legend = FALSE) + ggtitle("Número de mensajes") + 
      xlab("Número de mensajes") + ylab("Autor") +
      theme_minimal()
  })
  
  output$nummensajes <- renderPlot({
    nummensajes()
  })
  
  output$descarga2 <- descargar_grafico(2, nummensajes)
  
  #Longitud de los mensajes
  longmensaje <- reactive({
    mensajes <- mensajes()
    
    ggplot(mensajes, aes(x=longmensaje, y=autor, fill = factor(autor))) +
      geom_col(show.legend = FALSE) + 
      ggtitle("Longitud media de mensaje por usuario") + 
      xlab("Longitud media de mensaje") + ylab("Autor") +
      theme_minimal()
  })
  
  output$longmensaje <- renderPlot({
    longmensaje()
  })
  
  output$descarga3 <- descargar_grafico(3, longmensaje)
  
  #Numero de plabaras enviadas
  numwords <- reactive({
    numpalabras <- numpalabras()
    
    ggplot(numpalabras, aes(x = n, y = autor, fill = factor(autor))) +
      geom_col(show.legend = FALSE) + 
      ggtitle("Número de palabras enviadas por usuario") +
      xlab("Número de palabras") + ylab("Autor") +
      theme_minimal()
  })
  
  output$numpalabras <- renderPlot({
    numwords()
  })
  
  output$descarga4 <- descargar_grafico(4, numwords)
  
  #Palabras mas usadas
  palabrasusadas <- reactive({
    palabras <- palabras()
    
    chat <- chat() %>% 
      unnest_tokens(input = texto, output = word) %>% 
      filter(!word %in% palabras) %>% 
      count(word) %>% 
      top_n(30, n) %>% 
      arrange(desc(n))
    
    ggplot(chat, aes(x=reorder(word, n), y=n, fill=n, color=n))+
      geom_col(show.legend = FALSE, width = .05) +
      geom_point(show.legend = FALSE, size = 3) +
      scale_fill_gradient(low = "#2b83ba", high = "#d7191c") + 
      scale_color_gradient(low = "#2b83ba", high = "#d7191c") +
      ggtitle("Palabras más usadas") + xlab("Palabras") + ylab("Veces usada") +
      coord_flip() + theme_minimal()
  })
  
  output$palabrasusadas <- renderPlot({
    palabrasusadas()
  })
  
  output$descarga5 <- descargar_grafico(5, palabrasusadas)
  
  #Palabras mas usadas por usuario
  palabrasusadasuser <- reactive({
    palabras <- palabras()
    
    chat <- chat() %>% 
      unnest_tokens(input = texto, output = word) %>% 
      filter(!word %in% palabras) %>% 
      count(autor, word, sort = TRUE) %>%
      group_by(autor) %>% 
      top_n(5, n) %>% 
      slice(1:5) %>% 
      arrange(autor, desc(n)) %>% 
      mutate(order=row_number())
    
    ggplot(chat, aes(x=reorder(word, n), y=n, fill=autor, color=autor))+
      geom_col(show.legend = FALSE, width = .05) +
      geom_point(show.legend = FALSE, size = 3) +
      ggtitle("Palabras más usadas por usuario") + 
      xlab("Palabras") + ylab("Veces usada") +
      facet_wrap(~autor, ncol = 3, scales = "free") +
      coord_flip() + theme_minimal()
  })
  
  output$palabrasusadasuser <- renderPlot({
    palabrasusadasuser()
  })
  
  output$descarga6 <- descargar_grafico(6, palabrasusadasuser)
  
  #Emojis mas usados
  emojisusados <- reactive({
    chat_emoji <- chat_emoji() %>% 
      group_by(emoji) %>% 
      summarize(num = sum(n)) %>% 
      arrange(desc(num)) %>% 
      top_n(20, num) %>% 
      mutate(emoji_url = map_chr(emoji, 
                                 ~paste0("https://abs.twimg.com/emoji/v2/72x72/",
                                         as.hexmode(utf8ToInt(.x)), ".png")))
    
    ggplot(chat_emoji, aes(x=emoji, y=num, fill = factor(emoji))) +
      geom_col(show.legend = FALSE) + geom_image(aes(image = emoji_url), size = .05) +
      ggtitle("Emojis mas usados") + 
      xlab("Veces usado") + ylab("Emoji") + 
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  })
  
  output$emojisusados <- renderPlot({
    emojisusados()
  })
  
  output$descarga7 <- descargar_grafico(7, emojisusados)
  
  #Emojis mas usados por usuario
  emojisusadosuser <- reactive({
    chat_emoji <- chat_emoji() %>% 
      select(autor, n, emoji, emoji_url) %>% 
      group_by(autor) %>% 
      top_n(1, wt = n) %>%
      ungroup()
    
    ggplot(chat_emoji, aes(x=autor, y=n, fill = factor(autor))) +
      geom_col(show.legend = FALSE) + geom_image(aes(image = emoji_url), size = .1) +
      ggtitle("Emoji mas usado por usuario") + 
      xlab("Veces usado") + ylab("Autor") + theme_minimal()
  })
  
  output$emojisusadosuser <- renderPlot({
    emojisusadosuser()
  })
  
  output$descarga8 <- descargar_grafico(8, emojisusadosuser)
  
  #Sentimietos por emojis
  sentimientosemoji <- reactive({
    emoji_sentimientos <- emoji_sentimientos()
    
    final_emoji <- emoji_sentimientos %>% 
      mutate(negativo = -negativo,
             neutral.pos = neutral/2,
             neutral.neg = -neutral/2) %>%
      select(-neutral) %>% 
      gather("sentiment","mean", -autor, -balance) %>% 
      mutate(sentiment = factor(sentiment, levels = c("negativo", "neutral.neg", "positivo", "neutral.pos"), ordered = TRUE))
    
    ggplot(final_emoji, aes(x=autor, y=mean, fill=sentiment)) +
      geom_bar(position="stack", stat="identity", show.legend = FALSE, width = .5) +
      scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
      ylab("Negativo | Neutral | Positivo") + xlab("Usuario") +
      ggtitle("Análisis de sentimientos por usuario basado en emojis") +
      coord_flip() + theme_minimal()
  })
  
  output$sentimientosemoji <- renderPlot({
    sentimientosemoji()
  })
  
  output$descarga9 <- descargar_grafico(9, sentimientosemoji)
  
  #Sentimientos por palabras
  sentimientoslexico <- reactive({
    chat <- sentlexico()
    
    ggplot(chat, aes(x=autor, y=mean, fill=value)) +
      geom_bar(stat="identity",position="stack", show.legend = F, width = .5) +
      xlab("Usuario") + ylab("Escala de negativo a positivo") +
      coord_flip() +
      ggtitle("Análisis de sentimientos por usuario basado en el léxico AFINN") +
      theme_minimal()
  })
  
  output$sentimientoslexico <- renderPlot({
    sentimientoslexico()
  })
  
  output$descarga10 <- descargar_grafico(10, sentimientoslexico)
  
  #Emociones
  emociones <- reactive({
    chat <- emocioneslexico()
    
    ggplot(chat, aes(x=reorder(sentiment, n), y=n, fill=n, color=n)) +
      geom_col(show.legend = FALSE, width = .05) +
      geom_point(show.legend = FALSE, size = 3) +
      scale_fill_gradient(low = "#2b83ba", high = "#d7191c") + 
      scale_color_gradient(low = "#2b83ba", high = "#d7191c") +
      ggtitle("Emociones más expresadas") + xlab("Veces expresada") + ylab("Emociones") + 
      coord_flip() + theme_minimal()
  })
  
  output$emociones <- renderPlot({
    emociones()
  })
  
  output$descarga11 <- descargar_grafico(11, emociones)
  
  #Emociones por usuario
  emocionesuser <- reactive({
    chat <- emocioneslexicousuario()
    
    ggplot(chat, aes(x=reorder(sentiment, n), y=n, fill=autor, color=autor)) +
      geom_col(show.legend = FALSE, width = .05) +
      geom_point(show.legend = FALSE, size = 3) +
      ggtitle("Emociones más expresadas por usuario") + xlab("Veces expresada") + ylab("Emociones") + 
      facet_wrap(~autor, ncol = 3, scales = "free") +
      coord_flip() + theme_minimal()
  })
  
  output$emocionesuser <- renderPlot({
    emocionesuser()
  })
  
  output$descarga12 <- descargar_grafico(12, emocionesuser)
  
  #ARULES
  output$resumen <- renderPrint({
    reglas <- apriori(chat_arules1(),parameter=list(support=.5, confidence=.9))
    summary(reglas)
  })
  
  output$reglas <- renderPrint({
    reglas <- apriori(chat_arules2(),parameter=list(support=.5, confidence=.9))
    inspect(reglas)
  })
  
  #FCA
  #Conceptos
  output$fc_conceptos <- renderPlot({
    fcConceptos()$concepts$plot()
  })
  
  #Implicaciones
  output$fc_implicaciones <- renderPrint({
    fcImplicaciones()$implications$print()
  })
  
})
