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

shinyServer(function(input, output) {
  
  #Cargamos y modificamos el dataframe del chat
  chat <- reactive({
    req(input$archivo)
    chat <- rwa_read(input$archivo$datapath)
    
    chat <- chat %>% 
      select(time, author, text, emoji)
    
    colnames(chat) <- c("fecha", "autor", "texto", "emoji")
    
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
    chat_emoji <- chat() %>% 
      unnest(emoji) %>% 
      mutate(emoji = str_sub(emoji, end = 1)) %>% 
      count(autor, emoji, sort = TRUE) %>%
      arrange(desc(n)) %>% 
      mutate(emoji_url = map_chr(emoji, 
                                 ~paste0("https://abs.twimg.com/emoji/v2/72x72/",
                                         as.hexmode(utf8ToInt(.x)), ".png")))
    
    return(chat_emoji)
  })
  
  #Creamos un dataframe para los mensajes
  mensajes <- reactive({
    chat_in <- chat()
    
    mensajes <- chat_in %>% 
      group_by(autor) %>% 
      summarise(nummensajes = n(),
                longmensaje = mean(ncaracteres))
    
    autor <- as.character(na.omit(unique(chat_in$autor)))
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
  
  emojis <- reactive({
    chat_in <- chat()
    chat_emoji_in <- chat_emoji()
    
    emojis <- chat_emoji_in %>% 
      group_by(autor) %>% 
      summarise(num = sum(n))
    
    autor <- as.character(na.omit(unique(chat_in$autor)))
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
  
  multimedia <- reactive({
    chat_in <- chat()
    
    multimedia <- chat_in %>% 
      group_by(autor) %>% 
      summarise(num = sum(multimedia))
    
    autor <- as.character(na.omit(unique(chat_in$autor)))
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
  
  #Dataframe para los sentimientos con emojis
  emoji_sentimientos <- reactive({
    chat_in <- chat()
    emojis <- chat_emoji()
    
    url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
    doc <- read_html(url_base)
    
    tabla_emojis <- doc %>% 
      html_node("#myTable") %>% 
      html_table() %>% 
      as_tibble()
    
    sentimiento_emoji <- tabla_emojis %>% 
      select(1,6:9) %>% 
      set_names("char", "negativo","neutral","positivo","sent.score")
    
    emoji_chat <- emojis %>% 
      inner_join(sentimiento_emoji, by=c("emoji"="char")) 
    
    emoji_sentimientos_usuarios <- emoji_chat %>% 
      group_by(autor) %>% 
      summarise(positivo=mean(positivo),
                negativo=mean(negativo),
                neutral=mean(neutral),
                balance=mean(sent.score)) %>% 
      arrange(desc(balance))
    
    return(emoji_sentimientos_usuarios)
    
  })
  
  df <- reactive({
    mensajes <- mensajes()
    emojis <- emojis()
    multimedia <- multimedia()
    sentimientos <- sentimientos()
    
    meanmensajes <- mean(mensajes$nummensajes)
    meanlongitud <- mean(mensajes$longmensaje)
    meanemojis <- mean(emojis$num)
    meanmultimedia <- mean(multimedia$num)
    meanpalabras <- mean(sentimientos$WordCount)
    
    df <- data.frame(
      "Pocos mensajes" = mensajes$nummensajes < meanmensajes,
      "Muchos mensajes" = mensajes$nummensajes >= meanmensajes,
      "pocas palabras" = sentimientos$WordCount < meanpalabras,
      "Muchas palabras" = sentimientos$WordCount >= meanpalabras,
      "Poca longitud" = mensajes$longmensaje < 70,
      "Mucha longitud" = mensajes$longmensaje >= 70,
      "Pocos emojis" = emojis$num < meanemojis,
      "Muchos emojis" = emojis$num >= meanemojis,
      "Poca multimedia" = multimedia$num < meanmultimedia,
      "Mucha multimedia" = multimedia$num >= meanmultimedia
    )
    
    rownames(df) <- unique(chat()$autor)
    
    return(df)
  })
  
  fc <- reactive({
    fc <- FormalContext$new(df())
    fc$find_concepts()
    fc$find_implications()
    
    return(fc)
  })
  
  chat_arules <- reactive({
    chat_arules <- as(df(), "transactions")
    
    return(chat_arules)
  })
  
  #CARGAR CHAT
  output$chat <- renderDataTable({
    chat()
  })
  
  #TEXT MINING
  output$tiempo <- renderPlot({
    chat_tiempo <- chat() %>%
      mutate(fecha = as.Date(fecha)) %>% 
      count(fecha, name = "mensajes")
    
    ggplot(chat_tiempo, aes(x=fecha, y=mensajes))+
      geom_line() + ggtitle("Mensajes a lo largo del tiempo") + theme_minimal()
  })
  
  output$nummensajes <- renderPlot({
    mensajes <- mensajes()
    
    ggplot(mensajes, aes(x=autor, y=nummensajes, fill = factor(autor))) + 
      geom_col(show.legend = FALSE) + ggtitle("Numero de mensajes") + 
      theme_minimal()
  })
  
  output$longmensaje <- renderPlot({
    mensajes <- mensajes()
    
    ggplot(mensajes, aes(x=autor, y=longmensaje, fill = factor(autor))) +
      geom_col(show.legend = FALSE) +  ggtitle("Longitud de mensaje por usuario") + 
      theme_minimal()
  })
  
  output$numpalabras <- renderPlot({
    sentimientos <- sentimientos()
    
    ggplot(sentimientos, aes(x=autor, y=WordCount, fill = factor(autor))) +
      geom_col(show.legend = FALSE) +  ggtitle("Numero de palabras por usuario") + 
      theme_minimal()
  })
  
  output$palabrasusadas <- renderPlot({
    palabras <- c(stopwords("es"), "adjunto" , "2022", "audio", "opus",
                  "03", "04", "05", "11", "12", "14", "15", "13", "16", "17",
                  "imagen", "omitida", "omitido", "webp", "01", "02", "06", "07", "08", 
                  "09", "10", "18", "19", "20", "21", "22", "23", "24", "25", 
                  "26", "27", "28", "29", "00", "sticker", "photo", "jpg", "2021",
                  "30", "31", "33", "vm.tiktok.com", "puto", "puta")
    
    chat_extra <- chat() %>% 
      unnest_tokens(input = texto, output = word) %>% 
      filter(!word %in% palabras) %>% 
      count(word) %>% 
      top_n(30, n) %>% 
      arrange(desc(n))
    
    ggplot(chat_extra, aes(x=reorder(word, n), y=n, fill=n, color=n))+
      geom_col(show.legend = FALSE, width = .1) +
      geom_point(show.legend = FALSE, size = 3) +
      scale_fill_gradient(low = "#2b83ba", high = "#d7191c") + 
      scale_color_gradient(low = "#2b83ba", high = "#d7191c") +
      ggtitle("Palabras más usadas") + xlab("Palabras") + ylab("Veces usada") +
      coord_flip() + theme_minimal()
  })
  
  output$palabrasusadasuser <- renderPlot({
    palabras <- c(stopwords("es"), "adjunto" , "2022", "audio", "opus",
                  "03", "04", "05", "11", "12", "14", "15", "13", "16", "17",
                  "imagen", "omitida", "omitido", "webp", "01", "02", "06", "07", "08", 
                  "09", "10", "18", "19", "20", "21", "22", "23", "24", "25", 
                  "26", "27", "28", "29", "00", "sticker", "photo", "jpg", "2021",
                  "30", "31", "33", "vm.tiktok.com", "puto", "puta")
    
    chat_extra <- chat() %>% 
      unnest_tokens(input = texto, output = word) %>% 
      filter(!word %in% palabras) %>% 
      count(autor, word, sort = TRUE) %>%
      group_by(autor) %>% 
      top_n(5, n) %>% 
      slice(1:5) %>% 
      arrange(autor, desc(n)) %>% 
      mutate(order=row_number())
    
    ggplot(chat_extra, aes(x=reorder(word, n), y=n, fill=autor, color=autor))+
      geom_col(show.legend = FALSE, width = .1) +
      geom_point(show.legend = FALSE, size = 3) +
      ggtitle("Palabras más usadas por usuario") + 
      xlab("Palabras") + ylab("Veces usada") +
      facet_wrap(~autor, ncol = 3, scales = "free") +
      coord_flip() + theme_minimal()
  })
  
  output$emojisusados <- renderPlot({
    chat_emoji <- chat_emoji() %>% 
      group_by(emoji) %>% 
      summarize(num = sum(n)) %>% 
      arrange(desc(num)) %>% 
      top_n(15, num) %>% 
      mutate(emoji_url = map_chr(emoji, 
                                 ~paste0("https://abs.twimg.com/emoji/v2/72x72/",
                                         as.hexmode(utf8ToInt(.x)), ".png")))
    
    ggplot(chat_emoji, aes(x=emoji, y=num, fill = factor(emoji))) +
      geom_col(show.legend = FALSE) + geom_image(aes(image = emoji_url), size = .05) +
      ggtitle("Emojis mas usados") + theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
  })
  
  output$emojisusadosuser <- renderPlot({
    chat_emoji <- chat_emoji() %>% 
      select(autor, n, emoji, emoji_url) %>% 
      group_by(autor) %>% 
      top_n(1, wt = n) %>%
      ungroup()
    
    ggplot(chat_emoji, aes(x=autor, y=n, fill = factor(autor))) +
      geom_col(show.legend = FALSE) + geom_image(aes(image = emoji_url), size = .05) +
      ggtitle("Emoji mas usado por usuario") + theme_minimal()
  })
  
  output$sentimientosemoji <- renderPlot({
    emoji_sentimientos <- emoji_sentimientos()
    
    final_emoji <- emoji_sentimientos %>% 
      mutate(negativo = -negativo,
             neutral.pos = neutral/2,
             neutral.neg = -neutral/2) %>% 
      select(-neutral) %>% 
      gather("sentiment","mean", -autor, -balance) %>% 
      mutate(sentiment = factor(sentiment, levels = c("negativo", "neutral.neg", "positivo", "neutral.pos"), ordered = TRUE))
    
    ggplot(final_emoji, aes(x=reorder(autor,balance), y=mean, fill=sentiment)) +
      geom_bar(position="stack", stat="identity", show.legend = FALSE, width = .5) +
      scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
      ylab("Negativo | Neutral | Positivo") + xlab("Usuario") +
      ggtitle("Análisis de sentimientos por usuario basado en emojis") +
      coord_flip() + theme_minimal()
  })
  
  #ARULES
  output$arules <- renderPrint({
    reglas <- apriori(chat_arules(),parameter=list(support=.5, confidence=.9))
    summary(reglas)
  })
  
  #FCA
  output$fc_conceptos <- renderPlot({
    fc()$concepts$plot()
  })
  
  output$fc_implicaciones <- renderPrint({
    fc()$implications$print()
  })
  
})
