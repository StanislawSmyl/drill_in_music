theme <- theme(
  plot.title = element_text(size = 22, hjust = 0.5),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  panel.background = element_rect(fill = 'transparent', color = NA),
  panel.grid.major = element_line(colour = "grey20"),
  panel.grid.minor = element_line(colour = "grey20"),
  plot.margin = unit(c(1,1,1,1), "cm")
)

# Define server function required to create the scatterplot
shinyServer(function(input, output) {
  # Create the scatterplot object the plotOutput function is expecting
  output$boxplot <- renderPlotly({
    req(input$sentiment)
    req(input$years)
    data <- reactive({songsSentiment %>%
        select(-totalSentiment, -percentSentiment) %>%
        filter(sentiment == input$sentiment & releaseDate %in% input$years & genreTop %in% input$genre) %>%
        dplyr::count(songName, releaseDate, totalWords) %>%
        ungroup()  %>%
        mutate(percent = n / totalWords)
    })
    # Use ggplot to set up a plot with year and percent
    g <- ggplot(data(), aes(as.factor(releaseDate), percent)) +
      geom_boxplot() + labs(title=paste('Emotion:', input$sentiment),
                            x ='Year', y = '% all emotions') + theme
    ggplotly(g)
  })
  output$lineplot <- renderPlotly({
    req(input$emotions)
    data <- reactive({
      dt <- data.table(songsSentiment)
      dt <- dt[releaseDate > input$years2[1]
               & releaseDate < input$years2[2] 
               & sentiment %in% input$emotions]
      dt[, count := .N, by = .(releaseDate)][, count2 := .N, by = .(releaseDate, sent)]
      dt[sent == 'pos', .(posRatio = min(count2) / min(count) * 100, count = min(count)), 
         by = .(releaseDate, sent)]
    })
    # Use ggplot to set up a plot with year and percent
    g <- ggplot(data(), aes(releaseDate, posRatio)) +
      geom_hline(yintercept = 50, color = 'red') +
      annotate("text", min(data()$releaseDate), 54, vjust = -1, label = "50%") +
      ylim(0, 100) +
      geom_line() + geom_point(aes(size = count)) + labs(title = 'Positive emotions ratio',
                                                         x ='Year', y = '% positive emotions') + theme
    
    ggplotly(g)
  })
  #########################
  # Wordcloud
  output$cloud <- renderWordcloud2({
    req(input$wc_songName)
    req(input$num)
    req(input$col)
    # Use the values from the two inputs as
    # parameters to the word cloud
    # ODKOMENTUJ
    songsClean %>%
      # Filter song name
      filter(songName %in% input$wc_songName) %>%
      # Select unique doc_id
      select(doc_id) %>%
      pull(doc_id) %>%
      # Match doc_id in songsDTM dataframe
      match(rownames(songsDTM)) %>%
      songsDTM[.,] %>%
      # Sum words
      colSums() %>%
      # Sort in decreasing order
      sort(decreasing = T) %>%
      # Select only top n
      head(input$num) %>%
      data.frame() %>%
      rownames_to_column() %>%
      # Create dataframe that will suit wordcloud2
      `colnames<-`(c('word', 'freq')) %>%
      wordcloud2(background = input$col)
  })
  
# Comparison wordcloud
  output$wordcloud_comp <- renderPlot({
    req(input$cc_songName1)
    req(input$cc_songName2)
    req(input$cc_num)
    # Use the values from the two inputs as
    # parameters to the word cloud
    # ODKOMENTUJ
    songsClean %>%
      # Filter song name
      filter(songName %in% c(input$cc_songName1, input$cc_songName2))  %>%
      # filter(songName %in% c(input$cc_songName1, input$cc_songName2) & 
      #          artist %in% c("HAEVN", "Richie Kotzen"))  %>%
      # Ordered, so i know leter which is which, based on doc_id
      arrange(songName) %>%
      # Select unique doc_id
      select(doc_id) %>%
      pull(doc_id) %>%
      # Match doc_id in songsDTM dataframe
      match(rownames(songsDTM)) %>%
      songsDTM[.,] %>%
      data.table::transpose() %>%
      # Ordered
      `colnames<-`(sort(c(input$cc_songName1, input$cc_songName2))) %>%
      `rownames<-`(colnames(songsDTM)) %>%
      wordcloud::comparison.cloud(colors = c("orange", "blue"), max.words = input$cc_num)
  })
  
output$pyramid_comp <- renderPlot({
    # Use the values from the two inputs as
    # parameters to the word cloud
    # ODKOMENTUJ
    songsClean %>%
      # Filter song name
    filter(songName %in% c(input$pp_songName1, input$pp_songName2))  %>%
      # filter(songName %in% c(input$cc_songName1, input$cc_songName2) & 
      #          artist %in% c("HAEVN", "Richie Kotzen"))  %>%
    # Ordered, so i know leter which is which, based on doc_id
    arrange(songName) %>%
    # Select unique doc_id
    select(doc_id) %>%
    pull(doc_id) %>%
    # Match doc_id in songsDTM dataframe
    match(rownames(songsDTM)) %>%
    songsDTM[.,] %>%
    data.table::transpose() %>%
    # Temporary names
    `colnames<-`(c('x1','x2')) %>%
    mutate(diff = abs(.$x1 - .$x2), labels = colnames(songsDTM)) %>%
    arrange(diff) %>%
    filter_at(c(1,2), all_vars(. > 0)) %>%
    tail(input$pp_num)  -> pp_df
  
    pyramid.plot(pp_df$x1, pp_df$x2,
               labels = pp_df$labels, gap = 2,
               top.labels = c(sort(c(input$pp_songName1, input$pp_songName2))[1], 
                              "Words", 
                              sort(c(input$pp_songName1, input$pp_songName2))[2]),
               main = "Words in Common", laxlab = NULL, 
               raxlab = NULL, unit = NULL)
  })
  
output$ldaSongs <- renderPlot({
  # Use ggplot to set up a plot with year and percent
  g <- makePlot(ldaSongs, 15)
  g
})

output$ldaComments <- renderPlot({
  # Use ggplot to set up a plot with year and percent
  g <- makePlot(ldaComments, 15)
  g
})

output$songsComments <- renderPlotly({
  req(input$songsCommentsGenre)
  songsCommentsSentiment <- songsCommentsSentiment %>% 
    filter(genreTop == input$songsCommentsGenre) %>%
    select(genreTop, sentiment, song_percent, comment_percent) %>%
    unique() %>%
    # Create long data fromat
    gather(type, value, -c(genreTop, sentiment)) 
    # Create bar plot
    g <- ggplot(data = songsCommentsSentiment, aes(sentiment, value)) +   
    geom_bar(aes(fill = type), position = "dodge", stat="identity")
  ggplotly(g)
})

inputData <- reactive({
  df <- songsClean
  req(input$songLyrics)
  m <- as.matrix(DTMsongs)
  inputText <- input$songLyrics
  inputText <- doCleaning(inputText)
  inputText <- convert.tm.to.character(inputText)
  s <- intersect(unlist(strsplit(inputText, ' ')), colnames(m))
  a <- sapply(seq(dim(m)[1]), function(i) length(intersect(names(m[i, which(m[i, ] == 1)]), s)))
  df$match <- a
  songs.sorted <- df[order(df$match, decreasing = T), ]
  songs.sorted[df$match > 4, c('artist', 'songName', 'albumName', 'releaseDate',
                                         'duration', 'popularity', 'tempo', 'genreTop')]
})

output$ui <- renderUI({
  if(nrow(inputData()) == 0)
    return("Paste more lyrics to find similar songs :)")
  tableOutput("songLyrics")
})

output$songLyrics <- renderDataTable({
  head(inputData(), 10)
})

}
)
# Create a Shiny app object
#shinyApp(ui = ui, server = server)

