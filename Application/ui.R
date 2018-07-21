# Define UI for application that plots features of movies
dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Play with music"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Content", tabName = "home", icon = icon("dashboard")),
      menuItem("Sentiment", icon = icon("meh-o"), tabName = "sentiment",
               badgeLabel = "hot", badgeColor = "red"),
      menuItem("Wordclouds", icon = icon("cloud"), tabName = "wordcloud"),
      menuItem("Word comparison", icon = icon("cloud"), tabName = "comparison"),
      menuItem("Topics comparison", icon = icon("clone"), tabName = 'topics'),
      menuItem("Songs vs comments", icon = icon("bolt"), tabName = 'songsComments'),
      menuItem("Find similar songs", icon = icon('eye'), tabName = 'findSimilar')
    )
  ),
  # Sidebar layout with a input and output definitions
  
  # Inputs
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem(tabName = 'home',
              div(style = 'text-align: center',
                  h1('Stanislaw Smyl, Artur Gorlicki'),
                  h2('Please prepare to special music experience'))
      ),
      tabItem(tabName = 'sentiment',
              fluidRow(
                box(width = 12,
                    div(class = 'simple', 
                        'Boxplot showing the procentage rate of words in specified sentiment in comparison to all the words in a song based on different genre and across time.')
                )
              ),
              fluidRow(
                box(width = 12,
                    # # Select sentiment
                    box(width = 3, title = "Settings", status = "success",
                        solidHeader = F,
                        selectizeInput(inputId = "genre",
                                       label = "Genre:",
                                       sort(unique(songsSentiment$genreTop)), 
                                              selected = sort(unique(songsSentiment$genreTop))[1]
                        ),
                        selectizeInput(inputId = "sentiment",
                                       label = "Sentiment:",
                                       unique(songsSentiment$sentiment, 
                                              selected = unique(songsSentiment$sentiment)[1])
                        ),
                        pickerInput(inputId = "years", 
                                    label = "Select years", 
                                    choices = list('years' = sort(unique(songsSentiment$releaseDate), decreasing = T)), 
                                    options = list('actions-box' = TRUE), 
                                    multiple = T,
                                    selected =  sort(unique(songsSentiment$releaseDate))[length(unique(songsSentiment$releaseDate))])
                    ),
                    box(width = 9, title = "Boxplot of sentiments", status = "success", solidHeader = F,
                        div(class = 'simplePlot',
                            plotlyOutput(outputId = "boxplot"))
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    div(class = 'simpleMain', 
                        'Comparison of ratio between words with positive and negative sentiment across time.')
                )
              ),
              fluidRow(
                box(width = 12,
                    # # Select sentiment
                    box(width = 3, title = "Settings", status = "success",
                        solidHeader = F,
                        div(class = 'simpleSlider',
                            noUiSliderInput("years2",
                                            label = 'Years:',
                                            value = c(min(songsSentiment$releaseDate), 
                                                      max(songsSentiment$releaseDate)), 
                                            min = min(songsSentiment$releaseDate), 
                                            max = max(songsSentiment$releaseDate), 
                                            format = wNumbFormat(decimals = FALSE),
                                            step = 1)),
                        hr(),
                        pickerInput(inputId = "emotions", 
                                    label = "Select emotions", 
                                    choices = list('positive' = pos, 
                                                   'negative' = neg), options = list('actions-box' = TRUE), 
                                    multiple = T, selected = c(pos, neg))
                    ),
                    box(width = 9, title = "Positive emotions ratio", status = "success", solidHeader = F,
                        div(class = 'simplePlot',
                            plotlyOutput(outputId = 'lineplot')
                        )
                    )
                )
              )
      ),
####################################
tabItem(tabName = 'wordcloud',
        fluidRow(
          box(width = 12,
              div(class = 'simple', 
                  'Visualisation of specified number of words in a given list of songs.')
          )
        ),
        #####################
        fluidRow(
          box(width = 12,
              # # Select sentiment
              box(width = 3, title = "Settings", status = "success",
                  solidHeader = F,
                  selectInput(inputId = "wc_songName", 
                              label = "Choose song name",
                              choices = sort(unique(songsClean$songName)),
                              selected = sort(unique(songsClean$songName))[1],
                              multiple = T),
                  # Add a numeric input for the number of words
                  numericInput(inputId = 'num', label = "Maximum number of words",
                               value = 10, min = 5, max = 100),
                  # Add a colour input for the background colour
                  colourInput("col", "Background colour", "white")
              ),
              box(width = 9, title = "Wordcloud", status = "success", solidHeader = F,
                  div(class = 'simplePlot', 
                      style = 'text-align: center',
                      wordcloud2Output("cloud")
                  )
              )
              
          )
        )
        ######################
),
####################################
####################################
tabItem(tabName = 'comparison',
        fluidRow(
          box(width = 12,
              div(class = 'simple', 
                  'Wordcloud showing a given number of the most frequent words for two specified songs.')
          )
        ),
        #####################
        fluidRow(
          box(width = 12,
              # # Select sentiment
              box(width = 3, title = "Settings", status = "success",
                  solidHeader = F,
                  selectInput(inputId = "cc_songName1", 
                              label = "Choose song nr 1",
                              choices = sort(unique(songsClean$songName)),
                              selected = sort(unique(songsClean$songName))[1],
                              multiple = F),
                  selectInput(inputId = "cc_songName2", 
                              label = "Choose song nr 2",
                              choices = sort(unique(songsClean$songName)),
                              selected = sort(unique(songsClean$songName))[2],
                              multiple = F),
                  # Add a numeric input for the number of words
                  numericInput(inputId = 'cc_num', label = "Maximum number of words",
                               value = 10, min = 5, max = 100)
              ),
              box(width = 9, title = "Comparison cloud", status = "success", solidHeader = F,
                  div(class = 'simplePlot',
                      plotOutput(outputId = 'wordcloud_comp')
                  )
              )
              
          )
        ),
        fluidRow(
          box(width = 12,
              div(class = 'simple', 
                  'Frequencies of common words between two specified songs.')
          )
        ),
        ######################
        fluidRow(
          box(width = 12,
              # # Select sentiment
              box(width = 3, title = "Settings", status = "success",
                  solidHeader = F,
                  selectInput(inputId = "pp_songName1", 
                              label = "Choose song nr 1",
                              choices = sort(unique(songsClean$songName)),
                              selected = sort(unique(songsClean$songName))[1],
                              multiple = F),
                  selectInput(inputId = "pp_songName2", 
                              label = "Choose song nr 2",
                              choices = sort(unique(songsClean$songName)),
                              selected = sort(unique(songsClean$songName))[2],
                              multiple = F),
                  # Add a numeric input for the number of words
                  numericInput(inputId = 'pp_num', label = "Maximum number of words",
                               value = 10, min = 5, max = 100)
              ),
              box(width = 9, title = "Pyramid plot", status = "success", solidHeader = F,
                  div(class = 'simplePlot',
                      plotOutput(outputId = 'pyramid_comp')
                  )
              )
              
          )
        )
        #############################
),
####################################
####################################
tabItem(tabName = 'topics',
        fluidRow(
          box(width = 12,
              div(class = 'simple',
                  'All songs divided into two topics based on lyrics similarity. Below you can find the words that fitted its group most.')
          )
        ),
        #####################
        fluidRow(
              box(width = 12, title = "Topics in songs with most fitting words", status = "success", solidHeader = F,
                  div(class = 'simpleLDA',
                      plotOutput(outputId = 'ldaSongs')
                  )
              )
        ),
        fluidRow(
          box(width = 12,
              div(class = 'simple', 
                  'All comments divided into two topics based on content similarity. Below you can find the words that fitted its group most.')
          )
        ),
        ######################
        fluidRow(
          box(width = 12, title = "Topics in comments with most fitting words", status = "success", solidHeader = F,
              div(class = 'simpleLDA',
                  plotOutput(outputId = 'ldaComments')
              )
          )
        )
        #############################
),
##################################
#################################
tabItem(tabName = 'songsComments',
        fluidRow(
          box(width = 12,
              div(class = 'simple', 
                  'Comparison of sentiment in songs and comments related to that songs. Bar plot shows the procentage of words in a given sentiment in comparison to total number of words in specified genre')
              )
        ),
        fluidRow(
          box(width = 12,
              # # Select sentiment
              box(width = 3, title = "Settings", status = "success",
                  solidHeader = F,
                  selectizeInput(inputId = "songsCommentsGenre",
                                 label = "Genre:",
                                 sort(unique(songsSentiment$genreTop)), 
                                 selected = sort(unique(songsSentiment$genreTop))[1]
                  )
              ),
              box(width = 9, title = "Songs vs Comments", status = "success", solidHeader = F,
                  div(class = 'simplePlot',
                      plotlyOutput(outputId = "songsComments"))
              )
        )
      )
####################################
    ),
tabItem(tabName = 'findSimilar',
        fluidRow(
          box(width = 12,
              div(class = 'simple', 
                  'It is easy to find the song with lyrics similar to what you paste!')
          )
        ),
        fluidRow(
          box(width = 12,
              # # paste song lyrics
              box(width = 3, title = "Song lyrics", status = "success",
                  solidHeader = F,
                  textAreaInput(inputId = "songLyrics",
                                 label = HTML("<font size = '4em'>Paste song lyrics: </font><br/> (see example lyrics below)"),
                                value = inputTextExample,
                                height = '300px'
                  )
              ),
              box(width = 9, title = "Similar songs:", status = "success", solidHeader = F,
                  div(class = 'simplePlot',
                      dataTableOutput(outputId = "songLyrics"))
              )
          )
        )
        ####################################
)
  )
)
)