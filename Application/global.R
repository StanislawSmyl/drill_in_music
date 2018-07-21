# PACKAGES
##########################
# Shiny
library(colourpicker) # Colors
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Text mining
library(qdap)
library(tm)

# Data manipulation
library(data.table) # transpose()
library(tidyverse)
library(tidytext)

# Data visualization
library(plotly) # Interactive plots
library(plotrix) # Pyramid plot
library(wordcloud) # Word cloud
library(wordcloud2)
library(textstem)
library(textreg)

# Load data file
songsClean <- read.csv("Dane/songsCleanedSent.csv", stringsAsFactors = F)
load('Dane/LDAsongs.Rdata')
load('Dane/LDAcomments.Rdata')
load('Dane/DTMsongs.Rdata')
songsDTM <- as.data.frame(as.matrix(DTMsongs))
songsSentiment <- read.csv("Dane/songsSentiment.csv", stringsAsFactors = F)
songsCommentsSentiment <- read.csv("Dane/songsCommentsSentiment_sample.csv", stringsAsFactors = F)
songsSentiment <- songsSentiment[songsSentiment$releaseDate > 1937, ]
songsSentiment$releaseDate <- as.integer(songsSentiment$releaseDate)
pos <- c('positive', 'surprise', 'trust', 'joy')
neg <- c('anticipation', 'anger', 'disgust', 'fear', 'negative', 'sadness')
songsSentiment$sent <- ifelse(songsSentiment$sentiment %in% pos, 'pos', 'neg')


makePlot <- function(df, bins){
  # code from https://www.tidytextmining.com/topicmodeling.html
  terms <- df %>%
    group_by(topic) %>%
    top_n(bins, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  terms %>%
    mutate(term = reorder(term, beta))
  gg <- ggplot(data = terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") + coord_flip()
  return(gg)
}

# REMOVE NOT ASCII WORDS
removeNonASCII <- function(x) {
  return (gsub("[^\x01-\x7F]", " ", x))
}

# CLEAN TEXT WITH qdap FUNCTIONS
cleanText <- function(x) {
  x <- removeNonASCII(x)
  x <- replace_abbreviation(x) # Sr => Senior
  x <- replace_contraction(x) # shouldn't => should not
  x <- replace_ordinal(x) # 1st, 22nd => first, twentysecond
  x <- replace_symbol(x) # $ => dollar
  x <- tolower(x) # WORD => word
  #     x <- replace_number(x) # 2 => two
  return(x)
}

# CLEAN CORPUS with tm FUNCTIONS
cleanText2 <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus,  stripWhitespace)
  return(corpus)
}

# wrapper for cleanings
doCleaning <- function(x){
  x <- cleanText(x)
  x <- lemmatize_strings(x)
  xCorp <- cleanText2(VCorpus(VectorSource(x)))
  return(xCorp)
}


inputTextExample = "Cause sometimes you just feel tired, you feel weak
And when you feel weak you feel like you want to just give up
But you gotta search within you, you gotta find that inner strength
And just pull that shit out of you and get that motivation to not give up
And not be a quitter, no matter how bad you want to just fall flat on your face and collapse
'Til I collapse I'm spilling these raps long as you feel 'em
'Til the day that I drop you'll never say that I'm not killing 'em
'Cause when I am not then I'm a stop pinning them
And I am not hip-hop and I'm just not Eminem
Subliminal thoughts when I'm stop sending them
Women are caught in webs spin and hock venom
Adrenaline shots of penicillin could not get the illin' to stop
Amoxicillin is just not real enough
The criminal cop killing hip-hop filling a
Minimal swap to cop millions of Pac listeners
You're coming with me, feel it or not
You're gonna fear it like I showed you the spirit of god lives in us
You hear it a lot, lyrics that shock, is it a miracle
Or am I just a product of pop fizzing up
For shizzle my whizzle this is the plot listen up
You Bizzles forgot Slizzle does not give a fuck
'Til the roof comes off, till the lights go out
'Til my legs give out, can't shut my mouth.
'Til the smoke clears out and my high perhaps
I'm a rip this shit till my bone collapse.
'Til the roof comes off, till the lights go out
'Til my legs give out, can't shut my mouth.
'Til the smoke clears out and my high perhaps
I'm a rip this shit till my bone collapse.
Music is like magic there's a certain feeling you get
When you're real and you spit and people are feeling your shit
This is your moment and every single minute you spittin'
Trying to hold onto it 'cause you may never get it again
So while you're in it try to get as much shit as you can
And when your run is over just admit when it's at its end
'Cause I'm at the end of my wits with half the shit that gets in
I got a list, here's the order of my list that it's in;
It goes, Reggie, Jay-Z, Tupac and Biggie
Andre from Outkast, Jada, Kurupt, Nas and then me
But in this industry I'm the cause of a lot of envy
So when I'm not put on this list the shit does not offend me
That's why you see me walk around like nothing's bothering me
Even though half you people got a fuckin' problem with me
You hate it but you know respect you've got to give me
The press's wet dream like Bobby and Whitney, Nate hit me
'Til the roof comes off, till the lights go out
'Til my legs give out, can't shut my mouth.
'Til the smoke clears out and my high perhaps
I'm a rip this shit till my bone collapse.
'Til the roof comes off, till the lights go out
'Til my legs give out, can't shut my mouth.
'Til the smoke clears out and my high perhaps
I'm a rip this shit till my bone collapse.
Soon as a verse starts I eat at an MC's heart
What is he thinking? Enough to not go against me, smart
And its absurd how people hang on every word
I'll probably never get the props I feel I ever deserve
But I'll never be served my spot is forever reserved
If I ever leave earth that would be the death of me first
'Cause in my heart of hearts I know nothing could ever be worse
That's why I'm clever when I put together every verse
My thoughts are sporadic, I act like I'm an addict
I rap like I'm addicted to smack like I'm Kim Mathers
But I don't want to go forth and back in constant battles
The fact is I would rather sit back and bomb some rappers'
So this is like a full blown attack I'm launching at 'em
The track is on some battling raps who want some static
'Cause I don't really think that the fact that I'm Slim matters
A plaque of platinum status is whack if I'm not the baddest
'Til the roof comes off, till the lights go out
'Til my legs give out, can't shut my mouth.
'Til the smoke clears out and my high perhaps
I'm a rip this shit till my bone collapse.
'Til the roof comes off, till the lights go out
'Til my legs give out, can't shut my mouth.
'Til the smoke clears out and my high perhaps
I'm a rip this shit till my bone collapse.
Until the roof (Until the roof)
The roof comes off (The roof comes off)
Until my legs (Until my legs)
Give out from underneath me (Underneath me, I)
I will not fall
I will stand tall
Feels like no one can beat me"