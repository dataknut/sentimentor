#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(reshape2)
library(scales)

# Parameters ----
myParams <- list()
myParams$appUrl <- "https://dataknut.shinyapps.io/Sentimentor/"
myParams$plotCap <-paste0("Your text, sentimented",
                          "\nPlot by: @dataknut")
myParams$footer <- "Built by @dataknut based on an idea by @draccoops"

# load sentiment factors ----

# Define UI ----
ui <- fluidPage(

    # > Application title ----
    titlePanel("The Sentimentor"),
    
    # > plot def ----
    sidebarLayout(position = "right",
                  mainPanel(
                      ## > tabset ----
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot",
                                           #Output: dot plot here
                                           plotOutput(outputId = "dotPlot"),
                                               p("The Sentimentor finds meaningful words in your text, assigns negative/postive ",
                                                 "sentiment to them and plots sentiment frequency per line."),
                                           p("For extra fun try ",
                                           a("this text", 
                                             href="https://www.gov.uk/government/speeches/pm-address-to-the-nation-on-coronavirus-10-may-2020"),
                                           "..."
                                           )
                                  ),
                                  tabPanel("What...?",
                                           ## >> What? tab ----
                                           p(),
                                           p("The Sentimentor:"),
                                           tags$ol(
                                               tags$li("automatically filters out the 'stopping' words (the/and/if etc); "),
                                               tags$li("assigns a positive/negative sentiment to the words that, according to the ",
                                             a("Bing lexicon", 
                                               href = "https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html"),
                                             ", have positive or negative sentiment;"),
                                             tags$li("counts how many of each sentiment you have per line and "),
                                             tags$li("plots the result by line number.")
                                             ),
                                           p("You may find you have multiple negative and/or positive words (or no sentimented words at all) per line."),
                                           p("If you don't see the plot check you have used some non-stopping words."),
                                         p("For extra fun try ",
                                           a("this text", 
                                             href="https://www.gov.uk/government/speeches/pm-address-to-the-nation-on-coronavirus-10-may-2020"),
                                           "..."
                                           )
                                  ),
                                  tabPanel("How do I...", 
                                           # >> How to tab ----
                                           h3("Blame someone?"),
                                           p("The Sentimentor is brought to you by ",
                                             a("@dataknut", href="https://twitter.com/dataknut"), ",",
                                             a("@energySoton", href="https://twitter.com/energysoton"), " and ",
                                             a("@UoSEngineering", href="https://twitter.com/UoSEngineering"), ".",
                                             "Based on an idea by",
                                             a("@draccoops", href="https://twitter.com/dracoops"), 
                                             " and made possible by ",
                                             a("@juliasilge", href="https://twitter.com/juliasilge"), 
                                             " and ",
                                             a("@drob", href="https://twitter.com/drob"), "'s #rstats ",
                                             a("#tidytext", href="https://juliasilge.github.io/tidytext/"),
                                             "package."
                                           ),
                                           h3("Check the results are 'right'?"),
                                           p("Tricky one. We think the calculations are 'right' given the inputs (your typing and the sentiment data).",
                                             ". ",
                                             "If you really really think they're wrong, please raise an issue on the code ",
                                             a("repo.", 
                                               href="https://github.com/dataknut/Sentimentor/issues?q=is%3Aissue")
                                           ),
                                           h3("Spread the word?"),
                                           p( 
                                               #https://community.rstudio.com/t/include-a-button-in-a-shiny-app-to-tweet-the-url-to-the-app/8113/2
                                               # Create url with the 'twitter-share-button' class
                                               tags$a(href=myParams$appUrl, "Twitter", class="twitter-share-button"),
                                               # Copy the script from https://dev.twitter.com/web/javascript/loading into your app
                                               # You can source it from the URL below. It must go after you've created your link
                                               includeScript("http://platform.twitter.com/widgets.js"),
                                               " is our viral medium of choice."
                                           ),
                                           h3("Check the data sources?"),
                                           p("'Bing' sentiment data is sourced from ",
                                             a("Bing Liu and collaborators", 
                                               href = "https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html"),
                                             "."
                                           ),
                                           p("Stop words are derived from the R package ",
                                             a("tidytext package", 
                                               href = "https://github.com/juliasilge/tidytext"), "'s",
                                             a(" stop_words", 
                                               href = "https://rdrr.io/cran/tidytext/man/stop_words.html"), 
                                             "data."),
                                           h3("Give feedback?"),
                                           p("Raise an issue on the code ",
                                             a("repo", 
                                               href="https://github.com/dataknut/Sentimentor/issues?q=is%3Aissue"), "."
                                           ),
                                           h3("Get the code?"),
                                           p("The code is open source under an ", 
                                             a("MIT license",
                                               href="https://github.com/dataknut/Sentimentor/blob/master/LICENSE") 
                                           )
                                  )
                      )
        ),
        
        sidebarPanel(
            ## sidebar ----
            fluidRow(
                "Input:",
                textAreaInput("speech", label = "", rows = 10)
            ),
            fluidRow(
                p("Type/paste whatever text you want into the box above. Multiple lines (like a poem) work best."),
                h5("Nothing visible?"),
                p("Paste this into the box:"),
                code("The quick brown fox jumped over the lazy dog"),
                br(),
                code("It was a wonderful wonderful day"),
                p(),
                p("The Sentimentor finds `lazy` and `wonderful`. ")
            )
        )
    ),
    hr(),
    h6(align = "center", "The Sentimentor: built by ",
         a("@dataknut", href="https://twitter.com/dataknut"),
         " based on an idea by",
         a("@draccoops", href="https://twitter.com/dracoops")
         )
)

# Define server ----
server <- function(input, output) {

    output$dotPlot <- renderPlot({
        # > Get input$xxxx from ui.R ----
        # parse into multiple lines as we go using \n
        # input <- list()
        # input$speech <- "A load of text which might have a \n new line but I'm \n not really sure"
        s <- unlist(strsplit(input$speech, "\n"))
        df <- as.data.frame.character(s)
        df$line <- as.character(df$s)
        tbl <- as.tbl(df)
        #tbl
        tbl$s <- NULL
        
        data(stop_words) # English language stop words (and/in/of etc)
        
        # > Tidy the data ----
        # Deep in tidyverse territory here
        
        
        # set line number
        df2 <- tbl %>%
          mutate(linenumber = row_number()) %>%
          ungroup()
        nTotalLines <- nrow(df2)
        # get each word on to each row
        tidy_df <- unnest_tokens(df2, output = "word", input = "line")
        nTotalWords <- nrow(tidy_df)
        # remove the stop words - they are usually the most frequent
        # and usually the least interesting
        reduced_df <- tidy_df %>%
          anti_join(stop_words)
        nPlotWords <- nrow(reduced_df)
        
        bing_sentiment_counts_by_line <- reduced_df %>%
          inner_join(get_sentiments("bing")) %>% # only keeps those with sentiments
          count(linenumber, sentiment, sort = TRUE) %>%
          ungroup()
        nSentimentedWords <- sum(bing_sentiment_counts_by_line$n)
        # sentimentByLinePlot ----
        ggplot2::ggplot(bing_sentiment_counts_by_line, 
                        aes(x = linenumber,
                            y = n,
                            colour = sentiment,
                            size = n)
                        ) +
          geom_point() +
          theme(legend.position = "bottom") +
            scale_x_reverse() +
          coord_flip() + 
          labs(x = "Line/sentence number",
               y = "Sentiment frequency (number of words by sentiment per line)",
               caption = paste0(myParams$plotCap,
                                "\nDetected ", nTotalLines, " lines and ", nTotalWords, " words",
                                " of which ", nPlotWords, " are non-stopping words and ", nSentimentedWords, " are sentimented."))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#speech <- "The quick brown fox jumped over the lazy dog \n It was a wonderful wonderful day"
