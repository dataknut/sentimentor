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
                                             href="https://www.gov.uk/government/speeches/pm-address-to-the-nation-on-coronavirus-10-may-2020",
                                             target = "text"),
                                           "..."
                                           )
                                  ),
                                  tabPanel("What...?",
                                           ## >> What? tab ----
                                           p(),
                                           includeMarkdown("what.md")
                                  ),
                                  tabPanel("How do I...", 
                                           # >> How to tab ----
                                           includeMarkdown("howDoI.md")
                                  )
                      )
        ),
        
        sidebarPanel(
            ## Sidebar ----
            fluidRow(
                "Input:",
                textAreaInput("speech", label = "", rows = 10)
            ),
            fluidRow(
                p("Type/paste whatever text you want into the box above. Multiple lines (like a poem) work best."),
                p(tags$b("Nothing visible?"), "Paste this into the box:"
                    ),
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
         a("@draccoops", href="https://twitter.com/draccoops")
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
          labs(x = "Line number",
               y = "Sentiment frequency (number of words by sentiment per line)",
               caption = paste0(myParams$plotCap,
                                "\nDetected ", nTotalLines, " lines and ", nTotalWords, " words",
                                " of which ", nPlotWords, " are non-stopping words and ", nSentimentedWords, " are sentimented."))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#speech <- "The quick brown fox jumped over the lazy dog \n It was a wonderful wonderful day"
