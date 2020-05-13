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

# Parameters ----
myParams <- list()
myParams$appUrl <- "https://dataknut.shinyapps.io/Speechinator/"
myParams$plotCap <-paste0("Your text, speechinated.",
                          "\nPlot by: @dataknut")

# load sentiment factors ----

# Define UI ----
ui <- fluidPage(

    # > Application title ----
    titlePanel("The Speechinator"),
    
    # > plot def ----
    sidebarLayout(position = "left",
        sidebarPanel(
            # Output: dot plot here
            plotOutput(outputId = "dotPlot"),
            hr(),
            # > brought to you by ----
            code("The Speechinator is brought to you by ",
              a("@dataknut", href="https://twitter.com/dataknut"), ",",
              a("@energySoton", href="https://twitter.com/energysoton"), " and ",
              a("@UoSEngineering", href="https://twitter.com/UoSEngineering"),
              "Based on an original idea by",
              a("@dracoops", href="https://twitter.com/dracoops"), "."
            ),
            hr()
        ),

        
        mainPanel(
            fluidRow(
                # > info header ----
                p("Type whaver you want. Multiple lines (like a poem) work best.",
                  " The Speechinator will automatically calculate the sentiment of each of your lines of text and plot it by line number.")
            ),
            tabsetPanel(type = "tabs",
                        tabPanel("Text input", 
                                 # >> text input tab
                                 fluidRow(
                                   textAreaInput("speech", "Type here", rows = 10)
                                 )
                        ),
                        tabPanel("How do I...", 
                                 # >> How to tab ----
                                 h3("Check the results are 'right'?"),
                                 p("Tricky one. We think the calculations are 'right' given the inputs (your typing and the sentiment data).",
                                   ". ",
                                   "If you really really think they're wrong, please raise an issue on the code ",
                                     a("repo.", 
                                       href="https://github.com/dataknut/speechinator/issues?q=is%3Aissue")
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
                                     href = "https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html")
                                 ),
                                 p("Stop words are derived from the R package ",
                                    a("tidytext", href = "https://github.com/juliasilge/tidytext"), 
                                                  " stop_words data."),
                                 h3("Give feedback?"),
                                 p("Raise an issue on the code ",
                                   a("repo", 
                                     href="https://github.com/dataknut/speechinator/issues?q=is%3Aissue"), "."
                                 ),
                                 h3("Get the code?"),
                                 p("The code is open source under an ", 
                                   a("MIT license",
                                     href="https://github.com/dataknut/speechinator/blob/master/LICENSE") 
                                 )
                        )
            )
        )
    )
)

# Define server ----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # > Get input$xxxx from ui.R ----
        df <- input$speech
        data(stop_words) # English language stop words (and/in/of etc)
        
        # > Tidy the data ----
        # Deep in tidyverse territory here
        
        # set line number
        df2 <- df %>%
          mutate(linenumber = row_number()) %>%
          ungroup()
        
        # get each word on to each row
        tidy_df <- unnest_tokens(df2, output = "word", input = "line")
        
        # remove the stop words - they are usually the most frequent
        # and usually the least interesting
        reduced_df <- tidy_df %>%
          anti_join(stop_words)
        
        bing_sentiment_counts_by_line <- reduced_df %>%
          inner_join(get_sentiments("bing")) %>%
          count(linenumber, sentiment, sort = TRUE) %>%
          ungroup()
        
        # sentimentByLinePLot ----
        ggplot2::ggplot(bing_sentiment_counts_by_line, aes(x = linenumber,
                                                           y = n,
                                                           colour = sentiment,
                                                           size = n)) +
          geom_point() +
          theme(legend.position = "bottom") +
          labs(x = "Line/sentence number",
               y = "Number of words",
               caption = myParams$plotCap)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
