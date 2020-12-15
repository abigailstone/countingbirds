library(shiny)
library(tidyverse)
library(ggrepel)
library(leaflet)

# ebd <- read_tsv('ebd_US-VT_201001_202011_relSep-2020.txt')
high_counts <- read_csv('high_counts.csv')

# colnames(ebd) <- str_replace_all(colnames(ebd), " ", "_")
high_counts <- high_counts %>%
   filter(as.numeric(OBSERVATION_COUNT) >= 100)

# get proportion of counts that begin with each digit
high_count_proportion <- high_counts %>% 
   mutate(first.digit = substr(OBSERVATION_COUNT, 1, 1)) %>% 
   group_by(first.digit) %>% 
   summarise(n = n()) %>% 
   mutate(freq = n/sum(n))

# determine common dividing factors of counts
high_counts <- high_counts %>% 
   mutate(first.digit = substr(OBSERVATION_COUNT, 1, 1),
          div.ten = OBSERVATION_COUNT %% 10 == 0,
          div.twfive = OBSERVATION_COUNT %% 25 == 0,
          div.hundred = OBSERVATION_COUNT %% 100 == 0)

# filter for only species with several observations of large numbers
species.multiple.high.counts <- high_counts %>%
   group_by(COMMON_NAME) %>% 
   summarize(n = n()) %>% 
   filter(n > 200)

# TEXT ANALYSIS
# for species comments 
with.comments <- high_counts %>% 
   filter(!is.na(SPECIES_COMMENTS))

# find species comments where observer has described "counted by 10s" or "counted by 50s"
comment.pattern <- 'by[:space:][:digit:]+'
with.count.desc <- str_detect(with.comments$SPECIES_COMMENTS, comment.pattern)

# extract counting method from species comments
count.comments <- with.comments %>% 
   filter(with.count.desc) %>% 
   mutate(substr = str_extract(SPECIES_COMMENTS, comment.pattern)) %>% 
   mutate(count.method = str_extract(substr, '[:digit:]+')) %>% 
   filter(!(count.method %in% c('23', '63', '4')))  # outliers that were another instance of "by" + digit 

count.freqs <- count.comments %>%
   # calculate relative frequency
   group_by(count.method) %>% 
   summarise(n = n()) %>%
   mutate(freq = n/sum(n)) %>% 
   ungroup()


ui <- fluidPage(
   
   # Title
   titlePanel("Counting Birds"),
   
   mainPanel(
      navlistPanel(
         "Introduction",
         tabPanel(
            "The eBird Dataset",
            
            h5("Abigail Stone"),
            h6("Math 216 - Final Project"),
            
            p("The",
              tags$a(href="https://ebird.org/science/download-ebird-data-products", "eBird Basic Dataset"),
              "contains all reports and metadata of bird observations submitted to the eBird website, an online database
               where both researchers and amateur observers submit their field observations. The eBird Basic Dataset (EBD) 
               is an excellent resource for studying the abundance and distribution of various species over time. For the purposes of
               this project, we will examine reports in Vermont between 2010 and 2020.
               During the fall migration season, certain species may be seen in flocks of several hundred or up to a few thousand.
               There are several techniques that birders use to estimate the number of birds in such flocks for their eBird reports,
               but an exact individual count of a very large flock can be nearly impossible."),
         ),
         "High Counts",
         tabPanel(
            "How many birds is \"a lot\"?",
            
            p("First, let's define a \"high count\" of birds to be an observation where more than 100 individuals of a 
               particular species are reported. A brief look at a histogram of these high-count reports can give us some 
               insight into the way large flocks are typically reported."),
            
            plotOutput(outputId = "countFreqPlot"),
            
            p("As we might expect, there are peaks at typical \"estimate\" numbers: 100, 500, 800, 1000, 1200, 1500, 2000, etc.
               We see more counts that fall in between these divisions in the lower range of numbers, since it is easier to count fewer brids.
               However, as the number of individuals gets higher, the estimate becomes more rough. "),
   
          
         ),
         tabPanel( 
            "Benford's Law",
            p("We might expect these observation reports to adhere to",
              tags$a(href="https://en.wikipedia.org/wiki/Benford%27s_law", "Benford's Law"),
              ", which predicts that the leading digits in many sets of numbers are likely to be small. In other words, about 30% of 
               reports will have 1 as the first digit, while only about 4% of reports will have 9 as a leading digit."),
            
            plotOutput(outputId = "benfordPlot"),
            
            p("These data do follow the general trend of Benford's Law, but far more than 30% of the reports have a 1 as a leading digit.
               This is likely due to estimation bias in the reporting of such large counts; it is easy to round up to 100 or 1000, especially
               when estimating how many individuals are in a large and moving flock."),
            
         ),
         "Species Level",
         tabPanel(
            "Large flocks by species",
            
            p("Large flocks are also interesting to explore on the species level. For example, Snow Geese are often seen in groups of 
            well over 1,000 during their fall migration. Similarly, starlings and other blackbirds (in this dataset, Red-winged Blackbirds,
            Rusty Blackbirds, European Starlings, and Common Grackles are noteworthy) form murmurations of several thousand."),
            
            selectInput("Species",
                        label = "Choose a species to filter:",
                        choices = unique(species.multiple.high.counts$COMMON_NAME)),
            
            plotOutput(outputId = "speciesHistPlot"),
 
         
        ),
         tabPanel(
            "Average size of large flocks",
                        
            p("Certain birds are seen in large flocks more often than others. As we can see in the above histogram, Snow Geese are 
               very often reported in very large numbers. Other birds in our \"high counts\" subset of the data may appear fewer times
               and in fewer numbers. In other words, 100 reports averaging around 100 Juncos tells us that Juncos are occasionally seen in large 
               flocks; over 1,000 reports averaging more than 1,000 Snow Geese indicates that we're very likely to see these birds 
               in more abundant numbers."),
            
            plotOutput(outputId = "speciesScatter")
 
         ),
        "Estimation Techniques",
        tabPanel(
           "By the Numbers",
           
           p("Since many large flocks are counted by estimating in groups of 5, 10, 50, 100, etc. depending on the size of the flock,
               we might expect that many of the reported values will be divisble by these numbers. In the following table, we see
               that more than 50% of the high counts for all of the most frequently reported high-count species are divisble by 10. The
               chances that these birds are actually travelling in groups that are exactly divisible by 10 are quite low; instead,
               we are seeing that birders are more likely to estimate a group of about 50 (or 100, or 1000) birds and report that 
               \"round\" estimate instead of trying to count the exact number of birds that are present."),
           
           
           tableOutput(outputId = "propTable"),
           
            
         ),
         tabPanel(
            "Observer Comments",
            p("When a user tries to report a very high count of individual birds, eBird prompts the user for a species-level comment
              explaining the protocols that they used to arrive at the number that they are reporting. Many users enter some variation of
              \"counted by 10s\" or \"estimated by 50s\". This means that instead of counting individual birds, the user counted a group of 
              about 10 (or 50 or 100, etc.) birds and then used that group size to visually estimate the size of the rest of the flock.
              We can extract the units of counting/estimation by looking for strings that match
              this pattern of \"by [number]\". The following plot shows the relative frequency of counting units that users report."),
            
            plotOutput(outputId = "countMethodHist"),
            
            sliderInput(inputId = "sizeSlider",
                        label="Set a range of flock sizes:",
                        min = min(count.comments$OBSERVATION_COUNT),
                        max = max(count.comments$OBSERVATION_COUNT),
                        value = (c(min(count.comments$OBSERVATION_COUNT), max(count.comments$OBSERVATION_COUNT))),
                        step = 50,
                        width = '100%'
                        ),
            
            p("When we look at all of the observations that have a comment explaining the counting methods used, 
              nearly half of time, the user counted by 10s. There
              are also a large proportion of observations that were counted by 50s or 100s; these are equally logical numbers to use for such
              estimations, but we can safely assume that the frequency is lower because there are fewer flocks whose size warrant estimation 
              by 100s."),
            p("If we limit the range of flock sizes that we're investigating, the frequency plot shifts significantly. Above 1,000 
              individuals, the most common estimation unit is 100. There are still quite a few observations that were counted by 
              10s, but those become more and more infrequent as the number of individuals increases.")
   
         ),
        "Maps",
        tabPanel(
          "Where are all these birds?!?",
          
          selectInput("mapSpecies",
                      label = "Choose a species to filter:",
                      choices = c("All", unique(species.multiple.high.counts$COMMON_NAME)),
                      selected = "All"),
          
          
          leafletOutput("vtmap"),
          
          p("This map allows us to understand where these high counts are being observed. As we might expect,
            many duck species (Greater Scaup, Lesser Scaup, and Common Mergansers, for example) are usually
            found in large numbers along Lake Champlain. Snow Geese are reliably observed during their migration
            around the Goose Viewing Area in Addison; unsurprisingly, we see a lot of Snow Goose observations 
            clustered around this area. A close inspection of Snow Bunting reports show us that these birds
            are often found in large numbers in open spaces: many of the high counts in this data set appear
            around the open farm fields of the Champlain Valley."),
          
          p("Note that the markers are colored by the number of high counts reported from that location (eBird 
            allows its users to cluster observations at \"Hotspots\", which is why we see many observations with 
            identical coordinates). Markers are sized by the average value of high counts from that location.")
           
        ),
        "Conclusion",
         tabPanel( "Conclusion",
         p("This exploration of bird data has revealed several insights into the way large flocks of birds are counted.
           We see trends in the way that people estimate, whether by 10s, 50s, or 100s, when reporting large numbers of birds.
           Many birders report high counts in numbers that are divisible by similarly \"round\" numbers, which indicates that
           we cannot guarantee complete accuracy within this dataset. This leads us to conclude that the accuracy of the data varies by observer, as some 
           people may tend to over- or under-estimate in their reports."),
         p("Though it is tempting to have full confidence in the accuracy of such a large set of reported data,
           it is also important to remember that this data is full of human error, estimatino, and subjective interpretation.")
            
         )
         
      ),
      h5("Abigail Stone"),
      h5("Math 216 - Final Project"),
   ),
  
   
)

server <- function(input, output, session) {
   
   output$countFreqPlot <- renderPlot({
      high_counts %>% 
         ggplot(aes(x = OBSERVATION_COUNT)) +
         geom_histogram(binwidth=10, fill="royalblue") + 
         coord_cartesian(xlim=c(100, 2500)) +
         scale_x_continuous(breaks = seq(0, 2500, 250)) +
         labs(x = "Reported Count",
              y = "Occurences") +
         theme_bw()
   })

   output$benfordPlot <- renderPlot({
      high_count_proportion %>%
         ggplot(aes(x = first.digit,
                    y = freq)) +
         geom_bar(stat = "identity",fill="royalblue") +
         theme_bw() +
         labs(x = "First Digit of Observation Count",
              y = "Frequency of Occurence",
              title = "Frequency of Leading Digits in Observation Counts") + 
         coord_flip()
   })
   
   speciesfilter <- reactive({high_counts %>%
         filter(COMMON_NAME == input$Species)
   })
   
   output$speciesHistPlot <- renderPlot({
      speciesfilter() %>% 
         ggplot(aes(x = OBSERVATION_COUNT)) +
         geom_histogram(fill="royalblue") + 
         labs(x = "Reported Count",
              y = "Number of Observations",
              title = "Reports of Large Flocks by Species") +
         theme_bw()
   })
   
   output$propTable <- renderTable({
      high_counts %>% 
         group_by(COMMON_NAME) %>% 
         summarise(divisible.ten = mean(div.ten),
                   divisible.twfive = mean(div.twfive),
                   divisible.hundred = mean(div.hundred),
                   total.reports = n()) %>% 
         arrange(-total.reports) %>% 
         head(10) %>% 
         rename(`Common Name` = COMMON_NAME, 
                `Divisible by 10` = divisible.ten,
                `Divisible by 25` = divisible.twfive,
                `Divisible by 100` = divisible.hundred,
                `Total Reports` = total.reports)
   })
   
   output$speciesScatter <- renderPlot({
      high_counts %>% 
         group_by(COMMON_NAME) %>% 
         summarise(n.high.counts = n(),
                   avg.high.count = mean(OBSERVATION_COUNT)) %>% 
         filter(n.high.counts > 60) %>%
         ggplot(aes(x = n.high.counts,
                    y = avg.high.count,
                    label = COMMON_NAME)) + 
         geom_point(color="royalblue") + 
         # geom_text(aes(label=COMMON_NAME)) + 
         labs(x = "Number of High Counts Reported",
              y = "Average Number of Individuals Reported") +
         geom_text_repel() +
         scale_x_log10() + 
         scale_y_log10() + 
         theme_bw()
   })
   
   
   countfilter <- reactive({count.comments %>%
         filter(OBSERVATION_COUNT >= input$sizeSlider[1],
                OBSERVATION_COUNT <= input$sizeSlider[2])
   })
   
   
   output$countMethodHist <- renderPlot({
      countfilter() %>%
         # summarize to get frequency values
         group_by(count.method) %>% 
         summarise(n = n()) %>%
         mutate(freq = n/sum(n)) %>% 
         
         # plot 
         ggplot(aes(x= reorder(count.method, as.numeric(count.method)),
                    y = freq)) + 
         geom_bar(stat="identity",
                  fill="royalblue") + 
         coord_flip() + 
         theme_bw() + 
         labs(x = "Reported Counting Unit", 
              y = "Frequency",
              title = "Common Counting Units") + 
         scale_y_continuous(expand = expansion(mult = c(0, .1))) # get rid of weird gap
      
   })
   
   
   mapspeciesfilter <- reactive({
      if(input$mapSpecies == "All"){
         high_counts
      } else {
         high_counts %>% filter(COMMON_NAME == input$mapSpecies)
      }
   })
   
   output$vtmap <- renderLeaflet({
      
      map.data <- mapspeciesfilter() %>%
         group_by(LATITUDE, LONGITUDE, LOCALITY) %>% 
         summarise(n = n(),
                   avg.high.count = mean(OBSERVATION_COUNT),
                   max.high.count = max(OBSERVATION_COUNT)) %>%
         filter(n > 5) 
      
      pal <- colorNumeric(colorRamp(c("#0A72FA", "#820AFA"), interpolate="linear"),
                          domain = map.data$n)
      
      map.data %>% 
         leaflet() %>%
         addTiles() %>% 
         setView(-73, 44, 7) %>%
         addCircleMarkers(
            lat = ~LATITUDE,
            lng = ~LONGITUDE,
            radius = ~avg.high.count/100,
            color = ~pal(n),
            popup = paste("<b>", map.data$LOCALITY, "</b><br>",
                          "Average High Count:", round(map.data$avg.high.count), "<br>",
                          "Maximum High Count:", round(map.data$max.high.count), "<br>",
                          "Number of High Counts:", round(map.data$n), "<br>")) %>%
         addLegend("bottomright",
                   pal = pal,
                   values = ~n,
                   title = "Number of High Count Reports")
      
   })
 
}

shinyApp(ui, server)