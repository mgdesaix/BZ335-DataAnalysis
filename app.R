################ Bird Arrival Time Shiny App ##################################
## BZ335: Data Analysis Exercise
## Created by: Matt DeSaix

library(tidyverse)
library(shiny)
library(gridExtra)


temp <- read_csv("./Fort-Collins-Temp-Data-wrcc.csv")
colnames(temp) <- unlist(lapply(str_split(names(temp), " "), '[[', 1))

ebird <- read_csv("./ebd_filtered.csv") %>%
  rename(Year = year) %>%
  mutate(Ordinal_date = format(as.Date(observation_date, format = "%y%m%d"), "%j") %>%
           as.numeric()
         )

#######################################################################################


ui <- fluidPage(
  titlePanel("Data Analysis Exercise: Colorado Bird Arrival Times"),
  
    tabsetPanel(type = "tabs",
                tabPanel("Main Page",
                         sidebarLayout(
                           
                           # Sidebar panel for inputs
                           sidebarPanel(
                             
                             # Species dropdown list
                             selectInput(inputId = "species",
                                         label = "Species:",
                                         choices = unique(ebird$common_name)
                               
                             ),
                             # Temperature dropdown list
                             selectInput(inputId = "month",
                                         label = "Temperature from month:",
                                         choices = names(temp)[-1]
                             ),
                             
                             downloadButton(outputId = "download",
                                            label = "Download: "),
                             
                             tableOutput(outputId = "model")
                          
                             
                           ), # close sidebarPanel
                           
                           # Main panel with output
                           mainPanel(
                             textOutput("intro"),
                             br(),
                             br(),
                             
                             plotOutput(outputId = "BirdTempPlot"),
                             
                             dataTableOutput(outputId = "table")
                             # h3("Arrival Date versus Temperature"),
                             # plotOutput(outputId = "BirdTempPlot2")
                           )
                         )
                ) #close tabPanel Main Page
    ) # close tabsetPanel
  
 
) # close fluidPage


server <- function(input, output) {
  
  output$model <- renderTable({
    
    month <- input$month
    species <- input$species
    
    ebird.tmp <- ebird %>%
      filter(common_name == species) %>%
      inner_join(temp, by = "Year")
    
    ebird.lm1 <- summary(lm(Ordinal_date ~ Year, data = ebird.tmp))
    adj.r1 <- ebird.lm1$adj.r.squared %>%
      round(4)
    pval1 <- ebird.lm1$coefficients[2,4] %>%
      round(4)
    
    
    
    ebird.lm2 <- summary(lm(ebird.tmp$Ordinal_date ~ temp[,month] %>% pull()))
    adj.r2 <- ebird.lm2$adj.r.squared %>%
      round(4)
    pval2 <- ebird.lm2$coefficients[2,4] %>%
      round(4)
    
    statsum <- tibble(
      "Model" = c("OrdinalDay~Year", "OrdinalDay~Temp"),
      "R2" = c(adj.r1, adj.r2),
      "P-value" = c(pval1, pval2)
    )
    
    statsum
    
  })
  
  plotInput <- reactive({

    # Get values from input IDs above in UI
    month <- input$month
    species <- input$species
    
    ebird.tmp <- ebird %>%
      filter(common_name == species) %>%
      inner_join(temp, by = "Year")
    
    p1 <- ebird.tmp %>%
      ggplot(aes(x = Year, y = Ordinal_date)) +
      geom_point() +
      ylab("") +
             theme_bw() +
      geom_smooth(method = lm,
                  alpha = 0.5)
    
    p2 <- ebird.tmp %>%
      ggplot(aes_string(x = month, y = "Ordinal_date")) +
      geom_point()  +
      xlab(paste("Temperature (F) in ", month)) +
      ylab(paste("Ordinal Day of Arrival of ", species)) +
      theme_bw() +
      geom_smooth(method = lm,
                  alpha = 0.5)
    
    pfull <- grid.arrange(p1, p2, nrow = 2)
    
    
  })
  
  output$BirdTempPlot <- renderPlot({
    print(plotInput())
  })
  
  output$intro <- renderText({
    paste("Hi BZ335 students! Here is a web app to help complete the data analysis assignment.
          Choose a species of interest in the dropdown menu on the left, as well as the month of temperatures you are interested in.
          The plots on the right will automatically update based on the chosen information.  The y axes of these
          two plots are the same (Ordinal Day of Arrival) - but one is a function of year and the other a function of monthly temperature.
          Note that ordinal day is just the day number, ranging from 1-366. For example, Jan 8 -> 8 and February 1 -> 32.
          The summary statistics on the left show the adjusted r-squared value and the p-value for both of the models.
          If the p-value shows 0, that means it is <0.01, and is being shown as 0 because of rounding.
          You can produce the plots you need and then click the download button to download the plot and include
          in your assignment.")
  })
    
    output$download <- downloadHandler(
      filename = "Birdplot.png",
      content = function(file){
        ggsave(file, plotInput())
      }
    )
    
    output$table <- renderDataTable({
      month <- input$month
      species <- input$species
      
      ebird %>%
        filter(common_name == species) %>%
        inner_join(temp, by = "Year")
    })
    
  
}

shinyApp(ui = ui, server = server)
