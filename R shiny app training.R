library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

ui <- fluidPage(
  h4("Intro to Cumulative Claim"),
  fileInput("upload", "Choose claims data file"),
  sliderInput("tail", "Tail Factor", value = 1, min = 1, max = 2, step = 0.1),
  tableOutput("my_table"),
  plotOutput("my_plot")
) 

server <- function(input, output, session) {
  my_data <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath, header = TRUE)
  })
  
  # Create reactive tail factor
  tail_factor <- reactive({
    input$tail
  })
  
  # Create reactive table
  table_data <- reactive({
    data <- my_data()
    spread_data <- data %>% 
      group_by(Loss_Year) %>% 
      mutate(CumulativeClaims = cumsum(Amount_of_Claims_Paid)) %>% 
      select(Loss_Year, Development_Year, CumulativeClaims) %>%
      spread(key = Development_Year, value = CumulativeClaims, fill = NA)
      
    for (i in 2:ncol(spread_data)) {
      factor_col <- as.character(i - 1)
      current_col <- as.character(i)
      next_col <- as.character(i + 1)
      
      if (i != ncol(spread_data)) {
        
    
      factor <- sum(spread_data[, current_col][!is.na(spread_data[, current_col])] /
                      sum(spread_data[, factor_col][!is.na(spread_data[, current_col])]), na.rm = TRUE)

      spread_data[which(is.na(spread_data[, current_col])), current_col] <- 
        as.integer(unlist(spread_data[which(is.na(spread_data[, current_col])), factor_col]) * factor)
      
      } else {
        spread_data[,current_col] <- spread_data[,factor_col] * tail_factor()
      }
    }
    
    # Format the numbers with thousands separator
    spread_data[, 2:ncol(spread_data)] <- lapply(spread_data[, 2:ncol(spread_data)], function(x) format(round(x), big.mark = ","))
    
    colnames(spread_data)[1] <- "Loss Year"
    print(spread_data)
    spread_data
  })
  
  output$my_table <- renderTable({
    table_data() 
  })
  
  # Plot the data
  output$my_plot <- renderPlot({
    
    #Redefine the data frame to 3 columns  
    last_col_name <- colnames(table_data())[ncol(table_data())]
    long_data <- gather(table_data(), Development_Year, CumulativeClaims, `1`:`last_col_name`)
    
    long_data <- long_data %>%
      mutate(CumulativeClaims = gsub(",", "", CumulativeClaims)) %>%  # remove commas
      mutate(CumulativeClaims = as.numeric(CumulativeClaims))         # convert to numeric
    
    print(long_data)
    
    ggplot(long_data, aes(x = Development_Year, y = CumulativeClaims, color = factor(`Loss Year`), group = `Loss Year`)) +
      geom_line(size = 1) +
      labs(title = "Cumulative Paid Claims ($) - Graph",
           x = "Development Year",
           y = "Cumulative Paid Claims ($)",
           color = "Loss Year") + 
      
      guides(shape = guide_legend(override.aes = list(size = 4, linetype = 0, pch = 21))) +
      
      geom_text(aes(label = scales::comma_format()(round(CumulativeClaims,0))), vjust = -1) + 
      
      scale_y_continuous(labels = scales::comma_format(), limits = c(500000, 2500000), 
                         breaks = seq(500000, 2500000, by = 250000)) +
      theme(panel.grid.major = element_line(color = "gray", size = 0.5),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.9, 0.8)) 
  })
}

shinyApp(ui, server)

