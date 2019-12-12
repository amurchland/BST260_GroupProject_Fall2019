if (!require("pacman")) install.packages("pacman", repos='http://cran.us.r-project.org'); library(pacman)
p_load("shiny", "shinythemes", "dplyr", "sf", "ggplot2", "viridis")

load("BST260_workspace.Rdata")

ui <- fluidPage(
  # Change theme to darkly
  theme = shinythemes::shinytheme("darkly"),
  # Application title
  titlePanel("Restaurants in Boston by Yelp Rating"),
  
  # Create an app with 2 tabs
  tabsetPanel(
    tabPanel("Restaurants",
             sidebarLayout(
               sidebarPanel(
                 p("Plot of restaurants in Brookline and the surrounding 2 mile radius 
                   based on user preference. From the dropdown menu below, select restaurant
                   features you would be interested in finding a restaurant from"),
                 
                 # Add some space between the text above and animated
                 br(),
                 
                 selectInput(inputId = "City", label = "Location",
                             choices = as.list(levels(DT_sf$City))) 
                 ),
               
               # Show a scatter plot for all countries
               mainPanel(
                 plotOutput("mapPlot"),
                 br(), 
                 br(),
                 dataTableOutput("table")
               )
               )), 
    # Second tab: 
    tabPanel("K-Means Clustering",
             sidebarLayout(
               sidebarPanel(
                 p("Plot describing the output of the K-means clustering algorithm for classifying
                   restuarants by number of reviews, price and rating"), 
                 br(), 
                 radioButtons(inputId = "num", label = "Clusters", selected = NULL, 
                              inline = FALSE, width = NULL, choices = c(1,2,3,4,5,6,7,8))
               ),
          
               mainPanel(
                 plotOutput("cluster")
                 
               ))
    ), 
    
    ## Third tab: 
    tabPanel("Clusters by Location", 
             sidebarLayout(
               sidebarPanel(
                 p("Output of K-means clustering algorithm by location. We found that 7 clusters
                   maximized the Gap statistic."),
                 br(), 
                 radioButtons(inputId = "num", label = "Number of Clusters", selected = NULL, 
                              inline = FALSE, width = NULL, choices = c(7))
               ), 
               
               mainPanel(
                 plotOutput("clustermap")
               ))
             ), 
    
    ## Fourth Tab 
    tabPanel("Summary Statistics", 
             sidebarLayout(
               sidebarPanel(
                 p("Summary statistics describing relationships between different variables. 
                   The following plots show relationships between key variables of interest including
                   number of reviews per restaurant, price ($, $$ or $$$), and rating."),
                 br(), 
                 checkboxGroupInput(inputId = "money", label = "Price:", selected=2,
                                    choiceNames = list("$","$$","$$$"), 
                                    choiceValues = c(1,2,3)
                 ),
                 
                 sliderInput(inputId = "reviews", label = "At Least This Many Reviews:",
                             min = 8, max = 1290, value = 500)
              ),
                 mainPanel(
                   plotOutput("hist"),
                   br(), 
                   br(),
                   plotOutput("violin_plot_price"), 
                   br(), 
                   br(), 
                   plotOutput("relationship")
                 ))
    )
    
    
    
  )
)

server <- function(input, output){
  
  output$mapPlot <- renderPlot({
    data <- tmp_transformed_with_lat_lon %>% 
      dplyr::filter(City == input$City)
    
    ggplot2::ggplot() + 
      geom_sf(data = test, aes(fill = ids)) +
      geom_point(data = data, aes(x = X, y = Y), fill = "white", 
                 col = "darkturquoise", 
                 size = 3.5, 
                 shape = 21, 
                 stroke = 2) + 
      coord_sf(crs = 102003) + 
      scale_fill_manual(values = c(viridis(19, 
                                           option = "A", 
                                           direction = -1))) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            line = element_blank(),
            rect = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 16, face = "bold") , 
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
      labs(fill = "Neighborhoods")
    
  })
  
  selected <- reactive(tmp_transformed_with_lat_lon %>% dplyr::filter(City == input$City))
  
  output$table <- renderDataTable(selected() %>% 
                                    select(Name, N_reviews, Type, Keyword1, Keyword2, Price_factor,
                                           Rating, Monday, Tuesday, Wednesday, 
                                           Thursday, Friday, Saturday, Sunday))
  
  output$cluster <-  renderPlot({
    km.out=kmeans(dat[,c(2, 6, 15, 16, 17)], input$num, nstart = 2)
    plot(dat[,c(2, 6, 15, 16, 17)], 
         col=(km.out$cluster+1), 
         main="K-Means Clustering Results", 
         pch=20, 
         cex=2)
  })
  
  output$clustermap <- renderPlot({
    km.out=kmeans(dat[,c(2, 6, 15, 16, 17)], 7, nstart = 2)
    
    tmp_transformed_with_lat_lon$cluster <- km.out$cluster
    
    ggplot() +
      geom_sf(data = test) +
      geom_point(data = tmp_transformed_with_lat_lon, 
                 aes(x = X, y = Y, color = as.factor(cluster)), size = 3, 
                 position = position_jitter(w = 0.1, h = 0)) + 
      coord_sf(crs = 102003) + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            line = element_blank(),
            rect = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 16, face = "bold") , 
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
      labs(col = "Cluster")
  })
  
  output$hist <- renderPlot({
    tmp_transformed_with_lat_lon %>% 
      select(N_reviews, Price, Rating) %>% 
      filter(N_reviews <= input$reviews) %>% 
      filter(Price == input$money) %>% 
      ggplot() + 
      geom_histogram(aes(x = Rating, fill = as.factor(Rating)), binwidth = 0.5) + 
      xlab("Rating") + 
      ylab("Number of Restaurants") + 
      labs(fill = "Rating") + 
      theme_light() +  
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            axis.title.x = element_text(size=16), 
            axis.title.y = element_text(size=16), 
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.text.x=element_text(angle=0, hjust=1, size = 14),
            axis.text.y = element_text(size = 14)) + 
      scale_fill_viridis(discrete = T, option = "A", direction = -1, begin = 0.4, end = 0.8)
  })
  
  output$violin_plot_price <- renderPlot({
    tmp_transformed_with_lat_lon %>% 
      select(Price, Rating, N_reviews) %>% 
      ggplot() + 
      geom_violin(aes(x = as.factor(Price), y = N_reviews, fill = as.factor(Price))) + 
      scale_fill_viridis(discrete = T, option = "A", direction = -1, begin = 0.4, end = 0.8) + 
      theme_light() + 
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            axis.title.x = element_text(size=16), 
            axis.title.y = element_text(size=16), 
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.text.x=element_text(angle=0, hjust=1, size = 14),
            axis.text.y = element_text(size = 14)) + 
      labs(x = "Price", y = "Number of Reviews", fill = "Price") + 
      guides(fill = FALSE)
  })
  
  output$relationship <- renderPlot({
    a <- tmp_transformed_with_lat_lon %>%
      filter(Price %in% c(1, 2, 3)) %>%
      group_by(Price, Rating) %>%
      summarise(counts = n()) 
    
    
    ggplot(a, aes(x = as.factor(Rating), y = counts)) +
      geom_bar(
        aes(color = as.factor(Price), fill = as.factor(Price)),
        stat = "identity", position = position_dodge(0.8),
        width = 0.7
      ) + 
      theme_light() + 
      scale_fill_viridis(discrete = T, option = "A", direction = -1, begin = 0.4, end = 0.8) + 
      scale_color_viridis(discrete = T, option = "A", direction = -1, begin = 0.4, end = 0.8) + 
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            axis.title.x = element_text(size=16), 
            axis.title.y = element_text(size=16), 
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.text.x=element_text(angle=0, hjust=1, size = 14),
            axis.text.y = element_text(size = 14)) + 
      labs(x = "Rating", y = "Number of Restaurants", fill = "Price") + 
      guides(color = FALSE)
      
  })
  
}  


shinyApp(ui = ui, server = server)
