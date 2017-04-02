

server <- function(input, output) {
  
  output$Area_ui_a <- renderUI({
    area_choices <- HPI_AT_metro %>%
      filter(State == input$state_a) %>%
      select(Area)
    
    selectInput(inputId = 'Area_a',
                label = 'Region A',
                selectize = FALSE,
                size = 1,
                choices = c('None', unique(area_choices$Area)[order(unique(area_choices$Area))]))
  })
  
  output$Area_ui_b <- renderUI({
    area_choices <- HPI_AT_metro %>%
      filter(State == input$state_b) %>%
      select(Area)
    
    selectInput(inputId = 'Area_b',
                label = 'Region B',
                selectize = FALSE,
                size = 1,
                choices = c('None', unique(area_choices$Area)[order(unique(area_choices$Area))]))
  })
  
  
  housing_plot_data_year <- reactive({
    
    HPI_AT_metro %>%
      filter(Year >= input$year[1], Year <= input$year[2]) %>%
      arrange(Year_Quarter_num) %>%
      group_by(Core_Based_Statistical_Area) %>%
      mutate(Housing_Price_Index =(Housing_Price_Index / Housing_Price_Index[1]) * 100) %>%
      ungroup
  })
  
  date_range <- reactive({
    
    if((input$year[1] - floor(input$year[1])) == 0){
      min_month <- 'Jan'
    }
    
    if((input$year[1] - floor(input$year[1])) == .25){
      min_month <- 'Apr'
    }
    
    if((input$year[1] - floor(input$year[1])) == .50){
      min_month <- 'Jul'
    }
    
    if((input$year[1] - floor(input$year[1])) == .75){
      min_month <- 'Oct'
    }
    
    if((input$year[2] - floor(input$year[2])) == 0){
      max_month <- 'Jan'
    }
    
    if((input$year[2] - floor(input$year[2])) == .25){
      max_month <- 'Mar'
    }
    
    if((input$year[2] - floor(input$year[2])) == .50){
      max_month <- 'Jun'
    }
    
    if((input$year[2] - floor(input$year[2])) == .75){
      max_month <- 'Sept'
    }
    
    paste0(paste0(min_month, '-', floor(input$year[1])), '  -  ',
           paste0(max_month, '-', floor(input$year[2])))
    
  })
  
  
  housing_plot_data_msa_a <- reactive({ 
    housing_plot_data_year() %>%
      filter(State == input$state_a) %>%
      filter(Area == input$Area_a)
  })
  
  housing_plot_data_msa_a_perc_change <- reactive({
    
    if(nrow(housing_plot_data_msa_a()) == 0){return(NULL)}
    old_val <- housing_plot_data_msa_a() %>%
      filter(Year_Quarter_num == min(Year_Quarter_num )) %>%
      select(Housing_Price_Index) %>% as.numeric()
    
    new_val <- housing_plot_data_msa_a() %>%
      filter(Year_Quarter_num == max(Year_Quarter_num )) %>%
      select(Housing_Price_Index) %>% as.numeric()
    
    perc_change <- round((new_val - old_val) / old_val, 2) * 100

    if(sign(perc_change) == 1){
      sign_change <- '+'
    }
    
    if(sign(perc_change) == -1){
      sign_change <- '-'
    }
    
    if(sign(perc_change) == 0){
      sign_change <- ''
    }

    paste0(sign_change, perc_change, '%')
  })
  
  
  housing_plot_data_msa_b <- reactive({ 
    housing_plot_data_year() %>%
      filter(State == input$state_b) %>%
      filter(Area == input$Area_b)
  })
  
  housing_plot_data_msa_b_perc_change <- reactive({
    
    if(nrow(housing_plot_data_msa_b()) == 0){return(NULL)}
    old_val <- housing_plot_data_msa_b() %>%
      filter(Year_Quarter_num == min(Year_Quarter_num )) %>%
      select(Housing_Price_Index) %>% as.numeric()
    
    new_val <- housing_plot_data_msa_b() %>%
      filter(Year_Quarter_num == max(Year_Quarter_num )) %>%
      select(Housing_Price_Index) %>% as.numeric()
    
    perc_change <- round((new_val - old_val) / old_val, 2) * 100
  
    if(sign(perc_change) == 1){
      sign_change <- '+'
    }
    
    if(sign(perc_change) == -1){
      sign_change <- '-'
    }
    
    if(sign(perc_change) == 0){
      sign_change <- ''
    }
    
    paste0(sign_change, perc_change, '%')
  })
  
  
  nearest_line <- reactive({
    near_point <- nearPoints(housing_plot_data_year(),
                             input$housing_plot_hover,
                             xvar =  'Year_Quarter_num',
                             yvar = 'Housing_Price_Index',
                             threshold = 100,
                             maxpoints = 1)
    paste0(near_point$Area, ', ', near_point$State)
  })
  
  output$housing_plot_hover <- renderText({
    paste0('<br/>&nbsp&nbsp', nearest_line())
  })
  
  output$housing_plot <- renderPlot({
    
    x_range <- range(housing_plot_data_year()$Year_Quarter_num)
    
    if(is.null(input$Area_a)){return(NULL)}
    if(is.null(input$Area_b)){return(NULL)}
    
    if(input$Area_a != 'None'){
      state_check_a  <- HPI_AT_metro %>% filter(State == input$state_a) %>% filter(Area == input$Area_a)
      if(nrow(state_check_a) == 0){return(NULL)} 
    }
    
    if(input$Area_b != 'None'){
      state_check_b  <- HPI_AT_metro %>% filter(State == input$state_b) %>% filter(Area == input$Area_b)
      if(nrow(state_check_b) == 0){return(NULL)}   
    }
    
    housing_plot <- ggplot() +
      geom_line(data = housing_plot_data_year(), 
                aes(x = Year_Quarter_num,
                    y = Housing_Price_Index,
                    group = Core_Based_Statistical_Area),
                alpha = .1) +
      geom_point(data = housing_plot_data_year(), 
                 aes(x = Year_Quarter_num,
                     y = Housing_Price_Index),
                 alpha = 0) +
      theme_bw() +
      ggtitle(date_range()) +
      theme(legend.position = "none", 
            legend.title = element_blank(),
            text = element_text(size  = 20),
            axis.title.y = element_text(vjust = 1.5),
            axis.title.x = element_text(vjust = -.2)) +
      xlab('Year') + 
      ylab('Housing Price Index') + 
      coord_cartesian(xlim = x_range)
    if(!is.null(input$Area_a)){
      if(input$Area_a != "None"){
        housing_plot <-housing_plot +
          geom_line(data = housing_plot_data_msa_a(), 
                    aes(x = Year_Quarter_num,
                        y = Housing_Price_Index,
                        group = Core_Based_Statistical_Area),
                    colour = 'green',
                    size = 3) + 
          annotate(geom = 'point',
                   colour = 'green',
                   hjust = 0,
                   size = 5,
                   x = min(housing_plot_data_year()$Year_Quarter_num) +
                     ((max(housing_plot_data_year()$Year_Quarter_num)-
                         min(housing_plot_data_year()$Year_Quarter_num))* .025),
                   y = max(housing_plot_data_year()$Housing_Price_Index)) +
          annotate(geom = 'text',
                   label = gsub(pattern = '--', '-', housing_plot_data_msa_a_perc_change()),
                   hjust = 0,
                   size = 5,
                   color = ifelse(grepl('-', housing_plot_data_msa_a_perc_change()), 'red', 'black'),
                   x = min(housing_plot_data_year()$Year_Quarter_num) +
                     ((max(housing_plot_data_year()$Year_Quarter_num)-
                         min(housing_plot_data_year()$Year_Quarter_num))* 0.0375),
                   y = max(housing_plot_data_year()$Housing_Price_Index)) +
          annotate(geom = 'text',
                   label = input$Area_a,
                   hjust = 0,
                   x = min(housing_plot_data_year()$Year_Quarter_num) +
                     ((max(housing_plot_data_year()$Year_Quarter_num)-
                         min(housing_plot_data_year()$Year_Quarter_num))* 0.09),
                   y = max(housing_plot_data_year()$Housing_Price_Index))
      }
    }
    
    if(!is.null(input$Area_b)){
      if(input$Area_b != "None"){
        housing_plot <- housing_plot +
          geom_line(data = housing_plot_data_msa_b(), 
                    aes(x = Year_Quarter_num,
                        y = Housing_Price_Index,
                        group = Core_Based_Statistical_Area),
                    colour = 'blue',
                    size = 3) +
          annotate(geom = 'point',
                   colour = 'blue',
                   hjust = 0,
                   size = 5,
                   x = min(housing_plot_data_year()$Year_Quarter_num) +
                     ((max(housing_plot_data_year()$Year_Quarter_num)-
                         min(housing_plot_data_year()$Year_Quarter_num))* 0.025),
                   y = max(housing_plot_data_year()$Housing_Price_Index)-
                     (max(housing_plot_data_year()$Housing_Price_Index) * .05)) +
          annotate(geom = 'text',
                   label = gsub(pattern = '--', '-', housing_plot_data_msa_b_perc_change()),
                   hjust = 0,
                   size = 5,
                   color = ifelse(grepl('-', housing_plot_data_msa_b_perc_change()), 'red', 'black'),
                   x = min(housing_plot_data_year()$Year_Quarter_num) +
                     ((max(housing_plot_data_year()$Year_Quarter_num)-
                         min(housing_plot_data_year()$Year_Quarter_num))* 0.0375),
                   y = max(housing_plot_data_year()$Housing_Price_Index)-
                     (max(housing_plot_data_year()$Housing_Price_Index) * .05)) + 
          annotate(geom = 'text',
                   label = input$Area_b,
                   hjust = 0,
                   x = min(housing_plot_data_year()$Year_Quarter_num) +
                     ((max(housing_plot_data_year()$Year_Quarter_num)-
                         min(housing_plot_data_year()$Year_Quarter_num))* 0.09),
                   y = max(housing_plot_data_year()$Housing_Price_Index)-
                     (max(housing_plot_data_year()$Housing_Price_Index) * .05))
        
        
      }
    }
    housing_plot
  }, height = 450, width = 1050, units = "px")
}