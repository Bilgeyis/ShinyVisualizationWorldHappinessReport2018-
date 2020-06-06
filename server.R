library(shiny)
source("dataprep.R")

#once the app starts runnin all these functions are called once
# it should connect with appropriate links offered by github
# downoal the most recent data and join it in one tibble

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  options(shiny.sanitize.errors = FALSE)
  
   # renderPlot is an R function and it has curly brackets 
  # because of the curly brackets we dont have to include comma
  output$worldPlot1 <- renderPlot({
    
    #
    
    plain <- theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5)
    )
    
    
    missing_countries<-world %>% filter(world$Country_name== c('Sudan', 'Angola', 'Namibia' ,'Cuba', 'Taiwan', 'Eritrea', 'Gambia', 'Guam', 'Malaysia', 'Oman', 'Qatar', 'Swaziland', 'Syria'))
    
    all_countries <- bind_rows(happySubset_17,missing_countries)
    
    ggplot(data = all_countries, mapping = aes(x = long, y = lat, group = group)) + 
      geom_polygon(aes(fill = `Life Ladder`), colour = "black") +
      scale_fill_distiller(palette ="YlOrRd", direction = -1) + 
      ggtitle("Life Ladder Index for 2017 for the whole world") +
      plain +
      geom_text(data= happySubset_17test,aes(x= mean_long, y= mean_lat, label= Country_name),
                color = "black", check_overlap = TRUE, size = 3, hjust = 0) 
    
    
    
  }, height = 600, width = 900)
  
  output$worldPlot2 <- renderPlot({
    

    
    ggplot(data = all_countries %>% filter(Region %in% input$region), mapping = aes(x = long, y = lat, group = group)) + 
      geom_polygon(aes(fill = `Life Ladder`), colour = "black") +
      scale_fill_distiller(palette ="YlOrRd", direction = -1) + 
      ggtitle("Life Ladder Index for 2017 for selected region") +
      plain +
      geom_text(data= happySubset_17test %>% filter(Region %in% input$region),aes(x= mean_long, y= mean_lat, label= Country_name),
                color = "black", check_overlap = TRUE, size = 3, hjust = 0) 
    
    
  })
  

  
  
  output$worldPlot3 <- renderPlot({
    
    validate(
      need(input$country != '', 'Please choose a country. You can select more than one')
    )


    
    #countries_allyears = rbind(total[total$Country_name == input$country,])
    countries_allyears= total %>% filter(Country_name %in% input$country)
    ggplot(countries_allyears, aes(fill=countries_allyears$Country_name, y=countries_allyears$`Life Ladder`, x=countries_allyears$Year)) + 
      geom_bar(width = 0.6, position= position_dodge(width=0.5),stat="identity", colour="black") +
      facet_wrap(~Country_name,  scales = "free", shrink =  FALSE) + 
      theme_bw() +
      theme(strip.text = element_text(size=15, face="bold"))+
      theme(legend.position="none")+
      #theme(panel.grid.major  = element_line(colour = "black", size = 0.2))+
      #theme(panel.grid.minor  = element_line(colour = "black", size = 0.2))+
      theme(axis.text.x = element_text(angle = 30, hjust =1, vjust =0.5, size=12))+
      labs(x = expression(paste("Years")), y = expression(paste("Happiness evolution per selected countries")))
    
    
  })
  
  output$worldPlot4 <- renderPlot({

   gdp_2017 <- total %>% filter(Year %in% input$Year) %>%
      dplyr::select("Country_name", "Life Ladder", "Country Code", "Region", "Year", "Population", "Log GDP per capita",  "Generosity",  "GINI index (World Bank estimate), average 2000-16")


  
    ggplot(gdp_2017, aes(x = unlist(gdp_2017[input$variable]), y = Region, fill = Region )) +
      geom_density_ridges(inherit.aes = TRUE) +
      theme_ridges() +
      labs (x = input$variable) +
      theme(legend.position = "none")



  })
  
  output$worldPlot5 <- renderPlot({
    

    gdp_box <- total %>% filter(Year %in% input$year)
    
    theme_set(theme_classic())
    
    # Plot
    
    
    ggplot(gdp_box, aes(Region, unlist(gdp_box[input$variable1]))) +
      geom_boxplot(varwidth=T, fill="plum") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
      labs(title=input$year, 
           x="Regions of the World",
           y=input$variable1)
    
    
    
  })

  # output$worldPlot6 <- renderStreamgraph({
  #   
  #   
  #   generosity = subset(total,select= c( 1, 2, 8, 21))
  #   
  #   
  #   
  #   
  #   generosity_mean <- generosity %>%
  #     group_by(Region, Year) %>%
  #     summarise(Mean.response = mean(Generosity, na.rm = TRUE))
  #   
  #   
  #   
  #   
  #   generosity_mean %>%
  #     group_by(Year, Region) %>%
  #     streamgraph("Region", "Mean.response", "Year")%>%
  #     sg_axis_x(1, "year", "%Y") %>% 
  #     sg_legend(TRUE, "Region ")
  #   
  #   
  #   
  #   
  # })
  
  output$regression<-renderPlot({ggplot(data=total %>% filter(Year==input$year1),
                                        aes(x=`GINI index (World Bank estimate), average 2000-16`,
                                            y=`Life Ladder`, colour = Region, size = Population))+
      geom_point() +
      labs (title = "Linear regression approximation between Happiness Index and Log GDP Per Capita and GINI", 
            y= "Happiness Index", x= "GINI") +
      geom_abline(intercept =  7.045, slope =  -4.169) + 
      guides (size = FALSE)})
  
  #Regression Life ladder~Log GDP per capita
  output$regression2<-renderPlot({ggplot(data=total %>% filter(Year==input$year1),
                                         aes(x=`Log GDP per capita`,y=`Life Ladder`, 
                                             colour = Region, size = Population))+
      geom_point() +
      labs (y= "Happiness Index", x= "Log GDP per capita") +
      geom_abline(intercept =  -1.1414 , slope =  0.7145) + guides (size = FALSE)})
  
  
  
  output$regression_combined<-renderPlot({
    data_test<-total %>% filter(Year==input$year_)
    
    ggplot(data=total %>% filter(Year==input$year_),
           aes(x=unlist(data_test[input$user_variable]), 
               y=unlist(data_test[input$user_variable2]),colour = Region, size = Population))+
      geom_point() +
      labs (y= input$user_variable2, x= input$user_variable) +
      geom_smooth( inherit.aes = FALSE,
                   aes(x=unlist(data_test[input$user_variable]),
                       y=unlist(data_test[input$user_variable2])), method = 'lm', se = FALSE)+
      guides (size = FALSE)})
  
  output$bar_chart_race<- renderImage({
    # A temp file to save the output, removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    # now make the animation
    
    happiness_ranked <- total %>%
      group_by(Year) %>%
      # The * 1 makes it possible to have non-integer ranks while sliding
      mutate(rank = rank(-`Life Ladder`),
             Value_rel = `Life Ladder`/`Life Ladder`[rank==1],
             Value_lbl = paste0(" ",round(`Life Ladder`,2))) %>%
      group_by(Country_name) %>%
      filter(rank <=10) %>%
      ungroup()
    
    
    staticplot = ggplot(happiness_ranked, aes(rank, group = Country_name,
                                              fill = as.factor(Country_name), color = as.factor(Country_name))) +
      geom_tile(aes(y = `Life Ladder`/2,
                    height = `Life Ladder`,
                    width = 0.9), alpha = 0.8, color = "black") +
      geom_text(aes(y = `Life Ladder`/2, label = paste(Country_name, " ")), size=6, vjust = 0.2, hjust = 1, colour = "black") +
      geom_text(aes(y=`Life Ladder`,label = Value_lbl, hjust=0), size=6,  colour = "black")  +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_line( size=.1, color="grey" ),
            panel.grid.minor.x = element_line( size=.1, color="grey" ),
            plot.title=element_text(size=20, hjust=0.5, face="bold", colour="black", vjust=-1),
            plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="grey"),
            plot.caption =element_text(size=14, hjust=0.5, face="italic", color="grey"),
            plot.background=element_blank(),
            plot.margin = margin(3, 3, 3, 4, "cm"))
    
    
    anim = staticplot + transition_states(Year, transition_length = 4, state_length = 1) +
      view_follow(fixed_x = TRUE)  +
      labs(title = 'Happiness Index per Year : {closest_state}',  
           subtitle  =  "Top 10 Countries",
           caption  = "Happiness Index per Year | Data Source: Happiness World Report 2019")
    
    anim_save("outfile.gif", animate(anim)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif')}, deleteFile = FALSE)

  
  

})


