pacman::p_load(spdep, sfdep, tmap, sf, ClustGeo, ggpubr, cluster, factoextra, NbClust, heatmaply, corrplot, psych, tidyverse, GGally, shiny, shinythemes, gt)
pop_data<- read_rds("data/rds/pop_data.rds")
crime_district <- read_rds("data/rds/crime_district.rds")
adm2_sf <- read_rds("data/rds/adm2_sf.rds")
rate_crime_district<-read_rds("data/rds/rate_crime_district.rds")
crime_boundary<-read_rds("data/rds/crime_boundary.rds")
crime_boundary_west<- crime_boundary %>% 
  ungroup() %>% 
  filter(region == "Peninsular")
crime_boundary_west <- st_as_sf(crime_boundary_west)
rate_crime_district_bound <- read_rds("data/rds/rate_crime_district_bounds.rds")

#========================#
###### Shiny UI ######
#========================#

ui <- navbarPage(
  title =  "CrimeRojak",
  fluid = TRUE,
  theme = shinytheme("united"),
  id = "navbarID",
  tabPanel("Home"),
  tabPanel("Exploratory Data Analysis",
           
           
           sidebarLayout(
             
             
             sidebarPanel(
               
               
               selectInput(inputId = "type",
                           label = "Crime-type",
                           choices = list("Causing Injury" = "causing_injury",
                                          "Murder" = "murder",
                                          "Rape" = "rape",
                                          "Armed Gang Robber" = "robbery_gang_armed",
                                          "Unarmed Gang Robbery" = "robbery_gang_unarmed",
                                          "Armed Solo Robbery" = "robbery_solo_armed",
                                          "Unarmed Solo Robbery" = "robbery_solo_unarmed"),
                           selected = "causing_injury"),
               
               
               selectInput(inputId = "classification",
                           label = "Classification Method",
                           choices = list("sd" = "sd", 
                                          "Equal" = "equal", 
                                          "Pretty" = "pretty", 
                                          "Quantile" = "quantile", 
                                          "K-means" = "kmeans", 
                                          "Hclust" = "hclust", 
                                          "Bclust" = "bclust", 
                                          "Fisher" = "fisher", 
                                          "Jenks" = "jenks"),
                           selected = "pretty"),
               
               
               sliderInput(inputId = "classes",
                           label = "Number of classes",
                           min = 5,
                           max = 10,
                           value = c(6)),
               
               
               selectInput(inputId = "colour",
                           label = "Colour scheme:",
                           choices = list("Blues" = "Blues", 
                                          "Reds" = "Reds", 
                                          "Greens" = "Greens",
                                          "Yellow-Orange-Red" = "YlOrRd",
                                          "Yellow-Orange-Brown" = "YlOrBr",
                                          "Yellow-Green" = "YlGn",
                                          "Orange-Red" = "OrRd",
                                          "Purples" = "Purples"),
                           selected = "Purples"),
               
               
               sliderInput(inputId = "opacity",
                           label = "Level of transparency",
                           min = 0,
                           max = 1,
                           value = c(0.5))),
             
             
             mainPanel(
               tmapOutput("edaPlot",
                          width = "100%",
                          height = 580))
             
             )),
  
  
  navbarMenu("Exploratory Spatial Data Analysis",
             tabPanel("Global Measures",
                      
                      sidebarLayout(
                        
                        
                        sidebarPanel(
                          
                          
                          selectInput(inputId = "year1",
                                      label = "Year",
                                      choices = list("2020" = "2020",
                                                     "2021" = "2021",
                                                     "2022" = "2022",
                                                     "2023" = "2023"),
                                      selected = "2020"),
                          
                          
                          selectInput(inputId = "type2",
                                      label = "Crime-type",
                                      choices = list("Causing Injury" = "causing_injury",
                                                     "Murder" = "murder",
                                                     "Rape" = "rape",
                                                     "Armed Gang Robber" = "robbery_gang_armed",
                                                     "Unarmed Gang Robbery" = "robbery_gang_unarmed",
                                                     "Armed Solo Robbery" = "robbery_solo_armed",
                                                     "Unarmed Solo Robbery" = "robbery_solo_unarmed"),
                                      selected = "causing_injury"),
                          
                          radioButtons(inputId = "Contiguity1",
                                       label = "Contiguity Method",
                                       choices = c("Queen" = TRUE, 
                                                   "Rook" = FALSE),
                                       selected = "TRUE",
                                       inline = TRUE),
                          
                          
                          selectInput("MoranWeights","Spatial Weights Style",
                                      choices = c("W: Row standardised" = "W",
                                                  "B: Binary" = "B",
                                                  "C: Globally standardised" = "C",
                                                  "U: C / no of neighbours" = "U",
                                                  "minmax" = "minmax",
                                                  "S: Variance" = "S"),
                                      selected = "W"),
                          
<<<<<<< HEAD
                          radioButtons(inputId = "GMoranConfi",
                                       label = "Select Confidence level",
                                       choices = c("0.95" = 0.05, #use the alpha level in the back-end since that's what we use during calculations
                                                   "0.99" = 0.01,
                                                   "0.90" = 0.1),
                                       selected = 0.05,
                                       inline = TRUE),
                          
=======
>>>>>>> 28367036551d87f0dea82b4763df3bca029032f4
                        
                          
                          
                          actionButton("GMoranTable", "Generate Statistics"),
                          hr()
                          
                        ),
                        
                        
                        mainPanel(
                          gt_output("GMoranResult")
                        )
                      )),
             
             
             tabPanel("Local Measures",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(inputId = "year2",
                                      label = "Year",
                                      choices = list("2020" = "2020",
                                                     "2021" = "2021",
                                                     "2022" = "2022",
                                                     "2023" = "2023"),
                                      selected = "2020"),
                          
                          
                          selectInput(inputId = "type3",
                                      label = "Crime Type",
                                      choices = list("Causing Injury" = "causing_injury",
                                                     "Murder" = "murder",
                                                     "Rape" = "rape",
                                                     "Armed Gang Robber" = "robbery_gang_armed",
                                                     "Unarmed Gang Robbery" = "robbery_gang_unarmed",
                                                     "Armed Solo Robbery" = "robbery_solo_armed",
                                                     "Unarmed Solo Robbery" = "robbery_solo_unarmed"),
                                      selected = "Causing Injury"),
                          
                          
                          radioButtons(inputId = "Contiguity2",
                                       label = "Contiguity Method",
                                       choices = c("Queen" = TRUE, 
                                                   "Rook" = FALSE),
                                       selected = "TRUE",
                                       inline = TRUE),
                          
                          
                          selectInput("MoranWeights1", "Spatial Weights Style",
                                      choices = c("W: Row standardised" = "W",
                                                  "B: Binary" = "B",
                                                  "C: Globally standardised" = "C",
                                                  "U: C / no of neighbours" = "U",
                                                  "minmax" = "minmax",
                                                  "S: Variance" = "S"),
                                      selected = "W"),
                          
                          
                          sliderInput(inputId = "MoranSims", 
                                      label = "Number of Simulations:", 
                                      min = 99, max = 499,
                                      value = 99, step = 100),
                          
                          
                          actionButton("MoranUpdate", "Update Plot"),
                          hr(),
                          
                          
                          radioButtons(inputId = "MoranConf1",
                                       label = "Select Confidence level",
                                       choices = c("0.95" = 0.05, #use the alpha level in the back-end since that's what we use during calculations
                                                   "0.99" = 0.01,
                                                   "0.90" = 0.1),
                                       selected = 0.05,
                                       inline = TRUE),
                          
                          
                          selectInput("LisaClass", "Select Lisa Classification",
                                      choices = c("mean" = "mean",
                                                  "median" = "median",
                                                  "pysal" = "pysal"),
                                      selected = "mean"),
                          
                          
                          selectInput("localmoranstats", "Select Local Moran's Stat:",
                                      choices = c("local moran" = "local moran(ii)",
                                                  "expectation" = "expectation (eii)",
                                                  "variance" = "variance(var_ii)",
                                                  "std deviation" = "std deviation(z_ii)",
                                                  "P-value" = "p_value"),
                                      selected = "local moran")
                        ),
                        
                        
                        mainPanel(
                          fluidRow(
                            column(6, tmapOutput("LocalMoranMap")),
                            column(6, tmapOutput("LISA"))
                          )
                        )
                        
                        
                      )
             )),
  
  
  navbarMenu("Clustering Analysis",
             
             tabPanel("Hierarchical Clustering",
                      
                      sidebarLayout(
                        
                        
                        sidebarPanel(
                          
                          
                          selectInput(inputId = "year3",
                                      label = "Year",
                                      choices = list("2020" = "2020",
                                                     "2021" = "2021",
                                                     "2022" = "2022",
                                                     "2023" = "2023"),
                                      selected = "2020"),
                          
                        
                          
                          selectInput(inputId = "proxMethod",
                                      label = "Proximity Method",
                                      choices = list("Euclidean" = "euclidean",
                                                     "Maximum" = "maximum",
                                                     "Manhattan" = "manhattan",
                                                     "Canberra" = "canberra",
                                                     "Binary" = "binary",
                                                     "Minkowski" = "minkowski"),
                                      selected = "euclidean"),
                          
                          selectInput(inputId = "hclustMethod",
                                      label = "Hclust Method",
                                      choices = list(
                                        "Ward's Minimum Variance (ward.D)" = "ward.D",
                                        "Ward's Minimum Variance (ward.D2)" = "ward.D2",
                                        "Single Linkage (single)" = "single",
                                        "Complete Linkage (complete)" = "complete",
                                        "Average Linkage (average)" = "average",
                                        "McQuitty's Method (mcquitty)" = "mcquitty",
                                        "Median Linkage (median)" = "median",
                                        "Centroid Linkage (centroid)" = "centroid"
                                      ),
                                      selected = "ward.D"),
                          
                          sliderInput(inputId = "optimalClust", 
                                      label = "Number of Clusters", 
                                      min = 3, max = 15,
                                      value = 6, step = 1),
                        
                          
                          actionButton("Hclust", "Generate Dendrogram"),
                          hr(),
                          
                          actionButton("HeatMap", "Generate Heatmap"),
                          hr()
                          
                        ),
                        
                        
                        mainPanel(
                          
                          fluidRow(
                            column(12, plotOutput("HclustPlot")),
                            column(12, plotlyOutput("Heatmaply")))
                        )
    
                        
                      )),
  
             tabPanel("ClustGeo"),
             tabPanel("SKATER")))




#========================#
###### Shiny Server ######
#========================# 

server <- function(input, output){
  
  output$edaPlot <-renderTmap({
    tmap_options(check.and.fix = TRUE) 
    crime_data<- crime_boundary_west %>%  filter(type == input$type)
      tm_shape(crime_data)+
      tm_polygons("crimes",
              n = input$classes,
              style = input$classification,
              palette = input$colour,
              alpha = input$opacity) +
      tm_borders(lwd = 0.1,  alpha = 1) +
      tm_view(set.zoom.limits = c(6.5, 8)
      )})
  
  
  
  #========================#
  ###### Global Moran I ######
  #========================# 
    
    moran_results <- eventReactive(input$GMoranTable, {
      
      print("Button clicked, generating Moran's I results...")
      
      crime_data <- crime_boundary_west %>%
        filter((year(ymd(date.x))) == input$year1 & type == input$type2) 
      
      nb <- crime_data %>% 
        st_contiguity(crime_data$geometry,queen =input$Contiguity1)
      
      nb[17]<- as.integer(19) #handle case for langkawi, which is not connected to the others by admin boundary
      
      crime_data_wm <- crime_data %>% 
        mutate(nb = nb, .before = 1) %>% 
        mutate(wt = st_weights(nb, style = input$MoranWeights), 
               .before = 1)
      
      global_moran <- global_moran_test(crime_data_wm$crimes, 
                                        crime_data_wm$nb,
                                        crime_data_wm$wt)
      
      data.frame(
        Statistic = "Global Moran's I",
        Moran_I = global_moran$estimate[1],        
        Expectation = global_moran$estimate[2],    
        Variance = global_moran$estimate[3],       
        P_value = global_moran$p.value                   
      )
      
    })

    output$GMoranResult <- render_gt({
      
      req(moran_results())
      
      source_note <- "<center>Results generated using global_moran_test() function</center>"
      
     
      if (moran_results()$P_value < input$GMoranConfi) {
        source_note <- paste(
          "\n\n\n\n", 
          "<center>P-value is less than the significance level, indicating sufficient statistical evidence to reject the null hypothesis.</center>",
          "\n\n",
          "<center>There is presence of clustering among the spatial units.</center>",
          "\n\n"
        )
        
        if (moran_results()$Moran_I > 0) {
          source_note <- paste(
            source_note,
            "\n\n",
            "<center>Moran's I statistic is also positive, confirming clustering among the spatial units.</center>",
            "\n\n"
          )
        }
        
      } else if (moran_results()$P_value > input$GMoranConfi) {
        source_note <- paste(
          "\n\n\n\n",
          "<center>P-value is higher than the significance level, indicating insufficient statistical evidence to reject the null hypothesis.</center>",
          "\n\n",
          "<center>We conclude there is no clustering among the spatial units.</center>",
          "\n\n"
        )
        
        if (moran_results()$Moran_I < 0) {
          source_note <- paste(
            source_note,
            "\n\n",
            "<center>The negative Moran's I statistic suggests dispersion rather than clustering among the spatial units.</center>",
            "\n\n"
          )
        }
      }
      
      # Render the gt table
      moran_results() %>%
        gt() %>%
  
        tab_header(
          title = "Global Moran's I Test Results",
          subtitle = "Spatial Autocorrelation Analysis"
        ) %>%
        
        fmt_number(
          columns = c(Moran_I, Expectation, Variance, P_value),
          decimals = 4
        ) %>%
  
        cols_label(
          Statistic = "Statistic",
          Moran_I = "Moran's I",
          Expectation = "Expected Value",
          Variance = "Variance",
          P_value = "P-value"
        ) %>%
        
        
        # Pass the centered and spaced source note
        tab_source_note(
          md(source_note) # `md()` treats text as Markdown to render HTML
        )
        
       
      
        })

    
    
    #==========================================================
    # Local Measures of Spatial AutoCorrelation
    #==========================================================   
    
    localMIResults <- eventReactive(input$MoranUpdate,{
      
      crime_data <- crime_boundary_west %>% 
        filter((year(ymd(date.x))) == input$year2 & type == input$type3) 
      
      nb<- crime_data %>% 
        st_contiguity(crime_data$geometry, queen = input$Contiguity2)
      
      nb[17]<- as.integer(19) #handle case for langkawi, which is not connected to the others by admin boundary
      
      crime_data_wm <- crime_data %>% 
        mutate(nb = nb, .before = 1) %>% 
        mutate(wt = st_weights(nb, style = input$MoranWeights1), 
               .before = 1) %>% 
        select(1,2)
    
      
      # Computing Local Moran's I
      lisa <- crime_data %>%
        mutate(local_moran = local_moran(crime_data$crimes, crime_data_wm$nb, crime_data_wm$wt, nsim = input$MoranSims), .before = 1) %>%
        unnest(local_moran)
      
      lisa <- lisa %>%
        rename("local moran(ii)" = "ii", "expectation (eii)" = "eii",
               "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
               "p_value" = "p_ii")
      
      return(lisa) 
      
    })
    
    output$LocalMoranMap <- renderTmap({
      df <- localMIResults()
      
      if(is.null(df) || nrow(df) == 0) return()  # Exit if no data
      
      # Map creation using tmap
      localMI_map <- tmap_mode("plot") +
        tm_shape(df) + 
        tm_fill(input$localmoranstats) +
        tm_borders(alpha = 0.5) + 
        tm_view(set.zoom.limits = c(6,8)) +
        tm_layout(main.title = input$year2,
                  main.title.size= 1)
      
    })
    
    
    #Render LISA map 
    output$LISA <- renderTmap({
      df <- localMIResults()
      if(is.null(df)) return()
      
      
      lisa_sig <- df  %>%
        filter(p_value < as.numeric(input$MoranConf1))  
      
      lisamap <- tmap_mode("plot")
      
      tm_shape(df) + 
        tm_polygons() +
        tm_borders(alpha = 0.5) +
        
        tm_shape(lisa_sig) +
        tm_fill(col = input$LisaClass,
                palette = "-RdBu") +
        tm_borders(alpha = 0.4)
      
    })
    
    


#========================#
##### Render Hclust ######
#========================#

dendogram <- eventReactive(input$Hclust,{
  
  ci <- rate_crime_district_bound %>% 
    filter(type == "causing_injury") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`causing_injury`= `crime_rate`)
  
  mr <- rate_crime_district_bound %>% 
    filter(type == "murder") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`murder`= `crime_rate`)
  
  rp <- rate_crime_district_bound %>% 
    filter(type == "rape") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`rape`= `crime_rate`)
  
  rga <- rate_crime_district_bound %>% 
    filter(type == "robbery_gang_armed") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`robbery_gang_armed`= `crime_rate`)
  
  rgu <- rate_crime_district_bound %>% 
    filter(type == "robbery_gang_unarmed") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`robbery_gang_unarmed`= `crime_rate`)
  
  rsa <- rate_crime_district_bound %>% 
    filter(type == "robbery_solo_armed") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`robbery_solo_armed`= `crime_rate`)
  
  rsu <- rate_crime_district_bound %>% 
    filter(type == "robbery_solo_unarmed") %>% 
    select(-c(3, 5:7, 11)) %>% 
    rename(`robbery_solo_unarmed`= `crime_rate`)
  
  
  rate_crime_prep <- ci%>% 
    mutate(murder = mr$murder, .before = 5) %>% 
    mutate(rape = rp$rape, .before = 6) %>% 
    mutate(robbery_gang_armed = rga$robbery_gang_armed, .before = 7) %>% 
    mutate(robbery_gang_unarmed = rgu$robbery_gang_unarmed, .before = 8) %>% 
    mutate(robbery_solo_armed = rsa$robbery_solo_armed, .before = 9) %>% 
    mutate(robbery_solo_unarmed = rsu$robbery_solo_unarmed, .before = 10)
  
  
  rate_crime_data <- rate_crime_prep %>% 
    filter(year(ymd(date.x)) == input$year3)
  
  rate_crime_data <- as.data.frame(rate_crime_data)
  row.names(rate_crime_data) <- rate_crime_data$district 
  
  rate_crime_data<- rate_crime_data %>% 
    select(-c(2))
  
  rate_crime_data.std <- normalize(rate_crime_data)
  rate_crime_data.std <- rate_crime_data.std %>%  drop_na()
  
  rate_crime_data_numeric <- rate_crime_data %>% select(where(is.numeric))
  rate_crime_data_numeric <- rate_crime_data_numeric %>% drop_na() 
  proxmat <- dist(rate_crime_data_numeric, method = input$proxMethod)
  
  return(proxmat)
  
})
    
    
  output$HclustPlot <- renderPlot({
    
    prox <- dendogram()
    
    hclust_ward <- hclust(prox, method = input$hclustMethod)
    plot(hclust_ward, cex = 0.7)
    rect.hclust(hclust_ward, 
                k = input$optimalClust, 
                border = 2:5)
    
  })
  
  
  heatmap<-eventReactive(input$HeatMap,{
    
    ci <- rate_crime_district_bound %>% 
      filter(type == "causing_injury") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`causing_injury`= `crime_rate`)
    
    mr <- rate_crime_district_bound %>% 
      filter(type == "murder") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`murder`= `crime_rate`)
    
    rp <- rate_crime_district_bound %>% 
      filter(type == "rape") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`rape`= `crime_rate`)
    
    rga <- rate_crime_district_bound %>% 
      filter(type == "robbery_gang_armed") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`robbery_gang_armed`= `crime_rate`)
    
    rgu <- rate_crime_district_bound %>% 
      filter(type == "robbery_gang_unarmed") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`robbery_gang_unarmed`= `crime_rate`)
    
    rsa <- rate_crime_district_bound %>% 
      filter(type == "robbery_solo_armed") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`robbery_solo_armed`= `crime_rate`)
    
    rsu <- rate_crime_district_bound %>% 
      filter(type == "robbery_solo_unarmed") %>% 
      select(-c(3, 5:7, 11)) %>% 
      rename(`robbery_solo_unarmed`= `crime_rate`)
    
    
    rate_crime_prep <- ci%>% 
      mutate(murder = mr$murder, .before = 5) %>% 
      mutate(rape = rp$rape, .before = 6) %>% 
      mutate(robbery_gang_armed = rga$robbery_gang_armed, .before = 7) %>% 
      mutate(robbery_gang_unarmed = rgu$robbery_gang_unarmed, .before = 8) %>% 
      mutate(robbery_solo_armed = rsa$robbery_solo_armed, .before = 9) %>% 
      mutate(robbery_solo_unarmed = rsu$robbery_solo_unarmed, .before = 10)
    
    
    rate_crime_data <- rate_crime_prep %>% 
      filter(year(ymd(date.x)) == input$year3)
    
    rate_crime_data <- as.data.frame(rate_crime_data)
    row.names(rate_crime_data) <- rate_crime_data$district 
    
    rate_crime_data<- rate_crime_data %>% 
      select(-c(2))
    
    rate_crime_data.std <- normalize(rate_crime_data)
    rate_crime_data.std <- rate_crime_data.std %>%  drop_na()
   
    
    return(rate_crime_data.std)
    
  })
  
  output$Heatmaply<- renderPlotly({
    
    data <- heatmap()
    heatmaply(data,
              Colv=NA,
              dist_method = input$proxMethod,
              hclust_method = input$hclustMethod,
              seriate = "OLO",
              colors = Purples,
              k_row = input$optimalClust,
              margins = c(NA,200,60,NA),
              fontsize_row = 4,
              fontsize_col = 5,
              main="Geographic Segmentation of Malaysia by Crime Type",
              xlab = "Crime Type",
              ylab = "Districts"
              
    )
  })
  
                           
                           
    }
#========================#
##### Render output ######
#========================#

shinyApp(ui= ui, server = server)