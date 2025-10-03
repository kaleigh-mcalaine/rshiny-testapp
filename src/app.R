library(shiny)
library(readr)
library(ggplot2)
#library(ggforce) # for sina plot
library(dplyr)
library(tidyr)
library(plotly) # for interactive plot
library(vipor) # for interactive vipor plot


# (0) Load data ----
#DF_ALL <- read_csv("S:/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/ToothData B1_5 Clean/toothmetalaverages_080825.csv")
DAT_LONG_ALL <- read_csv("C:/Users/kam6950/Documents/GitHub/rshiny-testapp/src/dat_long_all.csv") 

# (1) Limited retidy----

dat_long_all <- DAT_LONG_ALL %>%
  
  # refactorize
  mutate(
    exp_pd_fac = factor(exp_pd_fac,
                        levels = c("Combined Average",
                                   "Prenatal Average",
                                   "Postnatal Average")),
    metal_fac = factor(metal_fac,
                       levels = c("Lead (Pb)",
                                  "Zinc (Zn)",
                                  "Manganese (Mn)",
                                  "Lithium (Li)")))

# # (1) Tidy data ----
#   # (1.1) tidy metals data into long df
#     dat_long_all <- DF_ALL %>% 
#       
#       # rewrite study ids (@ TEMPORARY)
#       arrange(study_id) %>%
#       mutate(study_id = dense_rank(study_id) + 123455) %>%
#     
#       select(study_id, starts_with(c("pb_", "zn_", "mn_", "li_")), -c(contains("tri"))) %>%
#       pivot_longer(cols = contains("metal"), values_to = "concentration", names_to = "metal_metric") %>% 
#       
#       # clean exposure period
#       mutate(exp_pd_fac = factor(case_when(grepl("_pre123", metal_metric) ~ "Prenatal Average",
#                                               grepl("_post123", metal_metric) ~ "Postnatal Average",
#                                               TRUE ~ "Combined Average"), 
#       levels = c("Combined Average", "Prenatal Average", "Postnatal Average"))) %>%
#       
#       # clean metals
#       separate(metal_metric, into = c("metal", NA, NA, NA), sep="_", remove=FALSE) %>%
#       mutate(metal_fac = factor(case_when(metal == "pb" ~ "Lead (Pb)",
#                                              metal == "zn" ~ "Zinc (Zn)",
#                                              metal == "mn" ~ "Manganese (Mn)",
#                                              metal == "li" ~ "Lithium (Li)"),
#       levels = c("Lead (Pb)","Zinc (Zn)","Manganese (Mn)","Lithium (Li)"))
#       ) %>%
#       
#       # filter out "bad values"
#       filter(!is.na(concentration)) %>%
#       #filter(concentration > 0) %>%
#       filter(!(metal_fac == "Zinc (Zn)" & concentration >= 300)) %>%
#       filter(!(metal_fac == "Lithium (Li)" & concentration >= 15)) 
#     
    
  # write_csv(dat_long_all, "S:/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Webpage/YourData/test_deploy/dat_long_all.csv", na="")
      
  # (1.2) add count for graphic
    metal_count <- dat_long_all %>%
      filter(!is.na(concentration)) %>%
      group_by(exp_pd_fac, metal_fac) %>%
      summarise(
        n = n(),
        avg = mean(concentration),
        .groups = "drop"
      ) %>%
      mutate(label = paste0("N = ", n))
    
    # @ TEMP
    # df=df_sel_full
    # width = 0.45
    # nbins = 40
    # varwidth = TRUE
  
  # (1.3) Plot Prep ----
    # (1.3.1) summary stats ----
    
    
    # (1.3.2) fun to compute the x-pos in VIPOR plot ----
    compute_sina_positions <- function(df, width = 0.45, nbins = 40, varwidth = TRUE) {
      
      if (nrow(df) == 0) return(df %>% mutate(x_sina = numeric(0)))
      
      # numeric centers 1/2/3 for Combined/Prenatal/Postnatal
      x_center <- as.numeric(df$exp_pd_fac)
      
      # y = concentration, x = numeric category center
      off <- vipor::offsetX(
        y        = df$concentration,
        x        = x_center,
        width    = width,      # horizontal spread -  e.g. 0.35–0.60
        nbins    = nbins,      # density resolution - e.g. 30–60 
        varwidth = varwidth,   # wider where denser
        method   = "quasirandom"
      )
      
      return(df %>% mutate(
        x_sina = x_center + off,
        x_center = x_center))
    }
    
  

# (2) UI ----
  ui <- fluidPage(
    
    # title at the top
    titlePanel("Your Tooth Metal Levels"),
    
    # short description
    p("This interactive chart shows how your baby tooth metal levels compare to other participants in the study."),
    p("To view your tooth's values, please enter your Study ID below. Your data points will be shown on the graph in a blue triangle."),
    br(),
    

    textInput("study_id", "Enter Your Study ID (e.g. 123456): ", ""),
    helpText("Note: Only participants with usable tooth samples will appear in the chart."),
    uiOutput("validationText"),
    
    # add dropdown
    selectInput(
      inputId = "metal",
      label   = "Choose a metal to view:",
      choices = levels(dat_long_all$metal_fac),
      selected = "Lead (Pb)" # default
    ),
    
    plotlyOutput("sinaPlot", height = "500px"),
    br(),
    tableOutput("participantTable"),
    
    p(em("Thank you for participating in the Saint Louis Baby Tooth Study!"))
  )

# (3) Server ----
server <- function(input, output) {
  
  # (@TEMP - for testing)
  # input <- data.frame(
  #   study_id = 123456,
  #   metal = "Lead (Pb)",
  #   stringsAsFactors = FALSE
  # )


  # (3.1) sina plots ----
    # (3.1.1) y-axis limits per metal ----
    y_limits <- list(
      "Lead (Pb)" = 5,
      "Lithium (Li)" = 3,
      "Zinc (Zn)" = 200,
      "Manganese (Mn)" = 2
    )
  
    # (3.1.2) Plot ----
    output$sinaPlot <- renderPlotly({
      
      id_input <- suppressWarnings(as.numeric(input$study_id))
      
      # (3.1.2a) prep dfs ----
        # slice by selected metal
        df_sel_full <- dat_long_all %>% 
          filter(metal_fac == input$metal)
  
        cnt_sel <- metal_count %>% 
          filter(metal_fac == input$metal)
        
        # compute sina positions for plot
        df_sina <- compute_sina_positions(df_sel_full, width = 0.45, nbins = 40, varwidth = TRUE)
        
  
      # (3.1.2b) base plot + hover ---
        p <- plot_ly(
          df_sina,
          x = ~x_sina, y = ~concentration,
          type = "scatter", mode = "markers",
          
          # add hover details
          text = ~paste0(
            "Period: ", exp_pd_fac,
            "<br>", input$metal, ": ", signif(concentration, 3), " ppm"
          ),
          hoverinfo = "text",
          marker = list(size = 6, color = "rgba(165,28,48,0.4)", line = list(width = 0)),
          showlegend = FALSE
        )
      
      # (3.1.2c) add ptcpt data ----
        # highlight ptcpt at the SAME sina offset
        hi_sina <- if (!is.na(id_input) && id_input %in% df_sina$study_id) {
          df_sina %>% 
            filter(study_id == id_input)
        } else df_sina[0, ]
        
        if (nrow(hi_sina) > 0) {
          p <- add_trace(
            p,
            data = hi_sina,
            x = ~x_center, y = ~concentration,
            type = "scatter", mode = "markers",
            text = ~paste0(
              "<br>Period: ", exp_pd_fac,
              "<br>", input$metal, ": ", signif(concentration, 4), " ppm"
            ),
            hoverinfo = "text",
            marker = list(color = "blue", size = 12, symbol = "triangle-up"),
            showlegend = FALSE
          )
        }
      
      # (3.1.2d) axes & title ----
        # levels for x axis 
        lvl <- levels(df_sina$exp_pd_fac)
        
        # y axis filters
          # y-axis upper limit
          metal_selected <- input$metal
          custom_max <- y_limits[[metal_selected]]
          
          # if ptcpt data goes above the limit
          max_obs <- if (nrow(hi_sina) > 0) {
            max(hi_sina$concentration, na.rm = TRUE)
          } else {
            NA  # no ptcpt entered yet
          }
          
          # y axis config
          if (!is.na(max_obs) && max_obs > custom_max) {
            # if participant exists AND exceeds limit → autoscale
            yaxis_config <- list(
              title = paste(metal_selected, "concentration (ppm)"),
              autorange = TRUE
            )
          } else {
            # default: lock to tight range
            yaxis_config <- list(
              title = paste(metal_selected, "concentration (ppm)"),
              range = c(0, custom_max),
              autorange = FALSE
            )
          }
          
        # lookup N for each level
        n_map <- setNames(cnt_sel$n, as.character(cnt_sel$exp_pd_fac))
        n_for_lvl <- ifelse(is.na(n_map[lvl]), 0, n_map[lvl])
        
        p <- layout(
          p,
          title = paste("Distribution of", input$metal, "measures"),
          xaxis = list(
            title = "",
            tickvals = c(1, 2, 3),
            ticktext = paste0(lvl, " (N = ", n_for_lvl, ")")
          ),
          yaxis = yaxis_config,
          dragmode = FALSE
        ) %>% config(displayModeBar = FALSE)
        
        p
    })
  
  # (3.2) table of vals ----
  output$participantTable <- renderTable({
    
    # only show when study ID given
    req(input$study_id, input$metal)

    id_input <- suppressWarnings(as.numeric(input$study_id))
    metal_input <- input$metal

    # participant data
    df_ptcpt <- dat_long_all %>%
      filter(study_id == id_input, metal_fac == metal_input) %>%
      select(exp_pd_fac, your_conc = concentration)

    # study avg data
    df_study <- dat_long_all %>%
      filter(metal_fac == metal_input) %>%
      group_by(exp_pd_fac) %>%
      summarise(avg_conc = round(mean(concentration, na.rm = TRUE), 2), .groups = "drop")

    #  merge dfs
    df_merged <- full_join(df_ptcpt, df_study, by = "exp_pd_fac") %>%
      mutate(
        `Exposure Period` = exp_pd_fac,
        `Your Concentration` = round(your_conc, 2),
        `Study Average Concentration` = avg_conc
      ) %>%
      select(`Exposure Period`, `Your Concentration`, `Study Average Concentration`) %>%
      arrange(factor(`Exposure Period`, levels = c("Combined Average", "Prenatal Average", "Postnatal Average")))

    return(df_merged)
  })
  
  
  # (3.3) conditional message if ID not found ----
  output$validationText <- renderUI({
    req(input$study_id)
    
    id_input <- suppressWarnings(as.numeric(input$study_id))
    if (is.na(id_input) || !(id_input %in% dat_long_all$study_id)) {
      div(style = "color:red; font-weight: bold;",
          "Sorry, we haven’t analyzed your baby tooth. Only about 20% of our participants were randomly selected to have their tooth metals analyzed.")
    }
  })
}
  
shinyApp(ui = ui, server = server)


