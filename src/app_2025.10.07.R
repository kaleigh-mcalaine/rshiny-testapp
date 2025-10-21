library(shiny)
library(readr)
library(ggplot2)
#library(ggforce) # for sina plot
library(dplyr)
library(tidyr)
library(plotly) # for interactive plot
library(vipor) # for interactive vipor plot


# (0) Load data ----
# DF_ALL <- read_csv("S:/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Tooth Analysis/ToothData B1_5 Clean/toothmetalaverages_080825.csv")
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
    
  # (1.4) Metal-specific text ----
  metal_info <- list(
    "Lead (Pb)" = HTML(
      "<h4>Lead (Pb)</h4>
  <strong>How Lead could get into your body:</strong> Lead used to be in gasoline, paint, and water pipes. People could be exposed by breathing air with lead dust, drinking water from lead pipes, or touching/ingesting contaminated soil or paint chips.<br><br>
  <strong>How Lead could affect your body:</strong> Lead can affect how the brain and nervous system develop, especially in early childhood. Very high levels could cause learning and behavior changes, but the low levels seen in population studies are less well understood. There is no known safe level of lead."),
    "Lithium (Li)" = HTML(
      "<h4>Lithium (Li)</h4>
  <strong>How Lithium could get into your body:</strong> Lithium is naturally present in rocks and groundwater. Some drinking water supplies and certain foods contain small amounts.<br><br>
  <strong>How Lithium could affect your body:</strong> Lithium is sometimes used in some medications at much higher doses which can help with mood regulation. The very small amounts that occur naturally in the environment are not known to cause harm, and scientists are still studying possible effects."),
    "Manganese (Mn)" = HTML(
      "<h4>Manganese (Mn)</h4>
  <strong>How Manganese could get into your body:</strong> Manganese is an essential nutrient found in foods like nuts, grains, and leafy greens. It can also enter drinking water from natural rock sources or older pipes.<br><br>
  <strong>How Manganese could affect your body:</strong> Our bodies need manganese for normal brain and bone health. Extremely high exposures (far above typical diet or drinking water) may affect movement and nervous system function, but the levels measured in baby teeth usually reflect everyday exposures."),
    "Zinc (Zn)" = HTML(
      "<h4>Zinc (Zn)</h4>
  <strong>How Zinc could get into your body:</strong> Zinc is an essential nutrient found in meat, beans, nuts, and whole grains. Some water supplies and dental materials can also add trace amounts of Zinc.<br><br>
  <strong>How Zinc could affect your body:</strong> Zinc supports immune function and growth. It’s generally safe at everyday exposures. However, too much zinc might cause stomach upset or affect your uptake of another essential element — Copper — but environmental exposures are rarely high enough to cause concern.")
  )
  
  

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
    
    # add metal specific text
    uiOutput("metalText"),
    br(),
    
    
    # plot
    plotlyOutput("sinaPlot", height = "500px"),
    br(),
    tableOutput("participantTable"),
    
    p(em("Thank you for participating in the Saint Louis Baby Tooth Study!"))
  )

# (3) Server ----
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
        "Lithium (Li)" = 1,
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
        
        # (3.1.2b) base plot + hover ----
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
            name = "Teeth Metal Concentrations",
            showlegend = TRUE
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
              name = "Your tooth",           # for legend
              showlegend = TRUE
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
          
          min_obs <- min(df_sina$concentration, na.rm = TRUE)
          
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
              range = c(min(min_obs, 0), custom_max),  # allows negative if needed, otherwise 0
              autorange = FALSE
            )
          }
          
          # lookup N for each level
          n_map <- setNames(cnt_sel$n, as.character(cnt_sel$exp_pd_fac))
          n_for_lvl <- ifelse(is.na(n_map[lvl]), 0, n_map[lvl])
          
          # layout & title
          p <- layout(
            p,
            title = paste("Distribution of", input$metal, "measures"),
            xaxis = list(
              title = "",
              tickvals = c(1, 2, 3),
              ticktext = paste0(lvl, " (N = ", n_for_lvl, ")")
            ),
            yaxis = yaxis_config,
            dragmode = FALSE,
            showlegend = TRUE,
            legend = list(
              x = 0.80,
              y = 0.98,
              bgcolor = "rgba(255,255,255,0.5)",
              bordercolor = "black",
              borderwidth = 1)
          ) %>% config(displayModeBar = FALSE)
          
          
        # (3.1.2e) add median line ----
          # calculate medians per exposure period for the selected metal
          med_combined <- median(
            df_sina$concentration[df_sina$exp_pd_fac == "Combined Average"],
            na.rm = TRUE
          )
          
          if (!is.na(med_combined)) {
            p <- layout(
              p,
              shapes = list(
                list(
                  type = "line",
                  xref = "paper",  # to make it full width
                  x0 = 0, x1 = 1,  
                  yref = "y",
                  y0 = med_combined, y1 = med_combined,
                  line = list(color = "black", width = 2, dash = "dot")
                )
              )
            )
          }
          
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
    
    # (3.4) Metal Specific Text ----
    output$metalText <- renderUI({
      req(input$metal)
      metal_info[[input$metal]]
    })
  }  
shinyApp(ui = ui, server = server)


