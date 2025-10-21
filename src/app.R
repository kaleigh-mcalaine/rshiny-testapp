library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly) # for interactive plot
library(vipor) # for interactive vipor plot
library(shinyWidgets) # for switchInput (views)
library(DT) # for bottom table



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
    

  #write_csv(dat_long_all, "S:/EOME/Weisskopf/Saint_Louis_Baby_Teeth_Study/Data 2020/Webpage/YourData/test_deploy/dat_long_all.csv", na="")
# anchor    
  # (1.2) calc count for graphic ----
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
    # (1.3.1) fun to compute the x-pos in VIPOR plot ----
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
    
  # (1.4) Metal Specific Text ----
  metal_info <- list(
    "Lead (Pb)" = HTML(
      "<strong>How Lead could get into your body:</strong> Lead used to be in gasoline, paint, and water pipes. People could be exposed by breathing air with lead dust, drinking water from lead pipes, or touching/ingesting contaminated soil or paint chips.<br><br>
  <strong>How Lead could affect your body:</strong> Lead can affect how the brain and nervous system develop, especially in early childhood. Very high levels could cause learning and behavior changes, but the low levels seen in population studies are less well understood. There is no known safe level of lead."),
    "Lithium (Li)" = HTML(
      "<strong>How Lithium could get into your body:</strong> Lithium is naturally present in rocks and groundwater. Some drinking water supplies and certain foods contain small amounts.<br><br>
  <strong>How Lithium could affect your body:</strong> Lithium is sometimes used in some medications at much higher doses which can help with mood regulation. The very small amounts that occur naturally in the environment are not known to cause harm, and scientists are still studying possible effects."),
    "Manganese (Mn)" = HTML(
      "<strong>How Manganese could get into your body:</strong> Manganese is an essential nutrient found in foods like nuts, grains, and leafy greens. It can also enter drinking water from natural rock sources or older pipes.<br><br>
  <strong>How Manganese could affect your body:</strong> Our bodies need manganese for normal brain and bone health. Extremely high exposures (far above typical diet or drinking water) may affect movement and nervous system function, but the levels measured in baby teeth usually reflect everyday exposures."),
    "Zinc (Zn)" = HTML(
      "<strong>How Zinc could get into your body:</strong> Zinc is an essential nutrient found in meat, beans, nuts, and whole grains. Some water supplies and dental materials can also add trace amounts of Zinc.<br><br>
  <strong>How Zinc could affect your body:</strong> Zinc supports immune function and growth. It’s generally safe at everyday exposures. However, too much zinc might cause stomach upset or affect your uptake of another essential element — Copper — but environmental exposures are rarely high enough to cause concern."))
  

# (2) UI ----
  ui <- fluidPage(
    
  # (2.0) css ----
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Libre+Baskerville:wght@400;700&family=Lato:wght@300;400;700&display=swap"
      ),
      tags$style(HTML("
      .main-title {
        font-family: 'Libre Baskerville', Georgia, serif;
        font-weight: 500;
        letter-spacing: 0.2px;
        font-size: 40px;
        color: #ABF4C4;
      }
      .subtitle {
        font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-weight: 400;
        font-size: 20px;
        color: white;
      }
      .intro-text {
        font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-weight: 300;
        font-size: 16px;
        color: white;
      }
    "))
    ),
    
  # (2.1) title & intro ----
    div(
      #style = "text-align:center; max-width:800px; margin:0 auto; padding-top:10px; padding-bottom:15px;",
      style="background-color:#113743; color:white; padding:25px 20px; text-align:center;",
      
      # main title
      h2("St. Louis Baby Tooth Study", class = "main-title", style = "color:#abf4c4; margin-bottom:5px;"),
      
      # Subtitle
      h3("Your Tooth Metal Levels", class = "subtitle", style = "color:white; margin-top:0;"),
      
      # intro text
      p("See how your baby tooth metal levels compare to other participants in our study. 
        Enter your Study ID below to view your personal data (shown as a blue triangle).",
        class = "intro-text", style = "max-width:650px; margin:10px auto 0 auto;")
    ),
    br(),
  
  # (2.2) user inputs ----
    fluidRow(
      column(
        width = 6, offset = 0,
        div(
          class = "text-center",                                                # bootstrap class to center
          style = "max-width:300px; margin:0 auto;",                            # prevent stretching too wide
          textInput("study_id", "Enter Your Study ID (e.g. 123456): ", ""),
          helpText("Note: Only participants with usable tooth samples will appear in the chart.")
        )
      ),
      column(
        width = 6,
        div(
          class = "text-center",
          style = "max-width:300px; margin:0 auto;",
          selectInput(
            inputId = "metal",
            label   = "Choose a metal to view:",
            choices = levels(dat_long_all$metal_fac),
            selected = "Lead (Pb)"
          )
        )
      )
    ),
    uiOutput("validationText"),
    
  # (2.3) Pre-plot Text ----
    # (2.3.1) research v clinical ----
      div(
        style = "background-color:#e9f5fb; border-left: 6px solid #113743; 
               padding:10px 15px; margin-top:15px; margin-bottom:20px; border-radius:4px;",
        strong("Important: Research Results Only"), 
        br(),
        "These numbers come from research laboratories and are not medical tests. They cannot be used to diagnose or guide treatment. If you have health concerns, talk with your healthcare provider."),
  
    
    # (2.3.2) metal specific text ----
      uiOutput("metalText"),
      br(),
  
  # (2.4) plots & table ----
    # (2.4.1) toggle for y axis ----
    radioGroupButtons(
      inputId = "view_range",
      choices = c("Zoom In", "Zoom Out"),
      selected = "Zoom In",
      justified = TRUE),
      
    # (2.4.2) primary plot ----
    plotlyOutput("sinaPlot", height = "500px"),
    br(),
  
    # (2.4.3) conditional data note ----
    uiOutput("conditionalNote"),
  
    # (2.4.4) missing data note ---
    uiOutput("missingDataNote"),
    br(),
    
    # (2.4.5) summary table ----
    DTOutput("participantTable"),
  
  # (2.5) post-plot text ----
    # (2.5.1) tooth measurement info ----
    tags$details(
      style = "margin-top:20px; border:1px solid #ddd; border-radius:5px; padding:10px; background-color:#f9f9f9;",
      tags$summary("➤   About how we tested your baby teeth", style = "cursor: pointer; font-weight: bold; color:#113743;"),
      tags$ul(
        tags$li("Baby teeth grow in layers, a bit like tree rings, and record what a child was exposed to before and after birth."),
        tags$li("Our lab cut each tooth in half and used a technique called laser ablation inductively coupled plasma mass spectrometry (LA-ICP-MS)."),
        tags$li("A tiny laser vaporizes bits of tooth material and the vapor goes into a mass spectrometer to estimate metal concentration."),
        tags$li("We usually see a special line called the neonatal line, which marks birth and lets us separate prenatal and postnatal exposure. Sometimes this line isn’t visible, so we may only have a whole-tooth average."))),
    br(),
  
    # (2.5.2) bottom of page text ----
    div(
      style = "
      background-color:#f9f9f9;
      border-top: 3px solid #113743;
      border-radius: 6px;
      padding: 20px;
      margin-top: 30px;
      text-align: center;
      font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif;
      max-width: 700px;
      margin-left: auto;
      margin-right: auto;
    ",
      
      # call to action
      tags$p(
        style = "font-size:17px; font-weight:600; color:#113743; margin-bottom:4px;",
        "Haven’t finished your surveys or tests?"
      ),
      
      # button
      tags$a(
        href = "https://hsph.me/slbt-surveys",
        "Click here to visit our Study Surveys page",
        style = "
      display:inline-block;
      background-color:#113743;
      color:white;
      font-weight:600;
      padding:10px 18px;
      border-radius:4px;
      text-decoration:none;
      margin:5px 0 10px 0;  /* top right bottom left */
    ",
        target = "_blank"
      ),
      
      # FAQ / Email
      tags$p(
        style = "margin-top:5px; font-size:15px; color:#333;",
        HTML("Questions? Check our <a href='https://hsph.me/slbt-faq' target='_blank' style='color:#007C89; font-weight:600; text-decoration:none;'>FAQ</a> or contact <a href='mailto:slbt@hsph.harvard.edu' style='color:#007C89; font-weight:600; text-decoration:none;'>slbt@hsph.harvard.edu</a>.")
      ),
      
      # thank you text
      tags$p(
        em("Thank you for your continued participation in the St. Louis Baby Tooth Study!"),
        style = "margin-top:10px; color:#444; font-size:15px;"
      )
    )
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
      "Lithium (Li)" = 1,
      "Zinc (Zn)" = 200,
      "Manganese (Mn)" = 2
    )
  
    # (3.1.2) plot ----
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
          marker = list(size = 6, color = "rgba(165,28,48,0.35)", line = list(width = 0)), # "rgba(165,28,48,0.5)"
          name = "Teeth Metal Concentrations",
          showlegend = TRUE
        )
        
      # (3.1.2c) add med. line for legend ----
        # calc medians per exposure period per selected metal
        med_combined <- median(
          df_sina$concentration[df_sina$exp_pd_fac == "Combined Average"],
          na.rm = TRUE
        )
         # add horizontal median line
        p <- add_trace(
          p,
          data = data.frame(x = c(0.3, 3.5), y = c(med_combined, med_combined)), 
          x = ~x, y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(color = "black", width = 2, dash = "dot"),
          name = "Median (Combined Avg)",
          hoverinfo = "skip",
          showlegend = TRUE,
          inherit = FALSE 
        )
      
      # (3.1.2d) add ptcpt data ----
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
            marker = list(color = "blue", size = 16, symbol = "triangle-up"),
            name = "Your tooth", # for legend
            showlegend = TRUE
          )
        }
      
      # (3.1.2e) axes, title & legend ----
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
          
          if (input$view_range == "Zoom Out") {
            # user chose Zoom Out
            yaxis_config <- list(
              title = paste(metal_selected, "concentration (ppm)"),
              autorange = TRUE
            )
          } else if (!is.na(max_obs) && max_obs > custom_max) {
            # auto expand if participant exceeds limit
            yaxis_config <- list(
              title = paste(metal_selected, "concentration (ppm)"),
              autorange = TRUE
            )
          } else {
            # Zoom in default
            yaxis_config <- list(
              title = paste(metal_selected, "concentration (ppm)"),
              range = c(min(min_obs, 0), custom_max),
              autorange = FALSE
            )
          }
          
        # lookup N for each level
          n_map <- setNames(cnt_sel$n, as.character(cnt_sel$exp_pd_fac))
          n_for_lvl <- ifelse(is.na(n_map[lvl]), 0, n_map[lvl])
        
        # layout, title, & legend
          p <- layout(
            p,
            title = paste("Distribution of", input$metal, "measures"),
            xaxis = list(
              title = "",
              tickvals = c(1, 2, 3),
              #ticktext = paste0(lvl, " (N = ", n_for_lvl, ")")
              ticktext = paste0(lvl, "<br><span style='font-size:11px; color:#555;'>N = ", n_for_lvl, "</span>"),
              automargin = TRUE
            ),
            yaxis = yaxis_config,
            dragmode = FALSE,
            showlegend = TRUE,
            legend = list(
              x = 0.02,
              y = 0.98,
              bgcolor = "rgba(255,255,255,0.5)",
              bordercolor = "black",
              borderwidth = 1)
          ) %>% config(displayModeBar = FALSE)
        
        p
    })
  
  # (3.2) table of vals ----
  output$participantTable <- renderDT({
    req(input$study_id, input$metal)
    
    id_input <- suppressWarnings(as.numeric(input$study_id))
    metal_input <- input$metal
    
    # participant data
    df_ptcpt <- dat_long_all %>%
      filter(study_id == id_input, metal_fac == metal_input) %>%
      select(exp_pd_fac, your_conc = concentration)
    
    # study median data
    df_study <- dat_long_all %>%
      filter(metal_fac == metal_input) %>%
      group_by(exp_pd_fac) %>%
      summarise(med_conc = round(median(concentration, na.rm = TRUE), 2), .groups = "drop")
    
    # merge and clean
    df_merged <- full_join(df_ptcpt, df_study, by = "exp_pd_fac") %>%
      mutate(
        `Exposure Period` = exp_pd_fac,
        `Your Concentration (ppm)` = round(your_conc, 2),
        `Study Median Concentration (ppm)` = med_conc
      ) %>%
      select(`Exposure Period`, `Your Concentration (ppm)`, `Study Median Concentration (ppm)`) %>%
      arrange(factor(`Exposure Period`, levels = c("Combined Average", "Prenatal Average", "Postnatal Average")))
    
    datatable(
      df_merged,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        responsive = TRUE, # for mobile version
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = "cell-border stripe hover",
      rownames = FALSE,
      width = "100%"
    )
  })
  
  # (3.3) conditional messages ----
    # (3.3.1) if study ID not found ----
      output$validationText <- renderUI({
        req(input$study_id)
        
        id_input <- suppressWarnings(as.numeric(input$study_id))
        if (is.na(id_input) || !(id_input %in% dat_long_all$study_id)) {
          div(style = "color:red; font-weight: bold;",
              "Sorry, we haven’t analyzed your baby tooth. Only about 20% of our participants were randomly selected to have their tooth metals analyzed.")
        }
      })
      
    # (3.3.2) if values look high ----
      output$conditionalNote <- renderUI({
        req(input$study_id, input$metal)
        id_input <- suppressWarnings(as.numeric(input$study_id))
        metal_input <- input$metal
        
        # participant data for selected metal
        df_pt <- dat_long_all %>%
          filter(study_id == id_input, metal_fac == metal_input)
        
        # show only if they have data
        if (nrow(df_pt) == 0) return(NULL)
        
        # high value check
        high_cutoff <- y_limits[[metal_input]]
        if (any(df_pt$concentration > high_cutoff, na.rm = TRUE)) {
          div(
            style = "background-color:#fff3cd; border-left:5px solid #856404; 
               padding:10px; border-radius:4px; margin-top:10px;",
            strong("Note: "), "Your values look higher compared with most other study participants so we are showing you the expanded view of the dataset. 
            This does not mean you will have health effects. These are research results only, and there is no known threshold for harm in baby teeth for any metal. People can have higher or lower numbers for many reasons, including environment, diet, how teeth develop, or because of our measurement processes."
          )
        }
      })
      
    # (3.3.3) missing data note ----
      output$missingDataNote <- renderUI({
        req(input$study_id, input$metal)
        
        id_input <- suppressWarnings(as.numeric(input$study_id))
        metal_input <- input$metal
        
        # skip if not analyzed at all (handled w validationText) ---
        if (!(id_input %in% dat_long_all$study_id)) return(NULL)
        
        # participant’s overall data (all metals) ---
        df_all_pt <- dat_long_all %>%
          filter(study_id == id_input)
        
        # participant’s data for selected metal ---
        df_metal_pt <- df_all_pt %>%
          filter(metal_fac == metal_input)
        
        exp_periods <- c("Combined Average", "Prenatal Average", "Postnatal Average")
        missing_periods <- setdiff(exp_periods, df_metal_pt$exp_pd_fac)
        
        # CASE 1: participant has no data for this metal ---
        if (nrow(df_metal_pt) == 0) {
          return(div(
            style = "background-color:#f8f9fa; border-left:5px solid #555858; 
               padding:10px; border-radius:4px; margin-top:10px; color:#2C2828;",
            tags$strong("Note: "),
            "We don’t have your data available for this metal. Some analyses could not be completed for technical reasons. Your other metal results are available by selecting other metals at the top of the screen."
          ))
        }
        
        # CASE 2: Participant has partial data (missing some exposure periods) for this metal ---
        if (length(missing_periods) > 0) {
          return(div(
            style = "background-color:#f8f9fa; border-left:5px solid #555858; 
               padding:10px; border-radius:4px; margin-top:10px; color:#2C2828;",
            tags$strong("Note: "),
            "We don’t have all values available for your tooth. This could be because we could not see a neonatal line in your tooth, your tooth was too degraded or we have not processed it yet."
          ))
        }
        
        # CASE 3: No missing data for this metal ---
        return(NULL)
      })
      
  # (3.4) metal specific text ----
    output$metalText <- renderUI({
      req(input$metal)
      tags$details(
        style = "margin-top:10px; margin-bottom:10px; border:1px solid #ddd; border-radius:5px; padding:10px; background-color:#f9f9f9;",
        tags$summary(
          paste0("➤ Learn more about ", input$metal, " and your body"),
          style = "cursor: pointer; font-weight: bold; color: #113743;"
        ),
        metal_info[[input$metal]]
      )
    })
}
  
shinyApp(ui = ui, server = server)


