# An app to plot the country graphs

###### Load libraries (need to declare them old school way, because shinyapps cannot detect dependencies)

library("dplyr")
library("reshape2")
library("ggplot2")
library("ggthemes")
library("countrycode")
library("readxl")
library("tidyr")
library("data.table")
library("writexl")
library("unikn")
library("ggtext")
library("svglite")
library("stringr")
library("directlabels")
library("fanplot")
#library("ggfan")
library("hrbrthemes")
library("glue")
library("readr")
library("shiny")
library("here")
library("shinyWidgets")
library("showtext")
library("clipr")

# Modes
mode_online <- 1

# Import all the necessary assets except data
here::i_am("app.R")
source(here("plot.R"))
peers_fname <- here("1_peers_params.xlsx")

# Import data
D <- importData(data_y_fname = "extdata_y.csv", data_q_fname = "extdata_q.csv", data_m_fname = "extdata_m.csv", 
                 data_d_fname = "extdata_d.csv", dict_fname = "dict.csv", path = here())

# Menu lists
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "structure_dynamic", "structure_country_comparison",
                 "structure_country_comparison_norm", "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", 
                 "bar_year_comparison", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic")

countries_peers <- as.list(D$extdata_y$country_id |> unique())
countries <- D$extdata_y$country |> unique()
names(countries_peers) <- countries

indicators_all <- D$dict |> select(indicator, indicator_code, source_frequency)
indicators_start <- as.list(indicators_all |> pull(indicator_code))
names(indicators_start) <- indicators_all |> pull(indicator)
indicator_groups <- c("", "GDP growth decomposition", "World shares", "BOP (Y)", "BOP (Q)",
                      "Trade balance (Y)","Trade balance (Q)", "IIP assets (Y)", "IIP liabilities (Y)",
                      "Exchange rates",
                      "Budget revenue definitions (Y)", "Budget revenue structure (Y)",
                      "Budget expenditure definitions (Y)","Budget expense structure (Y)", 
                      "Budget balance definitions (Y)", "Budget debt definitions (Y)",
                      "WGI components", "Population drivers", "Model indicative scores", 
                      "Macro scores", "Public finance scores", "External scores", "Institutional scores",
                      "Macroprudential measures (Y)", "Banks: capital (Q/Y)", "Banks: asset quality (Q/Y)", 
                      "Banks: earnings (Q/Y)", "Banks: liquidity (Q/Y)", "Banks: FC (Q/Y)",
                      "Global prices")
indicator_groups_content <- list("", 
                                 c("cons_role", "govcons_role", "gcfc_role", "netex_role", "other_role"),
                                 c("pop_shr", "gdp_ppp_shr", "ex_gs_shr"),
                                 c("ca_gdp", "bop_finacc_nrmns_gdp", "bop_res_mns_gdp", "bop_capacc_gdp", "bop_err_gdp"),
                                 c("ca_gdp_sm", "bop_finacc_nrmns_gdp_sm", "bop_res_mns_gdp_sm", "bop_capacc_gdp_sm", "bop_err_gdp_sm"),
                                 c("tb_g_gdp", "tb_s_gdp", "primainc_gdp", "secinc_gdp"),
                                 c("tb_g_gdp_sm", "tb_s_gdp_sm", "primainc_gdp_sm", "secinc_gdp_sm"),
                                 c("iip_a_di_gdp", "iip_a_pi_gdp", "iip_a_der_gdp", "iip_a_oth_gdp", "iip_a_res_gdp"),
                                 c("iip_l_di_gdp", "iip_l_pi_gdp", "iip_l_der_gdp", "iip_l_oth_gdp"),
                                 c("usdlc_av", "neer_av", "reer_av"),
                                 c("gg_rev_gdp_fm", "gg_rev_gdp_weo", "gg_rev_gdp_gfs", "gg_rev_gdp_gmd"),
                                 c("gg_rev_oth_gdp", "gg_rev_grants_gdp", "gg_rev_soc_gdp", "gg_taxes_gdp"),
                                 c("gg_exnd_gdp_fm", "gg_exnd_gdp_weo", "gg_exnd_gdp_gfs", "gg_exnd_gdp_gmd"),
                                 c("gg_exns_oth_gdp", "gg_exns_transf_gdp", "gg_exns_sub_gdp", "gg_exns_int_gdp", "gg_exns_usegs_gdp", "gg_exns_wages_gdp"),
                                 c("gg_bal_gdp_fm", "gg_bal_gdp_weo", "gg_bal_gdp_gfs", "gg_bal_gdp_gmd"),
                                 c("gg_debt_gdp_fm", "gg_debt_gdp_weo", "gg_debt_gdp_gfs", "gg_debt_gdp_gmd"),
                                 c("wgi_va_rnk", "wgi_ps_rnk", "wgi_cc_rnk", "wgi_rl_rnk", "wgi_rq_rnk", "wgi_ge_rnk"),
                                 c("birth_rate", "death_rate", "migr_rate"),
                                 c("ind_rat_r_score", "m_r_score", "p_r_score", "e_r_score", "i_r_score"),
                                 c("m1r_wealth", "m2r_growth", "m3r_size", "m4r_inflation"),
                                 c("p1r_gg_bal", "p2r_debt_burden", "p3r_extdebt_gg_gdp"),
                                 c("e1r_ca_gdp", "e2r_intres_cover", "e3r_niip", "e4r_ex_div", "e5r_cur_vol"),
                                 c("i1r_polstab", "i2r_ecgov", "i3r_hci"),
                                 c("ccb", "consb", "cap", "lvr", "llp", "lcg", "loanr", "lfc", "ltv", "dsti", "tax", "liq", "ltd", "lfx", "rr", "sifi", "ot"),
                                 c("regcap_to_rwa", "t_one_cap_to_rwa", "cet_one_to_rwa", "t_one_cap_to_ass"),
                                 c("npl_to_loans", "top_thr_sect_to_loans", "net_npl_to_cap", "prov_to_npl", "bank_conc_loans"),
                                 c("bank_roa", "bank_roe", "nim_to_income", "nonint_exns_to_income"),
                                 c("liquid_to_assets", "liquid_to_sr_liab", "lcr", "nsfr"),
                                 c("bank_open_position", "bank_dom_loans", "fc_loans_role", "fc_liab_role"),
                                 c("p_com", "p_oil", "p_metals", "p_agro"))


peers <- c("default", "neighbours", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "BRICS_plus", "EM", "DM", "ACRA")
peers_choice <- c("none", "default", "custom", "neighbours", "formula", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "BRICS_plus", "EM", "DM", "ACRA")

trend_types <- c("", "lm", "loess")
graph_themes <- c("ACRA", "ipsum", "economist", "minimal")
graph_groups <- list(macro = "ec", budget = "budg", external = "ext", institutional = "inst", demography = "demogr", 
                     covid = "covid", model = "model", other = "oth")

sec_y_axis_chosen <- ""

# Parameters
horizontal_size <- c(1800, 900)
vertical_size <- c(900, 900)
verbose <- F


# Interface

ui <-   fluidPage(

  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Graph country data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      ## Basic Input
      
      selectizeInput(
        'graph_type', 'Graph type', choices = graph_types,
        options = list(
          placeholder = 'Select an option below',
          onInitialize = I('function() { this.setValue("bar_country_comparison"); }')
        )),
      
      fluidRow(column(8,
              selectizeInput(
                'country_choice', 'Country', choices = countries,
                options = list(
                    placeholder = 'Select an option below',
                    onInitialize = I('function() { this.setValue("Russian Federation"); }')
                    ))
      ),      
      
      column(4,     
      selectizeInput(
        'data_frequency', 'Data freq', choices = c(" ", "y", "q", "m", "d"),
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(" "); }')
        )
      ))),
      
      ## Indicator input and comparison
      
      selectizeInput('indicators', 'Indicators', choices = indicators_start, multiple = T,
                options = list(
                   placeholder = 'Please select one or more options below',
                   onInitialize = I('function() { this.setValue("gdp_g"); }')
                          )),
      selectInput("ind_group", "Indicator group", choices = indicator_groups, selected = ""),
      textInput("time_fix", "Time fix", ""),
      
      
      h3("Peers"),
      fluidRow(column(6,selectizeInput("peers", "Peer group", choices = peers_choice,
                            options = list(
                                    placeholder = 'Select',
                                    onInitialize = I('function() { this.setValue("default"); }')
                                          )
                                       )),
               column(6,textInput("peers_formula", "Formula", "")),
      ),
      
      selectizeInput(
        'peers_custom', 'Custom list', choices = countries_peers, multiple = T,
        options = list(
          placeholder = 'Select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )),
      
      checkboxInput("all", "Show all countries"),
      
      
      ## Style
      
      h3("Style"),
      fluidRow(
               column(3,textInput("x_min", "X min")),
               column(3,textInput("x_max", "X max")),
               column(3,numericInput("y_min", "Y min", "")),
               column(3,numericInput("y_max", "Y max", ""))             
                      ),
      
      fluidRow(
        column(4,selectInput("trend_type", "Trend", choices = trend_types, selected = "")),
        column(8,selectInput("theme", "Style preset", graph_themes, selected = "ipsum"))
      ),
      
      fluidRow(
        column(8,selectizeInput("sec_y_axis_ind", "2nd Y-axis", choices = indicators_start, multiple = T,
                                options = list(
                                  placeholder = 'Select an option below',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )
        )),
        column(4,numericInput("sec_y_axis_coeff", "Axis mult", "")),
        
      ),
      
      fluidRow(
        column(4,checkboxInput("x_log", "X log")),
        column(3,checkboxInput("y_log", "Y log")),
        column(5,checkboxInput("swap_axis", "Swap axis"))
      ),
      
      
      fluidRow(
        column(4,checkboxInput("recession", "Recession")),
        column(3,checkboxInput("index", "Index")),
        column(5,checkboxInput("long_legend", "Long legend"))
      ),
      
      fluidRow(
        column(6,checkboxInput("short_names", "Short indicator names")),
        column(6,checkboxInput("vert_lab", "Vertical X labels")),
      ),
      
    ),
    
    mainPanel(
      fluidRow( 
        column(3, actionButton("plot_button", "Update Plot")),
        column(3, actionButton("fill_button", "Help with defaults")),
        column(3, actionButton("getPlan", "Import plan")),
        column(3,textInput("graph_plan", NULL, "Graph plan"))
      ),
      br(),
      plotOutput("graph"),
      br(),
      
      ## Output panel
      
      fluidRow(
        column(3, downloadButton("downloadJpeg", "Download jpeg")),
        column(3, downloadButton("downloadPng", "Download png")),
        column(3, downloadButton("downloadData", "Download data")),
        column(3, actionButton("copyPlan", "Export plan"))
        ),
      
      br(),
        
      fluidRow(
        column(5,textAreaInput("graph_title", "Graph Title", "Graph Title")),
        column(2, selectizeInput(
          'graph_group', 'Graph group', choices = graph_groups,
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue("ec"); }')
          )
        )),
        column(3,textInput("graph_name", "File name", "goodgraph")),
        column(2, selectInput("orientation", "Orientation", choices = c("horizontal", "vertical")))
      ),
        
      checkboxInput("show_title", "Show Title")
      
      # debugging instruments
      #,
      # tableOutput("table"),
      # tableOutput("table2"),
      # textOutput("check"),
      # textOutput("check1"),
      # textOutput("check2"),
      # textOutput("check3")
      
    )
  )
)


# Calculations

server <- function(input, output, session) {

  
  ## Listening to the user
  graphplan <- reactive({
                  data.frame(
                            graph_name = glue("{graph_group_short()}_{input$graph_name}"),
                            graph_title = input$graph_title,
                            graph_type = input$graph_type,
                            graph_group = graph_group(),      # external constructor
                            data_frequency = input$data_frequency,
                            indicators = indicators(),        # external constructor
                            time_fix = input$time_fix,
                            peers = peers_string(),           # external constructor
                            all = 1*input$all,
                            x_log = 1*input$x_log,
                            y_log = 1*input$y_log,
                            x_min = input$x_min,
                            x_max = input$x_max,
                            y_min = input$y_min,
                            y_max = input$y_max,
                            trend_type = input$trend_type,
                            index = 1*input$index,
                            recession = 1*input$recession,
                            sec_y_axis = sec_y_string(),      # external constructor
                            swap_axis = 1*input$swap_axis,
                            long_legend = 1*input$long_legend,
                            vert_lab = 1*input$vert_lab,
                            short_names = 1*input$short_names,
                            theme = input$theme,
                            orientation = input$orientation,
                            show_title = 1*input$show_title,
                            active = 1,
                            source_name = sources()
                            ) |> mutate(across(everything(), ~replace(., . ==  "" , NA)))
                      })
  
  ## Constructing peers string from inputs
  
  peers_string <- reactive({
    ifelse(
      input$peers == "none", 
      0,
      ifelse(
        input$peers %in% peers,
        input$peers,
        ifelse(
          input$peers == "custom",
          paste0("custom: ", paste(input$peers_custom, collapse = ", ")),
          ifelse(
            input$peers == "formula",
            input$peers_formula,
            0
                )
              )
            )
          )
  })

  ## Constructing second axis string from inputs
  
  sec_y_string_temp <- reactive({ ifelse(all(input$sec_y_axis_ind != ""), paste(input$sec_y_axis_ind, collapse = ", "), NA) })
  sec_y_string <- reactive({ ifelse(!is.na(input$sec_y_axis_coeff), 
                                    paste(c(sec_y_string_temp(), input$sec_y_axis_coeff), collapse = ", "), 
                                    sec_y_string_temp()) })
  
  ## Constructing long and short group names
  
  graph_group_short <- reactive({ input$graph_group })
  graph_group <- reactive({ names(graph_groups)[which(unlist(graph_groups) == graph_group_short())]  })
  
  
  ## Updating second axis indicator list based on the chosen indicators
  
  observeEvent( input$indicators,
                {   
                  if (all(input$indicators != " ")) {
                    indicators_selected_temp <- indicators_all |> filter(indicator_code %in% input$indicators) 
                    indicators_selected <- indicators_selected_temp |> pull(indicator_code) |> as.list()
                    names(indicators_selected) <- indicators_selected_temp |> pull(indicator)
                    updateSelectizeInput(session, "sec_y_axis_ind", label = "2nd Y-axis", 
                                         choices = indicators_selected, selected = sec_y_axis_chosen)   # корень проблемы затирания
                  }
                })
  
  ## Simultaneously updating indicators list based on the chosen frequency 
  ##    and updating indicators chosen based on the chosen preset
  
  indicator_trigger <- reactive({ list(input$data_frequency, input$ind_group) })
  
  observeEvent( indicator_trigger(),
                {   
                  
                  if (input$data_frequency != " ") {
                    indicators_temp <- indicators_all |> filter(source_frequency == input$data_frequency)} else {
                      indicators_temp <-indicators_all}
                  
                  indicators_variants <- as.list( indicators_temp |> pull(indicator_code) )
                  names(indicators_variants) <- indicators_temp |> pull(indicator)
                  
                  if (input$ind_group != "") {
                    
                    indicators_chosen <- indicator_groups_content[[which(indicator_groups == input$ind_group)]]
                    updateSelectizeInput(session, "indicators", label = "Indicators", 
                                         choices = indicators_variants, selected = indicators_chosen) 
                    
                    } else {
                    indicators_chosen <- ""}
                  
                })
  
  ## Fill graphplan's empty cells
  
    observeEvent(input$fill_button, {
        
        ##### Fill time fix
        if (input$graph_type %in% c("scatter_country_comparison", "structure_country_comparison",
                                    "structure_country_comparison_norm", "bar_country_comparison", "bar_country_comparison_norm") & input$time_fix == "") {
          
          a <- case_when(
            input$data_frequency == "y" ~ "2021",
            input$data_frequency == "q" ~ "2022q4",
            input$data_frequency == "m" ~ "2023m6",
            TRUE ~ "2021"
          )
          
          updateTextInput(session, "time_fix", label = "Time fix", value = a)
        }
        
        ##### Fill left X limit
        if (input$graph_type %in% c("scatter_dynamic", "structure_dynamic", "bar_dynamic", "lines_country_comparison", 
                                    "lines_indicator_comparison", "distribution_dynamic") & input$x_min == "") {
          
          a <- case_when(
            input$data_frequency == "y" ~ c("2005"),
            input$data_frequency == "q" ~ c("2018q1"),
            input$data_frequency == "m" ~ c("2020m1"),
            TRUE ~ "2005"
          )            
          
          updateTextInput(session, "x_min", "X min", value = a)
        }

        ##### Fill right X limit        
        if (input$graph_type %in% c("scatter_dynamic", "structure_dynamic", "bar_dynamic", "lines_country_comparison", 
                                    "lines_indicator_comparison", "distribution_dynamic") & input$x_max == "") {
          
          a <- case_when(
            input$data_frequency == "y" ~ c("2022"),
            input$data_frequency == "q" ~ c("2023q1"),
            input$data_frequency == "m" ~ c("2023m7"),
            TRUE ~ "2022"
          )            
          
          updateTextInput(session, "x_max", "X max", value = a)
        }          
        
        ##### Fill multiple time fix
        if (input$graph_type == "bar_year_comparison") {
          updateTextInput(session, "time_fix", label = "Time fix", value = "2005, 2014, 2021") }
  
    })
  
  ## Import graph plan from excel
    
  observeEvent(input$getPlan, {
    
    if (mode_online == 0) { imported <- read_clip_tbl(header = F) } else {
      
        imported <- read.table(text = input$graph_plan, sep="\t", na.strings=c("", "NA"), header = F)
        
    }
    
    output$table2 <- renderTable({ imported })
    
    
    updateTextInput(session, inputId = "graph_name", label = "File name", 
                    value = gsub(glue("^({paste(paste0(graph_groups,'_'), collapse='|')})"), "", imported[1,1]) )
    
    updateTextAreaInput(session, inputId = "graph_title", label = "Graph Title", value = imported[1,2])
    updateSelectizeInput(session, inputId = 'graph_type', label = 'Graph type', choices = graph_types, selected = imported[1,3])
    updateSelectizeInput(session, inputId = 'graph_group', label = 'Graph group', choices = graph_groups, 
                         selected = graph_groups[[ imported[1,4] ]])
    updateSelectizeInput(session, inputId = 'data_frequency', label = 'Data freq', 
                         choices = c(" ", "y", "q", "m", "d"), selected = imported[1,5])
    updateSelectInput(session, inputId = "ind_group", label = "Indicator group", choices = indicator_groups, selected = "")
    
    updateSelectizeInput(session, inputId = 'indicators', label = 'Indicators', choices = indicators_start,          
                    selected = strsplit(imported[1,6], ", |,")[[1]])  
    updateTextInput(session, inputId = "time_fix", label = "Time fix", value = imported[1,7])
    
    if (imported[1,8] == 0) {
        updateSelectizeInput(session, inputId = "peers", label = "Peer group", choices = peers_choice, selected = "none")
        updateTextInput(session, inputId = "peers_formula", label = "Formula", value = "")
        updateSelectizeInput(session, inputId = 'peers_custom', label = 'Custom list', choices = countries_peers, selected = "")
    }
    
    if (imported[1,8] != 0) {
      
        peers_vec <- unlist(strsplit(imported[1,8], ": |:"))
        peers_type <- peers_vec[1]
        peers_vec <- unlist(strsplit(peers_vec[2], ", "))
  
      
      if (imported[1,8] %in% peers) {
          updateSelectizeInput(session, inputId = "peers", label = "Peer group", choices = peers_choice, 
                               selected = imported[1,8])
          updateTextInput(session, inputId = "peers_formula", label = "Formula", value = "")
          updateSelectizeInput(session, inputId = 'peers_custom', label = 'Custom list', choices = countries_peers, selected = "")
      }
      
      if (peers_type == "custom") {    
          updateSelectizeInput(session, inputId = "peers", label = "Peer group", choices = peers_choice, selected = peers_type)
          updateTextInput(session, inputId = "peers_formula", label = "Formula", value = "")
          updateSelectizeInput(session, inputId = 'peers_custom', label = 'Custom list', choices = countries_peers, selected = peers_vec)
      }
      
      if (peers_type %in% c("top", "low", "similar")) {    
          updateSelectizeInput(session, inputId = "peers", label = "Peer group", choices = peers_choice, selected = "formula")
          updateTextInput(session, inputId = "peers_formula", label = "Formula", value = imported[1,8])
          updateSelectizeInput(session, inputId = 'peers_custom', label = 'Custom list', choices = countries_peers, selected = "")
      }

    }
    
    updateCheckboxInput(session, inputId = "all", label = "Show all countries", value = (imported[1,9] == 1))
    updateCheckboxInput(session, inputId = "x_log", label = "X log", value = (imported[1,10] == 1))
    updateCheckboxInput(session, inputId = "y_log", label = "Y log", value = (imported[1,11] == 1))
    updateTextInput(session, inputId = "x_min", label = "X min", value = imported[1,12])
    updateTextInput(session, inputId = "x_max", label = "X max", value = imported[1,13])
    updateNumericInput(session, inputId = "y_min", label = "Y min",  value = imported[1,14])
    updateNumericInput(session, inputId = "y_max", label = "Y max",  value = imported[1,15])
    updateSelectInput(session, inputId = "trend_type", label = "Trend", choices = trend_types, selected = imported[1,16])
    updateCheckboxInput(session, inputId = "index", label = "Index", value = (imported[1,17] == 1))
    updateCheckboxInput(session, inputId = "recession", label = "Recession", value = (imported[1,18] == 1))
    
    a <- strsplit(ifelse(imported[1,19]!="" & !is.null(imported[1,19]) & !is.na(imported[1,19]),imported[1,19],""), ", |,")[[1]]
    sec_y_axis_chosen <<- a[!grepl("^\\d+$", a)]
    # updateSelectizeInput(session, inputId = "sec_y_axis_ind", label = "2nd Y-axis", choices = indicators_start, 
    #                      selected = a[!grepl("^\\d+$", a)])                    
    updateNumericInput(session, inputId = "sec_y_axis_coeff", label = "Axis mult", value = a[grepl("^\\d+$", a)][1])
    
    updateCheckboxInput(session, inputId = "swap_axis", label = "Swap axis", value = (imported[1,20] == 1))
    updateCheckboxInput(session, inputId = "long legend", label = "Long legend", value = (imported[1,21] == 1))
    updateCheckboxInput(session, inputId = "vert_lab", label = "Vertical X labels", value = (imported[1,22] == 1))
    updateCheckboxInput(session, inputId = "short_names", label = "Short indicator names", value = (imported[1,23] == 1))
    
    updateSelectInput(session, inputId = "theme", label = "Style preset", choices = graph_themes, selected = imported[1,24])
    updateSelectInput(session, inputId = "orientation", label = "Orientation", choices = c("horizontal", "vertical"), selected = imported[1,25])
    updateCheckboxInput(session, inputId = "show_title", label = "Show Title", value = (imported[1,26] == 1))

  })
    
  ## Updating source string based on updated indicators 
  
  generateSources <- function(indicators, dict) {
    x <- unlist(str_extract_all( string = indicators, pattern = paste(na.omit(dict$indicator_code), collapse = "|") ))
    x <- plyr::mapvalues(x, from = dict$indicator_code, to = dict$source_name, warn_missing = F)
    x <- unlist(strsplit(x, ", "))
    x <- x[x!="расчеты АКРА"]
    x <- c(unique(x), "расчеты АКРА")
    x <- toString(x)
    return(x)
  }
  
  indicators <- reactive({ paste(input$indicators, collapse = ", ") })
  sources <-  reactive({ generateSources(indicators = indicators(), dict = D$dict)  })
  
  ## Producing graph (copied from do_plot)
  
  observeEvent(input$plot_button, {
    
        ##### Update country groups
        country_info <- getPeersCodes(country_name = input$country_choice, peers_fname = peers_fname)
    
        ##### Check integrity of the plans
        graphplan_aug <- graphplan() |> filter(active == 1) |>
          checkGraphTypes(graph_types = graph_types) |> 
          checkUnique() |>
          checkPeers(peer_groups = country_info$regions) |> # после того как напишу check проверить
          checkAvailability(dict = D$dict) |>
          mutate(checks = check_types*check_unique*check_peers*check_availability)
        error_report <- graphplan_aug |> filter(checks == 0)
      
        
        if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
          
            
            ### Parsing single graph parameters  
            graph_params <- parseGraphPlan(graphrow = graphplan(), dict = D$dict, horizontal_size = horizontal_size, vertical_size = vertical_size)
            
            ### Fixing peers
            peers_iso2c <- fixPeers(country_info = country_info, peers = graph_params$peers, data = D)
            updateSelectizeInput(session, "peers_custom", label = "Custom list", choices = countries_peers, selected = peers_iso2c)
            
            ### Filtering data to include only needed for the graph
            data_temp <- subsetData(data = D, graph_params = graph_params, country = country_info$country_iso2c, peers = peers_iso2c)
            
            ### Choosing the needed function based on the graph type 
            func_name <- funcNameTransform(graph_type = graph_params$graph_type)
            
            ### Producing the graph to show in app (нормально ли, что я graph_calculated делаю глобальной?)
            graph_calculated <<- eval(parse(text= paste0( 
              func_name, "(data = data_temp, graph_params = graph_params, country_iso2c = country_info$country_iso2c, peers_iso2c = peers_iso2c, verbose = verbose)"
            ) ))    
            output$graph <- renderPlot(graph_calculated$graph)
            
            ### Reproducing the graph for download
            graph_calculated2 <<- eval(parse(text= paste0( 
              func_name, "(data = data_temp, graph_params = graph_params, country_iso2c = country_info$country_iso2c, peers_iso2c = peers_iso2c, verbose = verbose)"
            ) ))
            
            ###Print graph plan
            updateTextInput(session, inputId = "graph_plan", 
                      value = capture.output(write.table(graphplan() |> 
                                  select(-c(source_name)), row.names = F, col.names = F , quote = F, sep = "\t", na="")) )
          
        } else {print("Errors found"); print(error_report)}
        
   
    })

  # Table to export to excel plan
  output$table <- renderTable({ graphplan() })
  
  # Checks
  output$check <- renderText({ sec_y_axis_chosen })
  output$check1 <- renderText({ is.na(input$x_min == "") })
  output$check2 <- renderText({ is.null(input$x_min == "") })
  output$check3 <- renderText({ paste0("non-na" ,!is.na(input$sec_y_axis_ind)) })
  
  
  # Download files
  
  ### Calculating output parameters
  graph_size <- reactive(ifelse(input$orientation == "horizontal", horizontal_size, vertical_size))
  output_name <- reactive(glue("{graph_group_short()}_{input$graph_name}"))
  
  ### Download handlers
  output$downloadJpeg <- downloadHandler(
    
    filename = function() {
      paste(output_name(), "jpeg", sep = ".")
    },
    content = function(file){
      ggsave(file, graph_calculated2$graph, device = "jpeg", width = graph_size()[1], height = graph_size()[2],
             units = "px", dpi = 150, bg = "white")
    }
    
  )
  
  output$downloadPng <- downloadHandler(
    
    filename = function() {
      paste(output_name(), "png", sep = ".")
    },
    content = function(file){
      ggsave(file, graph_calculated2$graph, device = "png", width = graph_size()[1], height = graph_size()[2],
             units = "px", dpi = 150, bg = "white")
    }
    
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() { glue("{input$graph_group}_{input$graph_name}.xlsx") },
    content = function(file) { write_xlsx(graph_calculated$data, file, col_names = T, format_headers = T) }
    
  )
  
  observeEvent(input$copyPlan, {
    
    plan_to_copy <- graphplan() |> select(-c(source_name))
    names(plan_to_copy) <- NULL
    
    if (!is.null(plan_to_copy)) {
      
      if(mode_online == 0) {
        write_clip(plan_to_copy)
        showModal(modalDialog(
          title = "Clipboard Copy",
          "Data copied to clipboard!",
          easyClose = TRUE
        ))
      } else {}
      
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
