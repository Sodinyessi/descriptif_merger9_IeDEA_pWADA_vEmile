# ===============================
#   Cohort Profile Dashboard
#   IeDEA West Africa – Pediatric cohort
# ===============================

library(shiny)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(kableExtra)
library(tidyr)


#Import data
chemin_apin <- "C:/Users/Emile/Desktop/IeDEA/MERGER 9/IeDEA M9 09oct25"
fichiers_apin <- list.files(path = chemin_apin, pattern = "\\.sas7bdat$", full.names = TRUE)
wa_apin_tbl <- lapply(fichiers_apin, read_sas)
names(wa_apin_tbl) <- file_path_sans_ext(basename(fichiers_apin))

#After having completed by hand in excel the coding of each art_id re-import the file and merge
artx_bis <- read.csv2("C:/Users/Emile/Desktop/IeDEA/projet réponse virologique/TRV pwada vEmile Sept25/File TRV pwada vEmile Sept25/bdd TRV pwada/art_ids_missing_arvtx.csv")

wa_apin_lower <- lapply(wa_apin_tbl, \(x) rename_with(x, tolower)) #Environnement & rename columns
names(wa_apin_lower) <- paste0("wa_apin_", names(wa_apin_lower))
list2env(wa_apin_lower, envir = .GlobalEnv)

wa_apin_full_m9_ped_apin <- wa_apin_full_m9_ped_apin %>%
  mutate(
    lastcontact_d = format(as.Date(lastcontact_d, origin = "1960-01-01"), "%Y-%m-%d"))

wa_apin_tblcenter <- wa_apin_tblcenter %>%
  mutate(
    close_d = format(as.Date(close_d, origin = "1960-01-01"), "%Y-%m-%d"),
    open_d = format(as.Date(open_d, origin = "1960-01-01"), "%Y-%m-%d"))
# ---- Chargement ou préparation des données ----
# (ici on suppose que ton dataset "wa_apin_full_m9_ped_apin" est déjà chargé dans ton environnement)

# Ajout de la variable patient_status
base0 <- wa_apin_full_m9_ped_apin %>%
  mutate(
    patient_status = case_when(
      groupe1 %in% c(1, 2) ~ "Infected",
      groupe1 == 3 ~ "HIV-Exposed"
    ),
    patient_status = factor(patient_status, levels = c("Infected", "HIV-Exposed"))
  )

#Create variables
wa_apin_inf <- wa_apin_full_m9_ped_apin %>% filter(groupe1 %in% c(1,2)) #

#Rename variable
tbl_inf_charc <- wa_apin_inf
tbl_inf_charc <- tbl_inf_charc %>%
  mutate(hiv_type = ifelse(is.na(hiv_type) | hiv_type %in% c(4, 9, 999), 1, hiv_type),
         hiv_type = case_when(
           hiv_type == 1 ~ "HIV - 1",
           hiv_type == 2 ~ "HIV - 2",
           hiv_type == 3 ~ "HIV - 1&2"),
         year_of_inclusion_to_cohort = case_when(
           !is.na(enrol_y) & enrol_y < 2006 ~ "< 2006",
           enrol_y >= 2006 & enrol_y <= 2008 ~ "2006 - 2008",
           enrol_y >= 2009 & enrol_y <= 2013 ~ "2009 - 2013",
           enrol_y >= 2014 & enrol_y <= 2016 ~ "2014 - 2016",
           enrol_y >= 2017 & enrol_y <= 2020 ~ "2017 - 2020",
           enrol_y >= 2021 ~ ">= 2021"),
         age_last_year_5cl = case_when(
           !is.na(age_last_year) & age_last_year < 5 ~ "< 5 years",
           age_last_year >= 5  & age_last_year < 10  ~ "5 – 9 years",
           age_last_year >= 10 & age_last_year < 16 ~ "10 – 15 years",
           age_last_year >= 16 & age_last_year < 20 ~ "16 – 19 years",
           age_last_year >= 20 & age_last_year <= 24 ~ "20 – 24 years"),
         outcomes = case_when(
           outcome == 1 ~ "Death",
           outcome == 2 ~ "Transferred out",
           outcome == 3 ~ "Loss to follow_up",
           outcome == 4 ~ "Censorhip at age 24",
           outcome == 5 ~ "Follow_up"),
         clinical_stage_at_inclusion_6months = case_when(
           stage_enrol == 1 ~ "CDC A/B. WHO I/II",
           stage_enrol == 2 ~ "CDC C. WHO III/IV",
           stage_enrol == 9 ~ "Not documented"
         ))

tbl_inf_charc$hiv_type = factor(tbl_inf_charc$hiv_type,
                                levels = c("HIV - 1","HIV - 2", "HIV - 1&2"))

tbl_inf_charc$outcomes = factor(tbl_inf_charc$outcomes,
                                levels = c("Death","Transferred out", "Loss to follow_up", "Follow_up", "Censorhip at age 24"))

tbl_inf_charc$age_last_year_5cl = factor(tbl_inf_charc$age_last_year_5cl,
                                         levels = c("< 5 years", "5 – 9 years", "10 – 15 years", "16 – 19 years", "20 – 24 years"))

tbl_inf_charc$year_of_inclusion_to_cohort <- factor(tbl_inf_charc$year_of_inclusion_to_cohort, 
                                                    levels = c("< 2006", "2006 - 2008", "2009 - 2013", "2014 - 2016", "2017 - 2020", ">= 2021"))


tbl_inf_charc <- tbl_inf_charc %>% rename(sex_at_birth = sex,
                                          age_med_at_enroL_yrs = age_enrol_year,
                                          med_follow_up_since_enroL_yrs = del_enrol_end_y,
                                          age_at_last_follow_up_yr = age_last_year,
                                          class_age_at_last_contact = age_last_year_5cl)
tbl_inf_charc <- tbl_inf_charc %>%
  mutate(sex_at_birth = if_else(sex_at_birth == 1, "Male", "FeMale"))

tbl_inf_charc <- tbl_inf_charc %>% mutate(OVERALL = "OVERALL")


##########################
wa_apin_inf <- wa_apin_inf %>%
  mutate(f_art_y =year(f_art_d))
art_init <-  wa_apin_inf %>%
  filter(groupe1 == 1) %>%
  mutate(
    class_year_of_art_initiation = case_when(
      !is.na(f_art_y) & f_art_y < 2015 ~ "< 2015",
      !is.na(f_art_y) & f_art_y >= 2015 & f_art_y <= 2016 ~ "2015-2016",
      !is.na(f_art_y) & f_art_y >= 2017 & f_art_y <= 2018 ~ "2017-2018",
      !is.na(f_art_y) & f_art_y >= 2019 & f_art_y <= 2020 ~ "2019-2020",
      !is.na(f_art_y) & f_art_y >= 2021 & f_art_y <= 2022 ~ "2021-2022",
      !is.na(f_art_y) & f_art_y >= 2023 ~ ">=2023",
      is.na(f_art_y) ~ "Missing"
    ))

art_init$class_year_of_art_initiation <- factor(
  art_init$class_year_of_art_initiation,
  levels = c("< 2015", "2015-2016", "2017-2018", "2019-2020", "2021-2022", ">=2023", "Missing"))

art_init$follow_Up_duration_since_art_initiation <- art_init$del_enrol_end_y

art_init <- art_init %>%
  mutate(class_age_follow_Up_duration_since_art_initiation = case_when(
    !is.na(age_art_year) & age_art_year < 1  ~ "< 1 year",
    age_art_year >= 1 & age_art_year < 2 ~ "[1-2[ years",
    age_art_year >= 2 & age_art_year < 3 ~ "[2-3[ years",
    age_art_year >= 3 & age_art_year < 4 ~ "[3-4[ years",
    age_art_year >= 4 & age_art_year < 5 ~ "[4-5[ years",
    age_art_year >= 5 ~ ">= 5 years"
  )) %>%
  mutate(class_age_follow_Up_duration_since_art_initiation = factor(
    class_age_follow_Up_duration_since_art_initiation,
    levels = c("< 1 year", "[1-2[ years", "[2-3[ years", "[3-4[ years", "[4-5[ years", ">= 5 years"),
    ordered = TRUE
  ))

art_init$haart_regimen <- as.factor(art_init$haart_regimen)
art_init <-  art_init %>% rename(Type_of_first_art_regimen   = haart_regimen,
                                 age_at_art_initiation_year    = age_art_year,
                                 sex_at_birth = sex)

art_init <- art_init %>%
  mutate(sex_at_birth = if_else(sex_at_birth == 1, "Male", "FeMale"))
###############################


##\section{CD4 cell count measures}
bdd_cd4_mesar <- wa_apin_tbllab_cd4 %>%
  left_join(wa_apin_full_m9_ped_apin %>% select(patient, f_art_d, enrol_y, enrol_d), by = "patient") %>% filter(cd4_u==1)
bdd_cd4_percent <- wa_apin_tbllab_cd4 %>%
  left_join(wa_apin_full_m9_ped_apin %>% select(patient, f_art_d, enrol_y, enrol_d), by = "patient") %>% filter(cd4_u==2)
########################################
wa_apin_full_m9_ped_apin <- wa_apin_full_m9_ped_apin %>%
  mutate(l_alive_y = year(l_alive_d))
##########################################




# ======================================================
#       DASHBOARD COHORT PROFILE – IeDEA West Africa
# ======================================================

# ---- Chargement des librairies ----
library(shiny)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(kableExtra)

# ---- Préparation des données ----
base0 <- wa_apin_full_m9_ped_apin %>%
  mutate(
    patient_status = case_when(
      groupe1 %in% c(1, 2) ~ "Infected",
      groupe1 == 3 ~ "HIV-Exposed",
      TRUE ~ NA_character_
    ),
    patient_status = factor(patient_status, levels = c("Infected", "HIV-Exposed"))
  )

# ======================================================
#                INTERFACE UTILISATEUR
# ======================================================
ui <- fluidPage(
  titlePanel("COHORT PROFILE – IeDEA WEST AFRICA PEDIATRIC"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "program_choice",
        label = "Select Program:",
        choices = c("OVERALL", sort(unique(base0$program))),
        selected = "OVERALL"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        # --- Résumé des fichiers actifs ---
        tabPanel(
          "Summary Table",
          h4("Active Files by Patient Status"),
          tableOutput("summary_table")
        ),
        
        # --- Caractéristiques de base ---
        tabPanel(
          "Baseline Characteristics",
          h4("Enrolment Trends among HIV-Infected Children"),
          plotOutput("baseline_plot", height = "500px")
        ),
        tabPanel(
          "Center Closure Dates",
          h4("Dates de clôture des centres IeDEA + APIN"),
          # on utilisera tableOutput pour afficher le tableau
          tableOutput("closure_dates_table")
        ),
        tabPanel(
          "Baseline Characteristics Table",
          h4("Baseline Characteristics of HIV-Infected Children"),
          selectInput(
            inputId = "program_char_choice",
            label = "Select Program for Characteristics Table:",
            choices = c("OVERALL", sort(unique(tbl_inf_charc$program))),
            selected = "OVERALL"
          ),
          uiOutput("baseline_char_table")
        ),
        tabPanel(
          "Overall Baseline Summary",
          h4("Baseline Summary – Continuous & Categorical Variables"),
          selectInput(
            inputId = "program_overall_choice",
            label = "Select Program for Overall Summary:",
            choices = c("OVERALL", sort(unique(tbl_inf_charc$program))),
            selected = "OVERALL"
          ),
          uiOutput("overall_char_table")
        ),
        tabPanel(
          "Outcomes by Sex",
          h4("Outcomes of HIV-Infected Children by Sex"),
          selectInput(
            inputId = "program_outcome_choice",
            label = "Select Program for Outcomes Table:",
            choices = c("OVERALL", sort(unique(tbl_inf_charc$program))),
            selected = "OVERALL"
          ),
          uiOutput("outcomes_sex_table")
        ),
        tabPanel(
          "ART Initiation",
          h4("Baseline Characteristics of Children with ART Initiation"),
          selectInput(
            inputId = "program_art_choice",
            label = "Select Program for ART Initiation:",
            choices = c("OVERALL", sort(unique(art_init$program))),
            selected = "OVERALL"
          ),
          uiOutput("art_initiation_table")
        ),
        tabPanel(
          "CD4 Measurements",
          h4("Trends in CD4 cell count measurement since 2004"),
          selectInput(
            inputId = "program_cd4_choice",
            label = "Select Program for CD4 measurements:",
            choices = c("OVERALL", sort(unique(bdd_cd4_mesar$program))),
            selected = "OVERALL"
          ),
          plotOutput("cd4_trend_plot", height = "400px"),
          verbatimTextOutput("cd4_summary_text")
        ),
        tabPanel(
          "CD4 Evolution",
          h4("CD4 Count Evolution by Year of Enrollment"),
          selectInput(
            inputId = "program_cd4_evol_choice",
            label = "Select Program:",
            choices = c("OVERALL", sort(unique(wa_apin_full_m9_ped_apin$program))),
            selected = "OVERALL"
          ),
          plotOutput("cd4_evol_plot", height = "500px")
        ),
        tabPanel(
          "HIV Viral Load",
          h4("Trends in HIV Viral Load Measurements"),
          selectInput(
            inputId = "program_rna_choice",
            label = "Select Program:",
            choices = c("OVERALL", sort(unique(wa_apin_full_m9_ped_apin$program))),
            selected = "OVERALL"
          ),
          plotOutput("rna_plot", height = "400px"),
          br(),
          h4("Summary per patient"),
          tableOutput("rna_summary_table")
        ),
        
        
        
        
        
        
        # --- 🔹 Zone libre pour de futures analyses 🔹 ---
        # Vous pouvez ajouter ici vos prochains graphiques / tableaux :
        # Exemple :
        # tabPanel("New Analysis", plotOutput("new_plot")),
        # tabPanel("Virological Response", tableOutput("virology_table"))
        # ------------------------------------------------
      )
    )
  )
)

# ======================================================
#                   LOGIQUE SERVEUR
# ======================================================
server <- function(input, output, session) {
  
  # ---- Filtrage selon le programme sélectionné ----
  base_filtered <- reactive({
    if (input$program_choice == "OVERALL") {
      base0
    } else {
      base0 %>% filter(program == input$program_choice)
    }
  })
  
  # ==================================================
  #     TABLEAU RÉSUMÉ : FILES ACTIVES PAR STATUT
  # ==================================================
  output$summary_table <- renderTable({
    
    data_tab <- base_filtered() %>%
      filter(file_active == 1) %>%
      mutate(all_active_file = "Active Files")
    
    tab1 <- data_tab %>%
      ungroup() %>%
      tbl_summary(
        by = all_active_file,
        include = c(patient_status),
        missing = "no"
      ) %>%
      add_overall(last = TRUE) %>%
      bold_labels() %>%
      modify_spanning_header(
        update = all_stat_cols() ~ "**Profile: Active Files**"
      )
    
    tab1_kable <- tab1 %>%
      as_kable(format = "html", booktabs = TRUE, linesep = "") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        font_size = 14,
        full_width = FALSE
      )
    
    tab1_kable
  }, sanitize.text.function = function(x) x)
  
  # ==================================================
  #     GRAPHIQUE : TENDANCES D’ENRÔLEMENT
  # ==================================================
  output$baseline_plot <- renderPlot({
    
    wa_apin_inf <- base_filtered() %>%
      filter(groupe1 %in% c(1, 2)) %>%
      mutate(
        groupe_label = case_when(
          groupe1 == 1 ~ "ART Initiation",
          groupe1 == 2 ~ "Unknown ART Initiation"
        )
      )
    
    ggplot(
      wa_apin_inf %>% filter(enrol_y > 2005 & enrol_y < 2024),
      aes(x = enrol_y, color = groupe_label)
    ) +
      geom_line(stat = "count", size = 1.2) +
      geom_point(stat = "count", size = 2) +
      scale_color_manual(
        values = c("ART Initiation" = "#1F77B4", "Unknown ART Initiation" = "#D62728")
      ) +
      scale_x_continuous(
        breaks = seq(2005, 2023, 1),
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      labs(
        x = "Year of Enrolment",
        y = "Number of Enrolments",
        color = "",
        title = paste0(
          "Enrolment Trends among HIV-Infected Children – ",
          ifelse(input$program_choice == "OVERALL", "All Sites", input$program_choice)
        )
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  })
  output$closure_dates_table <- renderTable({
    
    date_closed <- wa_apin_tblcenter %>%
      select(program, name, country, city, open_d, close_d) %>%
      arrange(close_d)
    
    # On peut renommer les colonnes pour affichage plus lisible
    date_closed <- date_closed %>%
      rename(
        Program = program,
        Center = name,
        Country = country,
        City = city,
        Open_Date = open_d,
        Close_Date = close_d
      )
    
    # Retourne le tableau
    date_closed
  }, striped = TRUE, hover = TRUE, spacing = "m")
  
  output$baseline_char_table <- renderUI({
    
    # filtrage selon le choix
    tbl_data <- if (input$program_char_choice == "OVERALL") {
      tbl_inf_charc
    } else {
      tbl_inf_charc %>% filter(program == input$program_char_choice)
    }
    
    # sécurité si pas de données
    if(nrow(tbl_data) == 0){
      return(HTML("<em>No data available for the selected program.</em>"))
    }
    
    # construction du tableau gtsummary
    tbl_char1 <- tbl_data %>%
      ungroup() %>%
      tbl_summary(
        by = if (input$program_char_choice == "OVERALL") NULL else "program",
        include = c("sex_at_birth", "hiv_type", "age_med_at_enroL_yrs",
                    "year_of_inclusion_to_cohort", "med_follow_up_since_enroL_yrs",
                    "age_at_last_follow_up_yr", "class_age_at_last_contact")
      ) %>%
      bold_labels() %>%
      modify_spanning_header(
        all_stat_cols() ~ "**Infected**"
      )
    
    # conversion en HTML pour Shiny
    tbl_char1_html <- tbl_char1 %>%
      as_kable(format = "html", booktabs = TRUE, linesep = "") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        font_size = 12,
        full_width = FALSE
      )
    
    HTML(tbl_char1_html)
  })
  output$overall_char_table <- renderUI({
    
    # filtrage selon le programme choisi
    tbl_data <- if (input$program_overall_choice == "OVERALL") {
      tbl_inf_charc
    } else {
      tbl_inf_charc %>% filter(program == input$program_overall_choice)
    }
    
    # sécurité si pas de données
    if(nrow(tbl_data) == 0){
      return(HTML("<em>No data available for the selected program.</em>"))
    }
    
    # variables catégorielles et continues
    cat_vars <- c("sex_at_birth", "hiv_type", "class_age_at_last_contact", 
                  "clinical_stage_at_inclusion_6months", "year_of_inclusion_to_cohort")
    cont_vars <- c("age_med_at_enroL_yrs", "med_follow_up_since_enroL_yrs", "age_at_last_follow_up_yr")
    
    # construction du tableau gtsummary
    tbl_char4 <- tbl_data %>%
      select(OVERALL, all_of(cat_vars), all_of(cont_vars)) %>%
      tbl_summary(
        by = if (input$program_overall_choice == "OVERALL") NULL else "OVERALL",
        type = list(all_of(cont_vars) ~ "continuous", all_of(cat_vars) ~ "categorical"),
        statistic = list(
          all_of(cont_vars) ~ "{median} [{p25}; {p75}]",
          all_of(cat_vars) ~ "{n} ({p}%)"
        ),
        digits = list(
          all_of(cont_vars) ~ c(1,1),
          all_of(cat_vars) ~ c(0,1)
        ),
        missing = "ifany",
        label = list(
          sex_at_birth ~ "Sex",
          hiv_type ~ "HIV type",
          age_med_at_enroL_yrs ~ "Age (years) at enrollment",
          year_of_inclusion_to_cohort ~ "Year of inclusion to cohort",
          med_follow_up_since_enroL_yrs ~ "Median follow-up since enrollment (years)",
          age_at_last_follow_up_yr ~ "Age (years) at last follow-up",
          class_age_at_last_contact ~ "Age category at last contact",
          clinical_stage_at_inclusion_6months ~ "WHO clinical stage at enrolment (+/-6 months)"
        )
      ) %>%
      bold_labels()
    
    # conversion en HTML pour Shiny
    tbl_char4_html <- tbl_char4 %>%
      as_kable(format = "html", booktabs = TRUE, linesep = "") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        font_size = 12,
        full_width = FALSE
      )
    
    HTML(tbl_char4_html)
  })
  output$outcomes_sex_table <- renderUI({
    
    # filtrage selon le programme choisi
    tbl_data <- if (input$program_outcome_choice == "OVERALL") {
      tbl_inf_charc
    } else {
      tbl_inf_charc %>% filter(program == input$program_outcome_choice)
    }
    
    # sécurité si pas de données
    if(nrow(tbl_data) == 0){
      return(HTML("<em>No data available for the selected program.</em>"))
    }
    
    # construction du tableau gtsummary
    tbl_char6 <- tbl_data %>%
      ungroup() %>%
      tbl_summary(
        by = "sex_at_birth",
        include = c("outcomes")
      ) %>%
      add_overall(last = TRUE) %>%
      bold_labels() %>%
      modify_spanning_header(
        all_stat_cols() ~ "**INFECTED**"
      )
    
    # conversion en HTML pour Shiny
    tbl_char6_html <- tbl_char6 %>%
      as_kable(format = "html", booktabs = TRUE, linesep = "") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        font_size = 12,
        full_width = FALSE
      )
    
    HTML(tbl_char6_html)
  })
  
  output$art_initiation_table <- renderUI({
    
    # filtrage selon le programme choisi
    tbl_data <- if (input$program_art_choice == "OVERALL") {
      art_init
    } else {
      art_init %>% filter(program == input$program_art_choice)
    }
    
    # sécurité si pas de données
    if(nrow(tbl_data) == 0){
      return(HTML("<em>No data available for the selected program.</em>"))
    }
    
    # construction du tableau gtsummary
    tbl_char10 <- tbl_data %>%
      ungroup() %>%
      tbl_summary(
        include = c(
          "sex_at_birth",
          "age_at_art_initiation_year",
          "class_year_of_art_initiation",
          "Type_of_first_art_regimen",
          "follow_Up_duration_since_art_initiation",
          "class_age_follow_Up_duration_since_art_initiation"
        ),
        type = list(
          all_categorical() ~ "categorical",
          all_continuous() ~ "continuous"
        ),
        statistic = list(
          all_categorical() ~ "{n} ({p}%)",
          all_continuous() ~ "{median} [{p25}; {p75}]"
        ),
        digits = list(
          all_categorical() ~ c(0, 1),
          all_continuous() ~ c(2, 1)
        ),
        missing = "no",
        label = list(
          sex_at_birth ~ "Sex",
          age_at_art_initiation_year ~ "Age at ART initiation (years)",
          class_year_of_art_initiation ~ "Year of ART initiation (class)",
          Type_of_first_art_regimen ~ "Type of first ART regimen",
          follow_Up_duration_since_art_initiation ~ "Follow-up duration since ART initiation (years)",
          class_age_follow_Up_duration_since_art_initiation ~ "Follow-up duration class since ART initiation"
        )
      ) %>%
      bold_labels() %>%
      modify_spanning_header(
        all_stat_cols() ~ "**ART INITIATION**"
      )
    
    # conversion en HTML pour Shiny
    tbl_char10_html <- tbl_char10 %>%
      as_kable(format = "html", booktabs = TRUE, linesep = "") %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        font_size = 10,
        full_width = FALSE
      )
    
    HTML(tbl_char10_html)
  })
  
  # ---- CD4 trend plot ----
  output$cd4_trend_plot <- renderPlot({
    
    tbl_data <- if(input$program_cd4_choice == "OVERALL") {
      bdd_cd4_mesar
    } else {
      bdd_cd4_mesar %>% filter(program == input$program_cd4_choice)
    }
    
    # préparation
    tbl_data <- tbl_data %>%
      mutate(year_mesure = year(cd4_d)) %>%
      filter(!is.na(year_mesure), year_mesure >= 2004 & year_mesure < 2025)
    
    cd4_count_year <- tbl_data %>%
      group_by(year_mesure) %>%
      summarise(n_mesures = n(), .groups = "drop")
    
    ggplot(cd4_count_year, aes(x = year_mesure, y = n_mesures)) +
      geom_line(size = 1.2, color = "darkorange") +
      geom_point(size = 2, color = "darkorange") +
      scale_x_continuous(breaks = 2004:2024) +
      labs(
        x = "Year of measurement",
        y = "Number of measurements",
        title = paste0("CD4 measurements trend – ", ifelse(input$program_cd4_choice=="OVERALL","All Programs",input$program_cd4_choice))
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold")
      )
  })
  
  # ---- CD4 summary (median, Q1, Q3 per patient) ----
  output$cd4_summary_text <- renderPrint({
    
    tbl_data <- if(input$program_cd4_choice == "OVERALL") {
      bdd_cd4_mesar
    } else {
      bdd_cd4_mesar %>% filter(program == input$program_cd4_choice)
    }
    
    tbl_nbr_mes_pat <- tbl_data %>%
      group_by(patient) %>%
      summarise(nbmesr_cd4 = sum(!is.na(cd4_v)), .groups = "drop")
    
    median_val <- round(median(tbl_nbr_mes_pat$nbmesr_cd4, na.rm = TRUE), 0)
    q1_val <- round(quantile(tbl_nbr_mes_pat$nbmesr_cd4, 0.25, na.rm = TRUE), 0)
    q3_val <- round(quantile(tbl_nbr_mes_pat$nbmesr_cd4, 0.75, na.rm = TRUE), 0)
    
    cat("CD4 measurements per patient:\n")
    cat("Median:", median_val, "\n")
    cat("Q1:", q1_val, "\n")
    cat("Q3:", q3_val, "\n")
  })
  
  output$cd4_evol_plot <- renderPlot({
    
    base_data <- if(input$program_cd4_evol_choice == "OVERALL") {
      wa_apin_full_m9_ped_apin
    } else {
      wa_apin_full_m9_ped_apin %>% filter(program == input$program_cd4_evol_choice)
    }
    
    gpl_cd4_2 <- base_data %>%
      filter(enrol_y >= 2003 & enrol_y < 2025) %>%
      group_by(enrol_y) %>%
      summarise(n_enrolled = n_distinct(patient), .groups = "drop") %>%
      left_join(
        bdd_cd4_mesar %>%
          filter(!is.na(cd4_v)) %>%
          filter(cd4_d >= (enrol_d - 90) & cd4_d <= (enrol_d + 30)) %>%
          {if(input$program_cd4_evol_choice=="OVERALL") . else filter(., program == input$program_cd4_evol_choice)} %>%
          group_by(enrol_y) %>%
          summarise(
            n_cd4 = n_distinct(patient),
            n_cd4_lt200 = n_distinct(patient[cd4_v < 200]),
            .groups = "drop"
          ) %>%
          mutate(
            n_cd4_only = n_cd4,
            label = ifelse(n_cd4 > 0, round(100 * n_cd4_lt200 / n_cd4), NA)
          ),
        by = "enrol_y"
      ) %>%
      mutate(
        n_enrolled = replace_na(n_enrolled, 0),
        n_cd4_only = replace_na(n_cd4_only, 0),
        n_cd4_lt200 = replace_na(n_cd4_lt200, 0),
        label = ifelse(is.na(label), "", as.character(label))
      ) %>%
      select(enrol_y, n_enrolled, n_cd4_only, n_cd4_lt200, label) %>%
      rename(
        "patients enrolled" = n_enrolled,
        "CD4 cell count at available" = n_cd4_only,
        "CD4 cell count < 200" = n_cd4_lt200
      ) %>%
      pivot_longer(
        cols = c("patients enrolled", "CD4 cell count at available", "CD4 cell count < 200"),
        names_to = "type",
        values_to = "count"
      ) %>%
      mutate(
        type = factor(type, levels = c("patients enrolled", "CD4 cell count at available", "CD4 cell count < 200"))
      ) %>%
      ggplot(aes(x = factor(enrol_y), y = count, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(
        data = . %>% filter(type == "CD4 cell count < 200"),
        aes(label = label),
        position = position_nudge(x = 0.29),
        vjust = -0.5,
        size = 3.5
      ) +
      scale_fill_manual(
        values = c(
          "patients enrolled" = "#084594",
          "CD4 cell count at available" = "#6BAED6",
          "CD4 cell count < 200" = "#CB181D"
        )
      ) +
      labs(
        x = "Year of enrollment",
        y = "Number of patients",
        fill = "",
        title = paste0("CD4 Evolution – ", ifelse(input$program_cd4_evol_choice=="OVERALL","All Programs",input$program_cd4_evol_choice))
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(face = "bold")
      )
    
    gpl_cd4_2
  })
  # Reactive base pour HIV RNA selon le programme choisi
  rna_base_filtered <- reactive({
    if (input$program_rna_choice == "OVERALL") {
      wa_apin_full_m9_ped_apin
    } else {
      wa_apin_full_m9_ped_apin %>% filter(program == input$program_rna_choice)
    }
  })
  
  # Graphique HIV Viral Load
  output$rna_plot <- renderPlot({
    
    base_data <- rna_base_filtered()
    
    bdd_rna_mesar_filtered <- wa_apin_tbllab_rna %>%
      filter(!is.na(rna_v)) %>%
      left_join(
        base_data %>% select(patient, enrol_d, enrol_y, f_art_d, l_alive_d),
        by = "patient"
      ) %>%
      mutate(year_mesure = year(rna_d)) %>%
      filter(!is.na(year_mesure), year_mesure >= 2004 & year_mesure < 2024)
    
    rna_count_year <- bdd_rna_mesar_filtered %>%
      group_by(year_mesure) %>%
      summarise(n_mesures = n(), .groups = "drop")
    
    ggplot(rna_count_year, aes(x = year_mesure, y = n_mesures)) +
      geom_line(size = 1.2, color = "darkorange") +
      geom_point(size = 2, color = "darkorange") +
      scale_x_continuous(breaks = 2004:2023) +
      labs(
        x = "Year of measurement",
        y = "Number of measurements",
        title = paste0(
          "HIV Viral Load Measurements – ",
          ifelse(input$program_rna_choice == "OVERALL", "All Programs", input$program_rna_choice)
        )
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(size = 13, face = "bold")
      )
  })
  
  # Tableau résumé par patient
  output$rna_summary_table <- renderTable({
    
    base_data <- rna_base_filtered()
    
    bdd_rna_mesar_filtered <- wa_apin_tbllab_rna %>%
      filter(!is.na(rna_v)) %>%
      left_join(
        base_data %>% select(patient, enrol_d, enrol_y, f_art_d, l_alive_d),
        by = "patient"
      )
    
    tbl_nbr_mes_pat <- bdd_rna_mesar_filtered %>%
      group_by(patient) %>%
      summarise(nbmesr_rna_v = sum(!is.na(rna_v)), .groups = "drop")
    
    data.frame(
      Min = min(tbl_nbr_mes_pat$nbmesr_rna_v, na.rm = TRUE),
      Q1 = quantile(tbl_nbr_mes_pat$nbmesr_rna_v, 0.25, na.rm = TRUE),
      Median = median(tbl_nbr_mes_pat$nbmesr_rna_v, na.rm = TRUE),
      Q3 = quantile(tbl_nbr_mes_pat$nbmesr_rna_v, 0.75, na.rm = TRUE),
      Max = max(tbl_nbr_mes_pat$nbmesr_rna_v, na.rm = TRUE)
    )
    
  }, digits = 0)
  
  
  
  
  # ==================================================
  #     🔹 ESPACE POUR COMPLÉTER VOS MODULES 🔹
  # ==================================================
  # Exemple :
  # output$new_plot <- renderPlot({...})
  # output$new_table <- renderTable({...})
}

# ======================================================
#                   LANCER L’APPLICATION
# ======================================================
shinyApp(ui = ui, server = server)

