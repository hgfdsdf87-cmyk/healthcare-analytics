
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(readr)
library(DT)
library(scales)
library(forcats)

# ── Color palette ───────────────────────────────────────────────────
CLR <- list(
  navy    = "#1F4E79",
  blue    = "#2E75B6",
  lblue   = "#D6E4F0",
  green   = "#1B7A34",
  red     = "#C00000",
  orange  = "#D46B08",
  purple  = "#5B2C8D",
  teal    = "#0E7C7B",
  gray    = "#595959"
)

CONDITION_COLORS <- c(
  "Arthritis"       = "#2E75B6",
  "Asthma"          = "#1B7A34",
  "Cancer"          = "#C00000",
  "Diabetes"        = "#D46B08",
  "Hypertension"    = "#5B2C8D",
  "Obesity"         = "#0E7C7B"
)

# ── Load & clean data ───────────────────────────────────────────────
load_data <- function() {
  if (!file.exists("healthcare_dataset.csv")) {
    stop("❌ healthcare_dataset.csv not found!\n
         Download from: https://www.kaggle.com/datasets/prasad22/healthcare-dataset\n
         Place it in the same folder as this R file.")
  }
  
  df <- read_csv("healthcare_dataset.csv", show_col_types = FALSE)
  
  # Standardise column names
  names(df) <- gsub(" ", "_", tolower(names(df)))
  
  # Parse dates — auto-detect format (handles m/d/Y, Y-m-d, d-m-Y, etc.)
  parse_any_date <- function(x) {
    # Try multiple formats in order
    result <- suppressWarnings(mdy(x))
    if (all(is.na(result))) result <- suppressWarnings(ymd(x))
    if (all(is.na(result))) result <- suppressWarnings(dmy(x))
    if (all(is.na(result))) result <- suppressWarnings(parse_date_time(x,
                                                                       orders = c("mdy","ymd","dmy","mdY","Ymd","dmY","m/d/Y","Y/m/d")))
    result
  }
  df$date_of_admission <- parse_any_date(df$date_of_admission)
  df$discharge_date     <- parse_any_date(df$discharge_date)
  
  # Derived columns
  df$length_of_stay <- as.numeric(df$discharge_date - df$date_of_admission)
  df$admission_year  <- year(df$date_of_admission)
  df$admission_month <- month(df$date_of_admission, label = TRUE, abbr = TRUE)
  df$admission_qtr   <- paste0("Q", quarter(df$date_of_admission))
  df$age_group <- cut(df$age,
                      breaks = c(0, 18, 35, 50, 65, 100),
                      labels = c("0-18", "19-35", "36-50", "51-65", "65+"),
                      right  = TRUE
  )
  df$billing_amount <- round(as.numeric(df$billing_amount), 2)
  
  df
}

df <- tryCatch(load_data(), error = function(e) {
  message(conditionMessage(e))
  NULL
})

# ── UI ──────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = span("🏥 Healthcare Analytics", style = "font-weight:700;")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",           tabName = "overview",   icon = icon("hospital")),
      menuItem("Patient Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Billing Analysis",   tabName = "billing",    icon = icon("dollar-sign")),
      menuItem("Length of Stay",     tabName = "los",        icon = icon("calendar")),
      menuItem("Conditions & Tests", tabName = "conditions", icon = icon("stethoscope")),
      menuItem("Data Explorer",      tabName = "explorer",   icon = icon("table"))
    ),
    hr(),
    tags$div(style = "padding:0 16px;",
             selectInput("filter_condition", "Medical Condition:",
                         choices  = c("All", "Arthritis","Asthma","Cancer","Diabetes","Hypertension","Obesity"),
                         selected = "All"),
             selectInput("filter_admission", "Admission Type:",
                         choices  = c("All","Emergency","Elective","Urgent"),
                         selected = "All"),
             selectInput("filter_gender", "Gender:",
                         choices  = c("All","Male","Female"),
                         selected = "All"),
             actionButton("reset_filters", "Reset Filters",
                          class = "btn-sm btn-default", style = "width:100%;margin-top:8px;")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background:#F0F4F8; }
      .box { border-top:3px solid #2E75B6; border-radius:8px; }
      .value-box .icon { opacity:0.7; }
      .small-box { border-radius:10px; }
      .small-box h3 { font-size:2rem; }
      .sidebar-menu li a { font-size:0.92rem; }
    "))),
    
    tabItems(
      
      # ── TAB 1: OVERVIEW ─────────────────────────────────────────
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("vb_patients",   width = 3),
                valueBoxOutput("vb_avg_billing", width = 3),
                valueBoxOutput("vb_avg_los",    width = 3),
                valueBoxOutput("vb_abnormal",   width = 3)
              ),
              fluidRow(
                box(title = "Monthly Admissions Trend", width = 8, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_monthly_trend", height = 300)),
                box(title = "Admission Type Split", width = 4, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_admission_donut", height = 300))
              ),
              fluidRow(
                box(title = "Patients by Medical Condition", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_condition_bar", height = 280)),
                box(title = "Test Results Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_test_results", height = 280))
              )
      ),
      
      # ── TAB 2: DEMOGRAPHICS ──────────────────────────────────────
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Age Distribution by Medical Condition", width = 7, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_age_condition", height = 350)),
                box(title = "Gender Split by Condition", width = 5, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_gender_condition", height = 350))
              ),
              fluidRow(
                box(title = "Age Group vs Admission Type", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_agegroup_admission", height = 300)),
                box(title = "Blood Type Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_bloodtype", height = 300))
              )
      ),
      
      # ── TAB 3: BILLING ──────────────────────────────────────────
      tabItem(tabName = "billing",
              fluidRow(
                box(title = "Average Billing by Medical Condition", width = 7, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_billing_condition", height = 320)),
                box(title = "Billing by Insurance Provider", width = 5, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_billing_insurance", height = 320))
              ),
              fluidRow(
                box(title = "Billing Amount Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_billing_hist", height = 300)),
                box(title = "Billing vs Length of Stay", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_billing_los", height = 300))
              )
      ),
      
      # ── TAB 4: LENGTH OF STAY ─────────────────────────────────
      tabItem(tabName = "los",
              fluidRow(
                box(title = "Length of Stay by Condition (Box Plot)", width = 7, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_los_box", height = 340)),
                box(title = "Avg LOS by Admission Type", width = 5, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_los_admission", height = 340))
              ),
              fluidRow(
                box(title = "LOS by Age Group", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_los_age", height = 280))
              )
      ),
      
      # ── TAB 5: CONDITIONS & TESTS ────────────────────────────
      tabItem(tabName = "conditions",
              fluidRow(
                box(title = "Test Results by Medical Condition (Heatmap)", width = 7, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_test_heatmap", height = 360)),
                box(title = "Top 10 Hospitals by Patient Volume", width = 5, status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_top_hospitals", height = 360))
              ),
              fluidRow(
                box(title = "Medication Prescribed — Frequency", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_medication", height = 260))
              )
      ),
      
      # ── TAB 6: DATA EXPLORER ────────────────────────────────
      tabItem(tabName = "explorer",
              fluidRow(
                box(title = "Full Dataset Explorer", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("table_full"))
              )
      )
    )
  )
)

# ── SERVER ──────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_condition", selected = "All")
    updateSelectInput(session, "filter_admission", selected = "All")
    updateSelectInput(session, "filter_gender",    selected = "All")
  })
  
  # Reactive filtered data
  df_f <- reactive({
    req(!is.null(df))
    d <- df
    if (input$filter_condition != "All") d <- d %>% filter(medical_condition == input$filter_condition)
    if (input$filter_admission != "All") d <- d %>% filter(admission_type    == input$filter_admission)
    if (input$filter_gender    != "All") d <- d %>% filter(gender            == input$filter_gender)
    d
  })
  
  # ── Value Boxes ─────────────────────────────────────────────────
  output$vb_patients <- renderValueBox({
    valueBox(format(nrow(df_f()), big.mark=","),
             "Total Patients", icon = icon("user-injured"), color = "blue")
  })
  output$vb_avg_billing <- renderValueBox({
    val <- dollar(round(mean(df_f()$billing_amount, na.rm=TRUE), 0))
    valueBox(val, "Avg Billing Amount", icon = icon("dollar-sign"), color = "green")
  })
  output$vb_avg_los <- renderValueBox({
    val <- round(mean(df_f()$length_of_stay, na.rm=TRUE), 1)
    valueBox(paste(val, "days"), "Avg Length of Stay", icon = icon("bed"), color = "purple")
  })
  output$vb_abnormal <- renderValueBox({
    pct <- round(mean(df_f()$test_results == "Abnormal", na.rm=TRUE)*100, 1)
    valueBox(paste0(pct, "%"), "Abnormal Test Results", icon = icon("vial"), color = "red")
  })
  
  # ── Monthly Trend ─────────────────────────────────────────────
  output$plot_monthly_trend <- renderPlotly({
    d <- df_f() %>%
      filter(!is.na(date_of_admission)) %>%
      mutate(ym = floor_date(date_of_admission, "month")) %>%
      filter(!is.na(ym)) %>%
      group_by(ym) %>%
      summarise(n = n(), .groups="drop")
    validate(need(nrow(d) > 0,
                  "Date parsing failed. Run this in RStudio Console to check your date format:\nhead(read_csv('healthcare_dataset.csv')$`Date of Admission`)"))
    p <- ggplot(d, aes(x=ym, y=n)) +
      geom_line(color=CLR$blue, linewidth=1.1) +
      geom_point(color=CLR$navy, size=2.5) +
      geom_area(fill=CLR$lblue, alpha=0.4) +
      scale_x_date(date_labels="%b %Y") +
      labs(x=NULL, y="Admissions") +
      theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank())
    ggplotly(p, tooltip=c("x","y"))
  })
  
  # ── Admission Donut ──────────────────────────────────────────
  output$plot_admission_donut <- renderPlotly({
    d <- df_f() %>% count(admission_type)
    plot_ly(d, labels=~admission_type, values=~n, type="pie",
            hole=0.5,
            marker=list(colors=c("#2E75B6","#1B7A34","#D46B08")),
            textinfo="label+percent") %>%
      layout(showlegend=TRUE,
             margin=list(l=10,r=10,t=10,b=10))
  })
  
  # ── Condition Bar ────────────────────────────────────────────
  output$plot_condition_bar <- renderPlotly({
    d <- df_f() %>%
      count(medical_condition) %>%
      arrange(n) %>%
      mutate(medical_condition = fct_inorder(medical_condition))
    p <- ggplot(d, aes(x=medical_condition, y=n, fill=medical_condition)) +
      geom_bar(stat="identity", show.legend=FALSE) +
      scale_fill_manual(values=CONDITION_COLORS) +
      coord_flip() +
      labs(x=NULL, y="Number of Patients") +
      theme_minimal(base_size=11)
    ggplotly(p, tooltip=c("x","y"))
  })
  
  # ── Test Results Pie ─────────────────────────────────────────
  output$plot_test_results <- renderPlotly({
    d <- df_f() %>% count(test_results)
    plot_ly(d, labels=~test_results, values=~n, type="pie",
            marker=list(colors=c("#1B7A34","#C00000","#D46B08")),
            textinfo="label+percent") %>%
      layout(showlegend=TRUE, margin=list(l=10,r=10,t=10,b=10))
  })
  
  # ── Age by Condition (Violin) ─────────────────────────────────
  output$plot_age_condition <- renderPlotly({
    d <- df_f()
    p <- ggplot(d, aes(x=medical_condition, y=age, fill=medical_condition)) +
      geom_violin(trim=FALSE, alpha=0.7, show.legend=FALSE) +
      geom_boxplot(width=0.12, fill="white", outlier.size=0.8) +
      scale_fill_manual(values=CONDITION_COLORS) +
      labs(x=NULL, y="Age") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=30, hjust=1))
    ggplotly(p)
  })
  
  # ── Gender by Condition ──────────────────────────────────────
  output$plot_gender_condition <- renderPlotly({
    d <- df_f() %>%
      count(medical_condition, gender) %>%
      group_by(medical_condition) %>%
      mutate(pct = n / sum(n) * 100)
    p <- ggplot(d, aes(x=medical_condition, y=pct, fill=gender)) +
      geom_bar(stat="identity") +
      scale_fill_manual(values=c("Male"=CLR$blue,"Female"=CLR$purple)) +
      coord_flip() +
      labs(x=NULL, y="Percentage (%)", fill="Gender") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Age Group vs Admission Type ───────────────────────────────
  output$plot_agegroup_admission <- renderPlotly({
    d <- df_f() %>%
      count(age_group, admission_type) %>%
      drop_na()
    p <- ggplot(d, aes(x=age_group, y=n, fill=admission_type)) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_manual(values=c("Emergency"=CLR$red,"Elective"=CLR$blue,"Urgent"=CLR$orange)) +
      labs(x="Age Group", y="Count", fill="Admission Type") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Blood Type ──────────────────────────────────────────────
  output$plot_bloodtype <- renderPlotly({
    d <- df_f() %>% count(blood_type) %>% arrange(desc(n))
    p <- ggplot(d, aes(x=reorder(blood_type,-n), y=n, fill=blood_type)) +
      geom_bar(stat="identity", show.legend=FALSE) +
      scale_fill_brewer(palette="Set2") +
      labs(x="Blood Type", y="Count") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Billing by Condition ─────────────────────────────────────
  output$plot_billing_condition <- renderPlotly({
    d <- df_f() %>%
      group_by(medical_condition) %>%
      summarise(avg_billing = mean(billing_amount, na.rm=TRUE), .groups="drop") %>%
      arrange(desc(avg_billing))
    p <- ggplot(d, aes(x=reorder(medical_condition,-avg_billing),
                       y=avg_billing, fill=medical_condition)) +
      geom_bar(stat="identity", show.legend=FALSE) +
      scale_fill_manual(values=CONDITION_COLORS) +
      scale_y_continuous(labels=dollar_format()) +
      labs(x=NULL, y="Average Billing ($)") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=30, hjust=1))
    ggplotly(p, tooltip=c("x","y"))
  })
  
  # ── Billing by Insurance ─────────────────────────────────────
  output$plot_billing_insurance <- renderPlotly({
    d <- df_f() %>%
      group_by(insurance_provider) %>%
      summarise(avg = mean(billing_amount,na.rm=TRUE), n=n(), .groups="drop")
    p <- ggplot(d, aes(x=reorder(insurance_provider,-avg), y=avg, fill=insurance_provider)) +
      geom_bar(stat="identity", show.legend=FALSE) +
      scale_y_continuous(labels=dollar_format()) +
      labs(x=NULL, y="Avg Billing ($)") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=30, hjust=1))
    ggplotly(p)
  })
  
  # ── Billing Histogram ────────────────────────────────────────
  output$plot_billing_hist <- renderPlotly({
    p <- ggplot(df_f(), aes(x=billing_amount)) +
      geom_histogram(bins=40, fill=CLR$blue, color="white", alpha=0.85) +
      geom_vline(xintercept=mean(df_f()$billing_amount,na.rm=TRUE),
                 color=CLR$red, linetype="dashed", linewidth=1) +
      scale_x_continuous(labels=dollar_format()) +
      labs(x="Billing Amount ($)", y="Count") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Billing vs LOS Scatter ───────────────────────────────────
  output$plot_billing_los <- renderPlotly({
    d <- df_f() %>% sample_n(min(nrow(df_f()), 1000))
    p <- ggplot(d, aes(x=length_of_stay, y=billing_amount,
                       color=medical_condition, text=paste("Patient age:", age))) +
      geom_point(alpha=0.5, size=1.5) +
      scale_color_manual(values=CONDITION_COLORS) +
      scale_y_continuous(labels=dollar_format()) +
      labs(x="Length of Stay (days)", y="Billing Amount ($)", color="Condition") +
      theme_minimal(base_size=11)
    ggplotly(p, tooltip=c("x","y","text","colour"))
  })
  
  # ── LOS Boxplot ──────────────────────────────────────────────
  output$plot_los_box <- renderPlotly({
    p <- ggplot(df_f(), aes(x=medical_condition, y=length_of_stay, fill=medical_condition)) +
      geom_boxplot(outlier.size=0.8, alpha=0.8, show.legend=FALSE) +
      scale_fill_manual(values=CONDITION_COLORS) +
      labs(x=NULL, y="Length of Stay (days)") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=30, hjust=1))
    ggplotly(p)
  })
  
  # ── LOS by Admission Type ────────────────────────────────────
  output$plot_los_admission <- renderPlotly({
    d <- df_f() %>%
      group_by(admission_type) %>%
      summarise(avg_los = mean(length_of_stay, na.rm=TRUE), .groups="drop")
    p <- ggplot(d, aes(x=admission_type, y=avg_los, fill=admission_type)) +
      geom_bar(stat="identity", width=0.5, show.legend=FALSE) +
      scale_fill_manual(values=c("Emergency"=CLR$red,"Elective"=CLR$blue,"Urgent"=CLR$orange)) +
      labs(x=NULL, y="Avg LOS (days)") +
      theme_minimal(base_size=12)
    ggplotly(p)
  })
  
  # ── LOS by Age Group ─────────────────────────────────────────
  output$plot_los_age <- renderPlotly({
    d <- df_f() %>%
      filter(!is.na(age_group)) %>%
      group_by(age_group, medical_condition) %>%
      summarise(avg_los=mean(length_of_stay,na.rm=TRUE), .groups="drop")
    p <- ggplot(d, aes(x=age_group, y=avg_los, fill=medical_condition)) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_manual(values=CONDITION_COLORS) +
      labs(x="Age Group", y="Avg LOS (days)", fill="Condition") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Test Result Heatmap ──────────────────────────────────────
  output$plot_test_heatmap <- renderPlotly({
    d <- df_f() %>%
      count(medical_condition, test_results) %>%
      group_by(medical_condition) %>%
      mutate(pct = n/sum(n)*100)
    p <- ggplot(d, aes(x=test_results, y=medical_condition, fill=pct)) +
      geom_tile(color="white", lwd=0.8) +
      geom_text(aes(label=paste0(round(pct,1),"%")), size=3.5, fontface="bold") +
      scale_fill_gradient(low="#D6E4F0", high="#1F4E79") +
      labs(x="Test Result", y=NULL, fill="% of Condition") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Top Hospitals ────────────────────────────────────────────
  output$plot_top_hospitals <- renderPlotly({
    d <- df_f() %>%
      count(hospital) %>%
      top_n(10, n) %>%
      arrange(n) %>%
      mutate(hospital = fct_inorder(hospital))
    p <- ggplot(d, aes(x=hospital, y=n)) +
      geom_bar(stat="identity", fill=CLR$teal) +
      coord_flip() +
      labs(x=NULL, y="Patient Volume") +
      theme_minimal(base_size=10)
    ggplotly(p)
  })
  
  # ── Medication ──────────────────────────────────────────────
  output$plot_medication <- renderPlotly({
    d <- df_f() %>%
      count(medication) %>%
      arrange(desc(n))
    p <- ggplot(d, aes(x=reorder(medication,-n), y=n, fill=medication)) +
      geom_bar(stat="identity", show.legend=FALSE) +
      scale_fill_brewer(palette="Set1") +
      labs(x="Medication", y="Prescriptions") +
      theme_minimal(base_size=11)
    ggplotly(p)
  })
  
  # ── Data Table ───────────────────────────────────────────────
  output$table_full <- renderDT({
    d <- df_f() %>%
      select(name, age, gender, blood_type, medical_condition,
             date_of_admission, admission_type, hospital, doctor,
             insurance_provider, billing_amount, length_of_stay,
             medication, test_results)
    datatable(d,
              filter   = "top",
              rownames = FALSE,
              options  = list(pageLength=15, scrollX=TRUE,
                              dom='lftip')
    ) %>%
      formatCurrency("billing_amount", "$") %>%
      formatStyle("test_results",
                  backgroundColor = styleEqual(
                    c("Normal","Abnormal","Inconclusive"),
                    c("#D4EDDA","#F8D7DA","#FFF3CD")))
  })
}

shinyApp(ui, server)