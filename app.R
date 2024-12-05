#################### 1. Preparation ####################
############### 1.1 Install Packages ###############
# Installiere nötige Pakete
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("shinyBS")) install.packages("shinyBS")
if (!require("Hmisc")) install.packages("Hmisc")












############### 1.2 Load Packages ###############
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(shinythemes)
library(shinyBS)
library(Hmisc)












############### 1.3 Define Panel-Function ###############
BSCollapseArrow <- function(text, icon_class = "glyphicon-menu-down") {
  HTML(sprintf(
    '<div class="panel-title-container">
       <i class="glyphicon %s"></i>
       <span>%s</span>
     </div>', 
    icon_class, text
  ))
}
tags$head(
  tags$style(HTML("
.panel-heading {
    background-color: red;
    color: white;
}

.panel-body {
    background-color: blue;
    color: white;
}

  "))
)

tags$head(
  tags$style(HTML("
    .axis-settings .col-sm-6 {
      border: 1px solid #ddd;
      padding: 10px;
      margin-bottom: 10px;
    }
  "))
)




















#################### 2. UI ####################
ui <- fluidPage(
  ############### 2.1 General Settings ###############
  # Define Theme
  theme = shinytheme("cerulean"),
  
  tags$head(
    tags$style(HTML("
    
    .panel{
        border: 2px solid #dddddd;
    }

    .panel-body {
        background-color: #f5f5f5 !important;
    }
    
    h1, h2, h3, h4, h5, h6 {
        color: #000000 !important; /* Schwarz */
    }

  "))
  ),
  
  
  
  fluidRow(
    column(3, titlePanel(title = span(img(src = "logo.png", height = 80), HTML('<span style="font-size: 2.5em;">ggpilot</span>')))),
    column(9, align = "center", 
           style = "margin-top: 15px;",
           tags$head(
             tags$style(HTML("
            .custom-btn {
              font-size: 20px;
              padding: 5px 30px;
              margin: 20px 10px;
              color: black;
              border: 5px solid #bbb;
              border-radius: 5px;
              outline: none;
              border-style: double;
            }
            .custom-btn:hover {
              background-color: #f0f0f0;
            }
            .active-btn {
              background-color: #e0e0e0;
              color: black;
              background-image: none;
            }
          "))
           ),
           tags$script(HTML("
            Shiny.addCustomMessageHandler('setActiveButton', function(btnId) {
              $('.custom-btn').removeClass('active-btn');
              $('#' + btnId).addClass('active-btn');
            });
         ")),
           actionButton("btn_data", label = HTML('<i class="glyphicon glyphicon-folder-open"></i> Daten'), class = "custom-btn"),
           actionButton("btn_variables", label = HTML('<i class="glyphicon glyphicon-tasks"></i> Variablen'), class = "custom-btn"),
           actionButton("btn_plot_options", label = HTML('<i class="glyphicon glyphicon-wrench"></i> Plot Optionen'), class = "custom-btn"),
           actionButton("btn_text", label = HTML('<i class="glyphicon glyphicon-font"></i> Text'), class = "custom-btn"),
           actionButton("btn_layout", label = HTML('<i class="glyphicon glyphicon-adjust"></i> Layout'), class = "custom-btn"),
    )
  ),
  
  
  # Verstecktes Input für activeTab
  tags$div(
    textInput("activeTab", label = NULL, value = "data"),
    style = "display: none;"
  ),  
  # Set Title
  # titlePanel("ggpilot"),
  
  # Set to use Shinyjs
  # useShinyjs(),
  
  # Define Sidebar
  sidebarLayout(
    
    #Define Panel in Sidebar with width of 3
    sidebarPanel(width = 4,
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ############### 2.2 Input Fields ###############
                 conditionalPanel(
                   condition = "input.activeTab == 'data'",
                   ########## 2.2.1 Select Data ########## 
                   # Add Button for File Input
                   fileInput("file", "Datensatz auswählen",
                             buttonLabel = "Durchsuchen", placeholder = "Keine Datei ausgewählt", 
                             accept = c(".csv", ".xlsx", ".rds"))),
                 
                 
                 
                 
                 
                 ########## 2.2.2 Select Variables ##########
                 conditionalPanel(
                   condition = "input.activeTab == 'variables'",
                   # Define a collapsable pannel
                   # bsCollapsePanel(
                   #   # Use HTM with an Icon 
                   #   title = HTML( ## <i class="glyphicon glyphicon-menu-down" style="margin-right: 8px;"></i>
                   #   '<div style="display: flex; align-items: center;">
                   #   <i class="glyphicon glyphicon-tasks" style="margin-right: 8px;"></i>
                   #   <span>Variablen definieren</span>
                   #   </div>'
                   #   ),
                   # X-Axis Variable
                   selectInput("x_var", "X-Achsen Variable", choices = c(""), selected = ""),
                   # Y-Axis Variable
                   selectInput("y_var", "Y-Achsen Variable", choices = c(""), selected = ""),
                   # Grouping Variable
                   selectInput("group_var", "Gruppierungs-Variable", choices = c(""), selected = ""),
                   # Facet Grid - Columns
                   selectInput("grid_col_var", "Variable für Spalten-Facettierung", choices = c(""), selected = ""),
                   # Facet Grid - Rows
                   selectInput("grid_row_var", "Variable für Zeilen-Facettierung", choices = c(""), selected = "")),
                 
                 
                 
                 
                 
                 ########## 2.2.3 Plot Options ##########
                 conditionalPanel(
                   condition = "input.activeTab == 'plot_options'",
                   # Define a collapsable pannel
                   # bsCollapsePanel(
                   #   # Use HTML with an Icon 
                   #   title = HTML( #<i class="glyphicon glyphicon-cog" style="margin-right: 8px;"></i>
                   #   '<div style="display: flex; align-items: center;">
                   #   <i class="glyphicon glyphicon-wrench" style="margin-right: 8px;"></i>
                   #   <span>Plot Einstellungen</span>
                   #   </div>'
                   #   ),
                   
                   # Theme
                   selectInput(inputId = "plot_theme", label = "Theme", choices = c("Bw", "Classic", "Gray", "Linedraw", "Light", "Dark", "Minimal", "Void"), selected = "Gray"),
                   
                   # Set a HTML header for the Y-Axis Range Text
                   HTML('<label class="control-label">Range X-Achse</label>'),
                   
                   # Define the min and max value next to each other
                   div(
                     # Define styla
                     style = "display: flex; justify-content: space-between; gap: 10px;",
                     div(
                       style = "flex: 1;",
                       # Numeric Input field for the minimal X-Axis value
                       numericInput(inputId = "x_axis_min", label = HTML('<span style="font-weight: normal;">Min</span>'), step = 0.1, value = "")
                     ),
                     div(
                       style = "flex: 1;",
                       # Numeric Input field for the max X-Axis value
                       numericInput(inputId = "x_axis_max", label = HTML('<span style="font-weight: normal;">Max</span>'), step = 0.1, value = "")
                     )
                   ),
                   
                   # Set a HTML header for the X-Axis Range Text
                   HTML('<label class="control-label">Range Y-Achse</label>'),
                   
                   # Define the min and max value next to each other
                   div(
                     # Define styla
                     style = "display: flex; justify-content: space-between; gap: 10px;",
                     div(
                       style = "flex: 1;",
                       # Numeric Input field for the minimal Y-Axis value
                       numericInput(inputId = "y_axis_min", label = HTML('<span style="font-weight: normal;">Min</span>'), step = 0.1, value = "")
                     ),
                     div(
                       style = "flex: 1;",
                       # Numeric Input field for the max Y-Axis value
                       numericInput(inputId = "y_axis_max", label = HTML('<span style="font-weight: normal;">Max</span>'), step = 0.1, value = "")
                     )
                   ),
                   # Dropbdown to select the type of errorbar
                   selectInput(inputId = "error_type", label = "Fehlerbalken", choices = c("Keiner", "Standardabweichung", "Konfidenzintervall", "Standardfehler"), selected = "Standardabweichung"),
                   # Numeric Input for the width of the errorbar
                   numericInput(inputId = "error_width", label = "Grösse Fehlerbalken", min = 0, max = 2, step = 0.1, value = 0.5),
                   # Numeric Input for the position-dodge value
                   numericInput(inputId = "dodge_value", label = "Abstand", min = 0, max = 2, step = 0.1, value = 0.9)),
                 
                 
                 
                 
                 
                 ########## 2.2.4 Text ##########
                 conditionalPanel(
                   condition = "input.activeTab == 'text'",
                   # Define a collapsable pannel
                   
                   # bsCollapsePanel(
                   #   # Use HTML with an Icon 
                   #   title = HTML(
                   #     '<div style="display: flex; align-items: center;">
                   #     <i class="glyphicon glyphicon-font" style="margin-right: 8px;"></i>
                   #     <span>Text</span>
                   #     </div>'
                   #     ),
                   # Text-Input for the Title
                   textInput(inputId = "plot_title", label = "Titel", value = "", placeholder = "Titel eingeben"),
                   # Text-Input for the Sub-Title
                   textInput(inputId = "plot_subtitle", label = "Untertitel", value = "", placeholder = "Untertitel eingeben"),
                   # Text-Input for the X-Axis-Title
                   textInput(inputId = "x_axis_title", label = "X-Achsen-Beschriftung", value = "", placeholder = "Eigene X-Achsen Beschriftung eingeben"),
                   # Text-Input for the Y-Axis-Title
                   textInput(inputId = "y_axis_title", label = "Y-Achsen-Beschriftung", value = "", placeholder = "Eigene Y-Achsen Beschriftung eingeben"),
                   # Text-Input for the Legend-Title
                   textInput(inputId = "legend_title", label = "Legenden-Beschriftung", value = "", placeholder = "Eigene Legenden Beschriftung eingeben")
                 ),
                 
                 
                 
                 
                 
                 ########## 2.2.5 Layout ##########
                 conditionalPanel(
                   condition = "input.activeTab == 'layout'",
                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                              bsCollapsePanel(
                                title = BSCollapseArrow("Überschrift"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("Titel"),
                                           # Text-Input for the Title
                                           selectInput(inputId = "Title_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Title_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Title_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Title_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "Title_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                    ),
                                    column(6,
                                           h3("Untertitel"),
                                           title = BSCollapseArrow("Untertitel"),
                                           # Text-Input for the X-Axis-Title
                                           selectInput(inputId = "Subtitle_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Subtitle_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Subtitle_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Subtitle_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "Subtitle_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                           )
                                )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Achsen Überschrift"),
                                div(class = "axis-settings", 
                                    column(6,
                                           h3("X-Achse"),
                                           # Text-Input for the X-Axis-Title
                                           selectInput(inputId = "X_Axis_Title_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "X_Axis_Title_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "X_Axis_Title_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "X_Axis_Title_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "X_Axis_Title_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                    ),
                                    column(6, 
                                           h3("Y-Achse"),
                                           # Text-Input for the Y-Axis-Title
                                           selectInput(inputId = "Y_Axis_Title_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Y_Axis_Title_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Y_Axis_Title_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Y_Axis_Title_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "Y_Axis_Title_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                           )
                                    )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Achsen-Text"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("X Achse"),
                                           # Text-Input for the X-Axis Label
                                           selectInput(inputId = "Axis_X_Text_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Axis_X_Text_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Axis_X_Text_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Axis_X_Text_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           numericInput(inputId = "Axis_X_Text_Rotation", label = "Rotation", min = 0, max = 360, step = 1, value = NA),
                                           selectInput(inputId = "Axis_X_Text_H_Alignment", label = "Vertikale Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Axis_X_Text_V_Alignment", label = "Horizonalte Ausrichtung", choices = c("Gemäss Theme", "Unten", "Mittig", "Oben"), selected = "Gemäss Theme")
                                           ),
                                    column(6,
                                           h3("Y Achse"),
                                           # Text-Input for the Y-Axis Label
                                           selectInput(inputId = "Axis_Y_Text_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Axis_Y_Text_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Axis_Y_Text_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Axis_Y_Text_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           numericInput(inputId = "Axis_Y_Text_Rotation", label = "Rotation", min = 0, max = 360, step = 1, value = NA),
                                           selectInput(inputId = "Axis_Y_Text_H_Alignment", label = "Vertikale Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Axis_Y_Text_V_Alignment", label = "Horizonalte Ausrichtung", choices = c("Gemäss Theme", "Unten", "Mittig", "Oben"), selected = "Gemäss Theme")
                                           )
                                  )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Achsen-Linien"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("X-Achse"),
                                           selectInput(inputId = "Axis_X_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Axis_X_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Axis_X_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           selectInput(inputId = "Axis_X_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Axis_X_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Axis_X_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    ),
                                    column(6,
                                           h3("Y-Achse"),
                                           selectInput(inputId = "Axis_Y_Text_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Axis_Y_Text_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Axis_Y_Text_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Axis_Y_Text_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "Axis_Y_Text_H_Alignment", label = "Vertikale Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Axis_Y_Text_V_Alignment", label = "Horizonalte Ausrichtung", choices = c("Gemäss Theme", "Unten", "Mittig", "Oben"), selected = "Gemäss Theme")
                                    )
                                )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Achsen-Ticks"),
                                div(class = "axis-settings",
                                    column(6,
                                            h3("X-Achse"),
                                            numericInput(inputId = "Axis_X_Ticks_Length", label = "Länge", min = 0, max = 50, step = 0.1, value = NA),
                                            numericInput(inputId = "Axis_X_Ticks_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                            selectInput(inputId = "Axis_X_Ticks_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                            textInput(inputId = "Axis_X_Ticks_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                            ),
                                    column(6,
                                            h3("Y-Achse"),
                                            numericInput(inputId = "Axis_Y_Ticks_Length", label = "Länge", min = 0, max = 50, step = 0.1, value = NA),
                                            numericInput(inputId = "Axis_Y_Ticks_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                            selectInput(inputId = "Axis_Y_Ticks_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                            textInput(inputId = "Axis_Y_Ticks_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    )
                                )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Haupt-Linien"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("X-Achse"),
                                           selectInput(inputId = "Major_Grid_X_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Major_Grid_X_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Major_Grid_X_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    ),
                                    column(6,
                                           h3("Y-Achse"),
                                           selectInput(inputId = "Major_Grid_Y_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Major_Grid_Y_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Major_Grid_Y_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    )
                                )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Minor-Linien"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("X-Achse"),
                                           selectInput(inputId = "Minor_Grid_X_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Minor_Grid_X_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Minor_Grid_X_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    ),
                                    column(6,
                                           h3("Y-Achse"),
                                           selectInput(inputId = "Minor_Grid_Y_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Minor_Grid_Y_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Minor_Grid_Y_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    )
                                )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Hintergrund"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("X-Achse"),
                                           textInput(inputId = "Plot_Background_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           selectInput(inputId = "Plot_Background_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Plot_Background_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Plot_Background_Line_Color", label = "Linien-Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    ),
                                    column(6,
                                           h3("Y-Achse"),
                                           textInput(inputId = "Panel_Background_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           selectInput(inputId = "Panel_Background_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                           numericInput(inputId = "Panel_Background_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                           textInput(inputId = "Panel_Background_Line_Color", label = "Linien-Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                    )
                                )
                              ),
                              bsCollapsePanel(
                                title = BSCollapseArrow("Legende"),
                                div(class = "axis-settings",
                                    column(6,
                                           h3("Titel"),
                                           # Text-Input for the Legend-Title
                                           selectInput(inputId = "Legend_Title_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Legend_Title_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Legend_Title_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Legend_Title_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "Legend_Title_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                           ),
                                    column(6,
                                           h3("Items"),
                                           # Text-Input for the X-Axis-Title
                                           selectInput(inputId = "Legend_Text_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                           selectInput(inputId = "Legend_Text_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                           textInput(inputId = "Legend_Text_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                           numericInput(inputId = "Legend_Text_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                           selectInput(inputId = "Legend_Text_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                           )
                                )
                            ),
                            bsCollapsePanel(
                              title = BSCollapseArrow("Legenden Hintergrund"),
                              div(class = "axis-settings",
                                  column(6,
                                         h3("Legenden-Box"),
                                         title = BSCollapseArrow("Legenden-Hintergrund"),
                                         textInput(inputId = "Legend_Background_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                         selectInput(inputId = "Legend_Background_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                         numericInput(inputId = "Legend_Background_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                         textInput(inputId = "Legend_Background_Line_Color", label = "Linien-Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                  ),
                              )
                            ),
                            bsCollapsePanel(
                              title = BSCollapseArrow("Legenden Optionen"),
                              div(class = "axis-settings",
                                  column(6,
                                         h3("Anordnung"),
                                         selectInput(inputId = "Legend_Position", label = "Position der Legende", choices = c("Gemäss Theme", "Keine", "Rechts", "Links", "Unten", "Oben"), selected = "Gemäss Theme"),
                                         selectInput(inputId = "Legend_Title_Position", label = "Position des Legenden-Titel", choices = c("Gemäss Theme", "Rechts", "Links", "Unten", "Oben"), selected = "Gemäss Theme"),
                                         selectInput(inputId = "Legend_Text_Position", label = "Position der Legenden-Items", choices = c("Gemäss Theme", "Rechts", "Links", "Unten", "Oben"), selected = "Gemäss Theme"),
                                         selectInput(inputId = "Legend_Text_Direktion", label = "Ausrichtung der Legenden-Items", choices = c("Gemäss Theme", "Vertikal", "Horizontal"), selected = "Gemäss Theme")
                                  ),
                                  column(6,
                                         h3("Grösse & Abstände"),
                                         numericInput(inputId = "Legend_Key_Width", label = "Breite der Symbolen", min = 0, max = 50, step = 0.1, value = NA),
                                         numericInput(inputId = "Legend_Key_Height", label = "Höhe der Symbolen", min = 0, max = 50, step = 0.1, value = NA),
                                         numericInput(inputId = "Legend_Key_Spacing", label = "Abstand der Symbole", min = 0, max = 50, step = 0.1, value = NA),
                                         numericInput(inputId = "Legend_Box_Spacing", label = "Abstand zum Plot", min = 0, max = 50, step = 0.1, value = NA)
                                         )
                              )
                            ),
                            bsCollapsePanel(
                              title = BSCollapseArrow("Facetten Hintergrund"),
                              div(class = "axis-settings",
                                  column(6,
                                         h3("Zeilen"),
                                         textInput(inputId = "Stripe_X_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                         selectInput(inputId = "Stripe_X_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                         numericInput(inputId = "Stripe_X_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                         textInput(inputId = "Stripe_X_Line_Color", label = "Linien-Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                  ),
                                  column(6,
                                         h3("Spalten"),
                                         textInput(inputId = "Stripe_Y_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                         selectInput(inputId = "Stripe_Y_Linetype", label = "Linien-Art", choices = c("Gemäss Theme", "Keine", "Solide", "Gestrichelt", "Gepunkted", "Punktgestrichelt", "Langgestrichen", "Doppelt gestrichelt"), selected = "Gemäss Theme"),
                                         numericInput(inputId = "Stripe_Y_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                         textInput(inputId = "Stripe_Y_Line_Color", label = "Linien-Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen")
                                  )
                              )
                            ),
                            bsCollapsePanel(
                              title = BSCollapseArrow("Facetten-Beschriftung"),
                              div(class = "axis-settings",
                                  column(6,
                                         h3("Zeilen"),
                                         selectInput(inputId = "Stripe_X_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                         selectInput(inputId = "Stripe_X_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                         textInput(inputId = "Stripe_X_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                         numericInput(inputId = "Stripe_X_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                         selectInput(inputId = "Stripe_X_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                  ),
                                  column(6,
                                         h3("Spalten"),
                                         selectInput(inputId = "Stripe_Y_Font", label = "Schirftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                         selectInput(inputId = "Stripe_Y_Face", label = "Formattierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                         textInput(inputId = "Stripe_Y_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                         numericInput(inputId = "Stripe_Y_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                         selectInput(inputId = "Stripe_Y_Alignment", label = "Ausrichtung", choices = c("Gemäss Theme", "Linksbündig", "Mittig", "Rechtsbündig"), selected = "Gemäss Theme")
                                  )
                              )
                            )
                            #Abstand zwischen Facetten
                            #Abstand Legende u Plot
                   )
                 ),
                 
                 
                 
                 
                 
                 ########## 2.2.10 Execute R-Code ##########
                 # Define a Checkbox for R-Code execution
                 checkboxInput("show_code", "R-Code ausgeben", value = FALSE)
    ),
    
    
    
    
    
    
    
    
    ############### 2.3 Main Panel ###############
    ########## 2.3.1 Plot-Output ##########
    # Define the Main-Panel
    mainPanel(
      # Set the plot as output
      plotOutput("plot"),
      
      
      
      
      
      ########## 2.3.2 Conditional-Panel ##########
      # Conditional-Panel which depends on rcode-button
      conditionalPanel(
        # If input.show_code is true
        condition = "input.show_code",
        # Add TextOutput for rcode
        verbatimTextOutput("rcode")
      )
    )
  )
)




















#################### 3. Server ####################
server <- function(input, output, session) {
  
  ############### 3.1 Read Data ###############
  # Create a reactive data with the loaded data
  data <- reactive({
    if (is.null(input$file)) {
      # Rückgabe eines leeren Standard-Datensatzes, wenn keine Datei geladen wurde
      return(data.frame(
        Placeholder_X = numeric(0),
        Placeholder_Y = numeric(0)
      ))
    } else {
      # If input-file is selected
      req(input$file)
      # Get the type of selected file
      file_ext <- tools::file_ext(input$file$name)
      
      # If file type is csv, read csv-File
      if (file_ext == "csv") {
        read.csv(input$file$datapath, header = TRUE)
      }
      # If file type is xlsx, read xlsx-File
      else if (file_ext == "xlsx") {
        read_excel(input$file$datapath)
      }
      # If file type is rds, load rds-File
      else if (file_ext == "rds") {
        readRDS(input$file$datapath)
      }
      # If file is other type, stop
      else {
        stop("Unbekanntes Dateiformat. Bitte laden Sie eine CSV-, XLSX- oder RDS-Datei hoch.")
      }
    }})
  
  
  # Reactiver Wert für aktiven Tab
  activeTab <- reactiveVal("data")
  
  # Button Events
  observeEvent(input$btn_data, {
    activeTab("data")
    session$sendCustomMessage("setActiveButton", "btn_data")
  })
  observeEvent(input$btn_variables, {
    activeTab("variables")
    session$sendCustomMessage("setActiveButton", "btn_variables")
  })
  observeEvent(input$btn_plot_options, {
    activeTab("plot_options")
    session$sendCustomMessage("setActiveButton", "btn_plot_options")
  })
  observeEvent(input$btn_text, {
    activeTab("text")
    session$sendCustomMessage("setActiveButton", "btn_text")
  })
  observeEvent(input$btn_layout, {
    activeTab("layout")
    session$sendCustomMessage("setActiveButton", "btn_layout")
  })
  
  # Buttons zum Steuern des aktiven Tabs
  observeEvent(input$btn_data, { activeTab("data") })
  observeEvent(input$btn_variables, { activeTab("variables") })
  observeEvent(input$btn_plot_options, { activeTab("plot_options") })
  observeEvent(input$btn_text, { activeTab("text") })
  observeEvent(input$btn_layout, { activeTab("layout") })
  
  # Aktiven Tab als Input verfügbar machen
  observe({
    updateTextInput(session, "activeTab", value = activeTab())
  })
  
  
  
  
  
  
  
  
  ############### 3.2 Update Variable Dropdown ###############
  observeEvent(data(), {
    updateSelectInput(session, "x_var", choices = c("Keine Variable" = " ", names(data())), selected = " ")
    updateSelectInput(session, "y_var", choices = c("Keine Variable" = " ", names(data())), selected = " ")
    updateSelectInput(session, "group_var", choices = c("Keine Variable" = " ", names(data())), selected = " ")
    updateSelectInput(session, "grid_col_var", choices = c("Keine Variable" = " ", names(data())), selected = " ")
    updateSelectInput(session, "grid_row_var", choices = c("Keine Variable" = " ", names(data())), selected = " ")
  })
  
  
  
  
  
  
  
  
  ############### 3.3 Generate Basic R-Code ###############
  # Define Code-Text
  r_code <- reactive({
    req(data())
    
    
    
    
    
    ########## 3.3.1 Set Variables Based on Input-Data ##########
    # X-Axis Variable
    x_var <- if (input$x_var == " ") "1" else input$x_var
    # Y-Axis Variable
    y_var <- if (input$y_var == " ") "1" else input$y_var
    # Grouping Variable
    group_var <- if (input$group_var == " ") NULL else input$group_var
    # Column Variable
    grid_col_var <- if (input$grid_col_var == " ") NULL else input$grid_col_var
    # Row Variable
    grid_row_var <- if (input$grid_row_var == " ") NULL else input$grid_row_var
    
    # Plot Title
    plot_title <- if (input$plot_title== "") NULL else input$plot_title
    # Plot Subtitle
    plot_subtitle <- if (input$plot_subtitle== "") NULL else input$plot_subtitle
    # X-Axis Title
    x_axis_title <- if (input$x_axis_title== "") NULL else input$x_axis_title
    # Y-Axis Title
    y_axis_title <- if (input$y_axis_title== "") NULL else input$y_axis_title
    # Legend Title
    legend_title <- if (input$legend_title== "") NULL else input$legend_title
    
    # Min Range of X-Axis
    x_axis_min <- if (is.na(input$x_axis_min)==TRUE) NA else input$x_axis_min
    # Max Range of X-Axis
    x_axis_max <- if (is.na(input$x_axis_max)==TRUE) NA else input$x_axis_max
    # Min Range of Y-Axis
    y_axis_min <- if (is.na(input$y_axis_min)==TRUE) NA else input$y_axis_min
    # Max Range of X-Axis
    y_axis_max <- if (is.na(input$y_axis_max)==TRUE) NA else input$y_axis_max
    
    # Errorbar Width
    error_width <- if (!is.na(input$error_width)) input$error_width else NULL
    # Position-Dodge value
    dodge_value <- if (is.na(input$dodge_value)==TRUE) NA else input$dodge_value
    # Selected Theme
    theme_selected <- input$plot_theme
    
    
    
    
    
    ########## 3.3.2 Generate Code-Output ##########
    # Define Code Lines
    r_code <- sprintf("q <- ggplot(data(), aes_string(x = '%s', y = '%s'", x_var, y_var)
    
    
    
    
    ########## 3.3.3 Add Grouping Variable ########## 
    # Define Grouping-variable if selected
    if (!is.null(group_var)) {
      r_code <- paste0(r_code, sprintf(", fill = '%s'))", group_var))
      # Close first line of Plot-relevant Code if no Grouping Variable is selected
    } else {
      r_code <- paste0(r_code, sprintf("))"))
    }
    
    
    
    
    
    ########## 3.3.4 Define Bar-Geoms ########## 
    if (x_var != "1" && y_var != "1") {
      r_code <- paste0(r_code, " +\n  stat_summary(fun = mean, geom = 'bar'")
      if (!is.na(dodge_value)) {
        r_code <- paste0(r_code, sprintf(", position = position_dodge(width = %s)", dodge_value))
      }
      r_code <- paste0(r_code, ")")
      
      
      
      
      
      ########## 3.3.5 Define Errorbar-Geom ##########       
      if (input$error_type != "Keiner") {
        r_code <- paste0(r_code, sprintf(" +\n  stat_summary(fun.data = %s, geom = 'errorbar'",
                                         switch(
                                           input$error_type,
                                           "Standardabweichung" = "mean_sdl",
                                           "Konfidenzintervall" = "mean_cl_normal",
                                           "Standardfehler" = "mean_se",
                                           "mean_sdl" # Default-Wert
                                         )))
        if (!is.null(error_width)) {
          r_code <- paste0(r_code, sprintf(", width = %s", error_width))
        }
        if (!is.na(dodge_value)) {
          r_code <- paste0(r_code, sprintf(", position = position_dodge(width = %s)", dodge_value))
        }
        r_code <- paste0(r_code, ")")
      }
    }
    
    
    
    
    
    ########## 3.3.6 Define Facets ##########  
    # Facet hinzufügen, wenn definiert
    if (!is.null(grid_col_var) & !is.null(grid_row_var)) {
      r_code <- paste0(r_code, sprintf(" +\n  facet_grid(rows = vars(%s), cols = vars(%s))", grid_row_var, grid_col_var))
    } else if (!is.null(grid_col_var)) {
      r_code <- paste0(r_code, sprintf(" +\n  facet_wrap(vars(%s))", grid_col_var))
    } else if (!is.null(grid_row_var)) {
      r_code <- paste0(r_code, sprintf(" +\n  facet_wrap(vars(%s))", grid_row_var))
    }
    
    
    
    
    
    ########## 3.3.7 Set Themes ##########  
    # Theme hinzufügen, wenn nicht "Gray"
    if (theme_selected != "Gray") {
      r_code <- paste0(r_code, sprintf(" +\n  %s", paste0("theme_", tolower(theme_selected), "()")))
    }
    
    
    
    
    
    ########## 3.3.8 Set Labs ##########  
    labs_code <- ""
    
    if (!is.null(input$plot_title) && input$plot_title != "") {
      labs_code <- paste0(labs_code, sprintf("title = '%s'", input$plot_title))
    }
    if (!is.null(input$plot_subtitle) && input$plot_subtitle != "") {
      labs_code <- paste0(
        labs_code, 
        if (labs_code != "") ", " else "",
        sprintf("subtitle = '%s'", input$plot_subtitle)
      )
    }
    if (!is.null(input$x_axis_title) && input$x_axis_title != "") {
      labs_code <- paste0(
        labs_code, 
        if (labs_code != "") ", " else "",
        sprintf("x = '%s'", input$x_axis_title)
      )
    }
    if (!is.null(input$y_axis_title) && input$y_axis_title != "") {
      labs_code <- paste0(
        labs_code, 
        if (labs_code != "") ", " else "",
        sprintf("y = '%s'", input$y_axis_title)
      )
    }
    if (!is.null(input$legend_title) && input$legend_title != "") {
      labs_code <- paste0(
        labs_code, 
        if (labs_code != "") ", " else "",
        sprintf("fill = '%s'", input$legend_title)
      )
    }
    
    # Wenn Labels vorhanden sind, füge `labs()` in den R-Code ein
    if (labs_code != "") {
      r_code <- paste0(r_code, sprintf(" +\n  labs(%s)", labs_code))
    }
    
    
    
    
    
    ########## 3.3.9 X-Axis Range ##########  
    # X-Achsen-Bereich anpassen
    if (!is.na(x_axis_min) || !is.na(x_axis_max)) {
      p <- p + scale_x_continuous(limits = c(if (!is.na(x_axis_min)) x_axis_min else -Inf, 
                                             if (!is.na(x_axis_max)) x_axis_max else Inf))
    }
    if (!is.na(x_axis_min) || !is.na(x_axis_max)) {
      r_code <- paste0(r_code, sprintf(" +\n  scale_x_continuous(limits = c(%s, %s))", 
                                       if (!is.na(x_axis_min)) x_axis_min else "NA", 
                                       if (!is.na(x_axis_max)) x_axis_max else "NA"))
    }
    
    
    
    
    
    ########## 3.3.10 Y-Axis Range ##########  
    if (!is.na(y_axis_min) || !is.na(y_axis_max)) {
      p <- p + scale_y_continuous(limits = c(if (!is.na(y_axis_min)) y_axis_min else -Inf, 
                                             if (!is.na(y_axis_max)) y_axis_max else Inf))
    }
    if (!is.na(y_axis_min) || !is.na(y_axis_max)) {
      r_code <- paste0(r_code, sprintf(" +\n  scale_y_continuous(limits = c(%s, %s))", 
                                       if (!is.na(y_axis_min)) y_axis_min else "NA", 
                                       if (!is.na(y_axis_max)) y_axis_max else "NA"))
    }
    
    
    
    
    
    
    
    
    
    ############### 3.4 Generate Theme-Code ###############
    # Create Variable for Theme Code
    theme_code <- ""
    
    
    
    
    
    ########## 3.4.1 Title ##########
    if (input$Title_Font != "Gemäss Theme" || input$Title_Face != "Gemäss Theme" ||
        input$Title_Color != "" || !is.na(input$Title_Size) || input$Title_Alignment != "Gemäss Theme") {
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      theme_code <- paste0(theme_code, "plot.title = element_text(")
      
      
      theme_code <- paste0(theme_code,
                           if (input$Title_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                           switch(input$Title_Font,
                                                                                  "Sans Serife" = "sans",
                                                                                  "Serife" = "serif",
                                                                                  "Monospace" = "mono")) else "",
                           if (input$Title_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                           switch(input$Title_Face,
                                                                                  "Normal" = "plain",
                                                                                  "Fett" = "bold",
                                                                                  "Kursiv" = "italic",
                                                                                  "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Title_Size)) sprintf("size = %.1f, ", input$Title_Size) else "",
                           if (input$Title_Color != "") sprintf("colour = '%s', ", input$Title_Color) else "",
                           if (input$Title_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                switch(input$Title_Alignment,
                                                                                       "Linksbündig" = 0,
                                                                                       "Mittig" = 0.5,
                                                                                       "Rechtsbündig" = 1)) else "")
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.1 Subtitle ##########
    if (input$Subtitle_Font != "Gemäss Theme" || input$Subtitle_Face != "Gemäss Theme" ||
        input$Subtitle_Color != "" || !is.na(input$Subtitle_Size) || input$Subtitle_Alignment != "Gemäss Theme") {
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      theme_code <- paste0(theme_code, "plot.subtitle = element_text(")
      
      theme_code <- paste0(theme_code,
                           if (input$Subtitle_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                              switch(input$Subtitle_Font,
                                                                                     "Sans Serife" = "sans",
                                                                                     "Serife" = "serif",
                                                                                     "Monospace" = "mono")) else "",
                           if (input$Subtitle_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                              switch(input$Subtitle_Face,
                                                                                     "Normal" = "plain",
                                                                                     "Fett" = "bold",
                                                                                     "Kursiv" = "italic",
                                                                                     "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Subtitle_Size)) sprintf("size = %.1f, ", input$Subtitle_Size) else "",
                           if (input$Subtitle_Color != "") sprintf("colour = '%s', ", input$Subtitle_Color) else "",
                           if (input$Subtitle_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                   switch(input$Subtitle_Alignment,
                                                                                          "Linksbündig" = 0,
                                                                                          "Mittig" = 0.5,
                                                                                          "Rechtsbündig" = 1)) else "")
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.3 X-Axis Title ##########
    if (input$X_Axis_Title_Font != "Gemäss Theme" || input$X_Axis_Title_Face != "Gemäss Theme" ||
        input$X_Axis_Title_Color != "" || !is.na(input$X_Axis_Title_Size) || input$X_Axis_Title_Alignment != "Gemäss Theme") {
      
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      theme_code <- paste0(theme_code, "axis.title.x = element_text(")
      
      theme_code <- paste0(theme_code,
                           if (input$X_Axis_Title_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                                  switch(input$X_Axis_Title_Font,
                                                                                         "Sans Serife" = "sans",
                                                                                         "Serife" = "serif",
                                                                                         "Monospace" = "mono")) else "",
                           if (input$X_Axis_Title_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                                  switch(input$X_Axis_Title_Face,
                                                                                         "Normal" = "plain",
                                                                                         "Fett" = "bold",
                                                                                         "Kursiv" = "italic",
                                                                                         "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$X_Axis_Title_Size)) sprintf("size = %.1f, ", input$X_Axis_Title_Size) else "",
                           if (input$X_Axis_Title_Color != "") sprintf("colour = '%s', ", input$X_Axis_Title_Color) else "",
                           if (input$X_Axis_Title_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                       switch(input$X_Axis_Title_Alignment,
                                                                                              "Linksbündig" = 0,
                                                                                              "Mittig" = 0.5,
                                                                                              "Rechtsbündig" = 1)) else "")
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.4 Y-Axis Title ##########
    if (input$Y_Axis_Title_Font != "Gemäss Theme" || input$Y_Axis_Title_Face != "Gemäss Theme" ||
        input$Y_Axis_Title_Color != "" || !is.na(input$Y_Axis_Title_Size) || input$Y_Axis_Title_Alignment != "Gemäss Theme") {
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      theme_code <- paste0(theme_code, "axis.title.y = element_text(")
      
      theme_code <- paste0(theme_code,
                           if (input$Y_Axis_Title_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                                  switch(input$Y_Axis_Title_Font,
                                                                                         "Sans Serife" = "sans",
                                                                                         "Serife" = "serif",
                                                                                         "Monospace" = "mono")) else "",
                           if (input$Y_Axis_Title_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                                  switch(input$Y_Axis_Title_Face,
                                                                                         "Normal" = "plain",
                                                                                         "Fett" = "bold",
                                                                                         "Kursiv" = "italic",
                                                                                         "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Y_Axis_Title_Size)) sprintf("size = %.1f, ", input$Y_Axis_Title_Size) else "",
                           if (input$Y_Axis_Title_Color != "") sprintf("colour = '%s', ", input$Y_Axis_Title_Color) else "",
                           if (input$Y_Axis_Title_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                       switch(input$Y_Axis_Title_Alignment,
                                                                                              "Linksbündig" = 0,
                                                                                              "Mittig" = 0.5,
                                                                                              "Rechtsbündig" = 1)) else "")
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.1 Legend-Title ##########
    if (input$Legend_Title_Font != "Gemäss Theme" || input$Legend_Title_Face != "Gemäss Theme" ||
        input$Legend_Title_Color != "" || !is.na(input$Legend_Title_Size) || input$Legend_Title_Alignment != "Gemäss Theme") {
      
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      theme_code <- paste0(theme_code, "legend.title = element_text(")
      
      
      theme_code <- paste0(theme_code,
                           if (input$Legend_Title_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                                  switch(input$Legend_Title_Font,
                                                                                         "Sans Serife" = "sans",
                                                                                         "Serife" = "serif",
                                                                                         "Monospace" = "mono")) else "",
                           if (input$Legend_Title_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                                  switch(input$Legend_Title_Face,
                                                                                         "Normal" = "plain",
                                                                                         "Fett" = "bold",
                                                                                         "Kursiv" = "italic",
                                                                                         "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Legend_Title_Size)) sprintf("size = %.1f, ", input$Legend_Title_Size) else "",
                           if (input$Legend_Title_Color != "") sprintf("colour = '%s', ", input$Legend_Title_Color) else "",
                           if (input$Legend_Title_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                       switch(input$Legend_Title_Alignment,
                                                                                              "Linksbündig" = 0,
                                                                                              "Mittig" = 0.5,
                                                                                              "Rechtsbündig" = 1)) else "")
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.6 Legend-Text ##########
    if (input$Legend_Text_Font != "Gemäss Theme" || input$Legend_Text_Face != "Gemäss Theme" ||
        input$Legend_Text_Color != "" || !is.na(input$Legend_Text_Size) || input$Legend_Text_Alignment != "Gemäss Theme") {
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      theme_code <- paste0(theme_code, "legend.text = element_text(")
      
      
      theme_code <- paste0(theme_code,
                           if (input$Legend_Text_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                                 switch(input$Legend_Text_Font,
                                                                                        "Sans Serife" = "sans",
                                                                                        "Serife" = "serif",
                                                                                        "Monospace" = "mono")) else "",
                           if (input$Legend_Text_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                                 switch(input$Legend_Text_Face,
                                                                                        "Normal" = "plain",
                                                                                        "Fett" = "bold",
                                                                                        "Kursiv" = "italic",
                                                                                        "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Legend_Text_Size)) sprintf("size = %.1f, ", input$Legend_Text_Size) else "",
                           if (input$Legend_Text_Color != "") sprintf("colour = '%s', ", input$Legend_Text_Color) else "",
                           if (input$Legend_Text_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                      switch(input$Legend_Text_Alignment,
                                                                                             "Linksbündig" = 0,
                                                                                             "Mittig" = 0.5,
                                                                                             "Rechtsbündig" = 1)) else "")
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    

    
    ########## 3.4.7 X-Axis-Labels ##########
    if (input$Axis_X_Text_Font != "Gemäss Theme" || input$Axis_X_Text_Face != "Gemäss Theme" ||
        input$Axis_X_Text_Color != "" || !is.na(input$Axis_X_Text_Size) || input$Axis_X_Text_H_Alignment != "Gemäss Theme"
        || input$Axis_X_Text_V_Alignment != "Gemäss Theme" || !is.na(input$Axis_X_Text_Rotation)) {
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }

      
      
      theme_code <- paste0(theme_code, "axis.text.x = element_text(")
      
      
      theme_code <- paste0(theme_code,
                           if (input$Axis_X_Text_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                                 switch(input$Axis_X_Text_Font,
                                                                                        "Sans Serife" = "sans",
                                                                                        "Serife" = "serif",
                                                                                        "Monospace" = "mono")) else "",
                           if (input$Axis_X_Text_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                                 switch(input$Axis_X_Text_Face,
                                                                                        "Normal" = "plain",
                                                                                        "Fett" = "bold",
                                                                                        "Kursiv" = "italic",
                                                                                        "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Axis_X_Text_Size)) sprintf("size = %.1f, ", input$Axis_X_Text_Size) else "",
                           if (input$Axis_X_Text_Color != "") sprintf("colour = '%s', ", input$Axis_X_Text_Color) else "",
                           if (input$Axis_X_Text_H_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                        switch(input$Axis_X_Text_H_Alignment,
                                                                                               "Linksbündig" = 0,
                                                                                               "Mittig" = 0.5,
                                                                                               "Rechtsbündig" = 1)) else "",
                           if (input$Axis_X_Text_V_Alignment != "Gemäss Theme") sprintf("vjust = %s, ", 
                                                                                        switch(input$Axis_X_Text_V_Alignment,
                                                                                               "Unten" = 0,
                                                                                               "Mittig" = 0.5,
                                                                                               "Oben" = 1)) else "",
                           if (!is.na(input$Axis_X_Text_Rotation)) sprintf("angle = %.0f, ", input$Axis_X_Text_Rotation) else ""
      )
      
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.7 Y-Axis-Labels ##########
    if (input$Axis_Y_Text_Font != "Gemäss Theme" || input$Axis_Y_Text_Face != "Gemäss Theme" ||
        input$Axis_Y_Text_Color != "" || !is.na(input$Axis_Y_Text_Size) || input$Axis_Y_Text_H_Alignment != "Gemäss Theme"
        || input$Axis_Y_Text_V_Alignment != "Gemäss Theme" || !is.na(input$Axis_Y_Text_Rotation)) {
      
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n  ")
      }
      
      
      
      theme_code <- paste0(theme_code, "axis.text.y = element_text(")
      
      
      theme_code <- paste0(theme_code,
                           if (input$Axis_Y_Text_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                                 switch(input$Axis_Y_Text_Font,
                                                                                        "Sans Serife" = "sans",
                                                                                        "Serife" = "serif",
                                                                                        "Monospace" = "mono")) else "",
                           if (input$Axis_Y_Text_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                                 switch(input$Axis_Y_Text_Face,
                                                                                        "Normal" = "plain",
                                                                                        "Fett" = "bold",
                                                                                        "Kursiv" = "italic",
                                                                                        "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Axis_Y_Text_Size)) sprintf("size = %.1f, ", input$Axis_Y_Text_Size) else "",
                           if (input$Axis_Y_Text_Color != "") sprintf("colour = '%s', ", input$Axis_Y_Text_Color) else "",
                           if (input$Axis_Y_Text_H_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                        switch(input$Axis_Y_Text_H_Alignment,
                                                                                               "Linksbündig" = 0,
                                                                                               "Mittig" = 0.5,
                                                                                               "Rechtsbündig" = 1)) else "",
                           if (input$Axis_Y_Text_V_Alignment != "Gemäss Theme") sprintf("vjust = %s, ", 
                                                                                        switch(input$Axis_Y_Text_V_Alignment,
                                                                                               "Unten" = 0,
                                                                                               "Mittig" = 0.5,
                                                                                               "Oben" = 1)) else "",
                           if (!is.na(input$Axis_Y_Text_Rotation)) sprintf("angle = %.0f, ", input$Axis_Y_Text_Rotation) else ""
      )
      
      
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    ########## 3.4.20 Adjust Theme C ##########
    if (theme_code!=""){
      theme_code <- paste0(" +\n  theme(", sprintf(theme_code), ")")
      r_code <- paste0(r_code, sprintf(theme_code))
    }
    
    
    
    
    
    
    ########## 3.5 Return R-Code ##########
    r_code
  }
  )  
  
  
  
  
  
  
  
  
  ############### 3.6 Generate Plot ###############
  output$plot <- renderPlot({
    req(data())
    req(r_code())
    
    # Code ausführen und 'q' erstellen
    eval(parse(text = r_code()))
    
    # Den erstellten Plot (nicht den String) zurückgeben
    return(q)
  }
  )
  
  
  
  
  
  
  
  
  ############### 3.7 Execute Code ###############
  output$rcode <- renderText({
    req(r_code())
    
    full_code <- paste0("library(ggplot2)\n\n", r_code())
    
    
    full_code
  })
  
}




















#################### 4. Run App ####################
shinyApp(ui = ui, server = server)