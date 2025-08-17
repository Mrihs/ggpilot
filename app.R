#################### 1. Preparation ####################
############### 1.1 Packages ###############
# Create list of required Packages
packages <- c("shiny",
          "ggplot2",
          "readxl",
          "dplyr",
          "shinythemes",
          "shinyBS",
          "Hmisc",
          "ggsci",
          "ggthemes",
          "rclipboard",
          "sortable",
          "svglite",
          "shiny.i18n",
          "shinyWidgets")

# For each Package
for (pkg in packages) {
  # Install package if not installed yet
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  # Load package
  library(pkg, character.only = TRUE)
}










############### 1.2 Create Translator ###############
# Create translator
i18n <- Translator$new(translation_csvs_path = "i18n", translation_csv_config = "www/i18n.config.yaml")
# Set default language
i18n$set_translation_language("en")










############### 1.3 Define Functions ###############
########## 1.3.1 BSCollapse-Function is set using an arrow-down icon ##########
BSCollapseArrow <- function(text, id = NULL, icon_class = "glyphicon-menu-down") {
  HTML(sprintf(
    '<div class="panel-title-container">
       <i class="glyphicon %s"></i>
       <span %s>%s</span>
     </div>',
    icon_class,
    if (is.null(id)) "" else sprintf('id="%s"', id),
    text
  ))
}





########## 1.3.2 is_valid_colo-Function to validate color-inputs ##########
is_valid_color <- function(color) {
  # Empty colors are not invalid
  if (is.null(color) || color == "") return(FALSE)
  tryCatch({
    # Check if valid color
    col2rgb(color)
    TRUE
  }, error = function(e) FALSE)
}





########## 1.3.3 tr-Function to Help Translate ##########
tr <- function(key) {
  # Get the key from the translation filee
  x <- i18n$t(key)
  # Convert the key to characters
  x <- paste(as.character(x), collapse = "")
  # Remove potential HTML-elements
  x <- gsub("<[^>]+>", "", x)
  trimws(x)
}

tr_html <- function(key) {
  x <- i18n$t(key)
  x <- paste(as.character(x), collapse = "")
  HTML(x)
}





########## 1.3.4 Functions to Serialise Dropdown-Localization ##########
font_choices <- function() setNames(
  c("Gemäss Theme","Sans Serife","Serife","Monospace"),
  c(tr("options.font.theme"), tr("options.font.sans"), tr("options.font.serif"), tr("options.font.mono"))
)
face_choices <- function() setNames(
  c("Gemäss Theme","Normal","Fett","Kursiv","Fett & Kursiv"),
  c(tr("face.theme"), tr("face.plain"), tr("face.bold"), tr("face.italic"), tr("face.bolditalic"))
)
align_h_choices <- function() setNames(
  c("Gemäss Theme","Linksbündig","Mittig","Rechtsbündig"),
  c(tr("align.theme"), tr("align.left"), tr("align.center"), tr("align.right"))
)
align_v_choices <- function() setNames(
  c("Gemäss Theme","Unten","Mittig","Oben"),
  c(tr("align.theme"), tr("align.bottom"), tr("align.center"), tr("align.top"))
)
linetype_choices_all <- function() setNames(
  c("Gemäss Theme","Keine","Solide","Gestrichelt","Gepunkted","Punktgestrichelt","Langgestrichen","Doppelt gestrichelt"),
  c(tr("options.linetype.theme"), tr("options.linetype.none"), tr("options.linetype.solid"),
    tr("options.linetype.dashed"), tr("options.linetype.dotted"),
    tr("options.linetype.pointdash"), tr("options.linetype.longdash"),
    tr("options.linetype.twodash"))
)
legend_pos_choices <- function() setNames(
  c("Gemäss Theme","Keine","Rechts","Links","Oben","Unten","Im Plot"),
  c(tr("options.position.theme"), tr("options.position.none"), tr("options.position.right"),
    tr("options.position.left"), tr("options.position.top"),
    tr("options.position.bottom"), tr("options.position.inside"))
)
legend_text_pos_choices <- function() setNames(
  c("Gemäss Theme","Oben","Unten","Links","Rechts"),
  c(tr("options.position.theme"), tr("options.position.top"),
    tr("options.position.bottom"), tr("options.position.left"), tr("options.position.right"))
)
legend_dir_choices <- function() setNames(
  c("Gemäss Theme","Vertikal","Horizontal"),
  c(tr("options.position.alignment.theme"), tr("options.position.alignment.vertical"),
    tr("options.position.alignment.horicontal"))
)















#################### 2. UI ####################
ui <- fluidPage(
  ############### 2.1 General Settings ###############
  ########## 2.1.1 Define Theme ##########
  theme = shinytheme("cerulean"),
  
  
  
  
  
  ########## 2.1.2 Use i18n ##########  
  usei18n(i18n),
  
  
  
  
  
  ########## 2.1.3 Define custom CSS ##########
  tags$head(
    tags$style(HTML("
    
    /* Border of Panels */
    .panel{
    border: 2px solid #dddddd;
    }
    
    /* Background Color of Panels */
    .panel-body {
    background-color: #f5f5f5 !important;
    }

    /* Axis Settings */
    .collapse_panel-settings .col-sm-6 {
    border: 1px solid #ddd;
    padding: 10px;
    margin-bottom: 10px;
    }
    
    /* Font Color of Heading */
    h1, h2, h3, h4, h5, h6 {
    color: #000000 !important;
    }
    
    /* Define Buttons */
    .custom-btn {
    font-size: 18px;
    padding: 5px 15px;
    margin: 20px 5px;
    color: black;
    border: 5px solid #bbb;
    border-radius: 5px;
    outline: none;
    border-style: double;
    }
    
    /* Define Buttons when Hovered */
    .custom-btn:hover {
    background-color: #f0f0f0;
    }

    /* Define Buttons when Active */
    .active-btn {
    background-color: #e0e0e0;
    color: black;
    background-image: none;
    }

    /* Define Button for Plots */
    .plot-btn {
    font-size: 30px;
    padding: 5px 30px;
    margin: 20px 10px;
    color: black;
    border: 5px solid #bbb;
    border-radius: 5px;
    outline: none;
    border-style: double;
    }

    /* Define Plot-Buttons when Selected/active */
    .active-plot {
    background-color: #e0e0e0;
    color: black;
    background-image: none;
    }  
    
    /* Body Space for footer */
    body { padding-bottom: 0px; }

    /* Fixed footer */
    footer.app-footer {
      position: static;
      left: 0;
      bottom: 0;
      width: 100%;
      background: #ffffff;
      border-top: 1px solid #ddd;
      padding: 6px 12px;
      z-index: 1000;
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-top: 48px;
    }
    .app-footer .bootstrap-select { 
      margin: 0 !important; 
    }
    .app-footer .bootstrap-select > .dropdown-toggle {
      padding: 6px 12px;        /* gleiche vertikale Padding wie GitHub-Button */
      height: 36px;             /* gleiche Höhe wie .github-btn */
      background: #fff;
      border: 1px solid #ddd;
      display: inline-flex;     /* vertikal zentrieren */
      align-items: center;      /* vertikal zentrieren */
      line-height: 1;           /* vermeidet Verschiebung nach oben */
    }
    .app-footer .bootstrap-select .filter-option,
    .app-footer .bootstrap-select .filter-option-inner-inner {
      display: flex;
      align-items: center;
    }
    .app-footer .bootstrap-select .caret {
      margin-top: 0 !important;
      align-self: center;
    }
    
    /* Language switcher */
    .lang-switch {
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .lang-btn {
      width: 24px;
      height: 24px;
      cursor: pointer;
      opacity: 0.6;
      transition: opacity .15s ease, transform .05s ease;
      user-select: none;
    }
    .lang-btn:hover { opacity: 0.85; }
    .lang-btn.active {
      opacity: 1;
      outline: 2px solid #bbb;
      border-radius: 50%;
    }
    
    /* Language-Icons */
    .lang-icon { width: 20px; height: 20px; vertical-align: middle; }

    /* GitHub-Button */
    .app-footer .github-btn {
      display: inline-flex;
      align-items: center;
      gap: 8px;
      padding: 6px 12px;
      background: #fff;
      border: 1px solid #ddd;
      border-radius: 6px;
      text-decoration: none;
      color: #000;
      transition: background-color .15s ease, box-shadow .15s ease;
    }
    .app-footer .github-btn:hover {
      background: #f5f5f5;
      text-decoration: none;
      box-shadow: 0 1px 3px rgba(0,0,0,.08);
    }
    .app-footer .github-btn .fa {
      font-size: 18px;   /* Font Awesome Icon-Größe */
      line-height: 1;
    }
    
    .app-footer .shiny-input-container,
    .app-footer .form-group {
      margin: 0 !important;
      margin-bottom: 0 !important;
    }
    .app-footer .shiny-input-container {
      display: flex;
      align-items: center;
    }
    .app-footer .bootstrap-select > .dropdown-toggle {
      display: inline-flex;
      align-items: center;
      height: 36px;
      padding: 6px 12px;
      line-height: 1;
      box-sizing: border-box;
    }
    .app-footer .bootstrap-select .caret {
      margin-top: 0 !important;
    }
    
    /* Right Side of Footer*/
    .right-tools {
      display: flex;
      align-items: center;
      gap: 8px;
    }
    
    /* Info-Button */
    .app-footer .info-btn,
    .app-footer .info-btn:visited,
    .app-footer .info-btn:active {
      width: 36px;
      height: 36px;
      border: 1px solid #ddd;
      border-radius: 4px;
      background: #fff;
      color: #000 !important;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      text-decoration: none;
      cursor: pointer;
      transition: background-color .15s ease, box-shadow .15s ease;
    }
    
    .app-footer .info-btn .fa {
      font-size: 18px;
      line-height: 1;
      color: #000;
    }
    
    .app-footer .info-btn:hover {
      background: #f5f5f5;
      box-shadow: 0 1px 3px rgba(0,0,0,.08);
      color: #000 !important;
      text-decoration: none;
    }
    
    .modal-title .info-modal-title { 
      font-size: 28px; 
      line-height: 1.2;
      font-weight: 600;
    }
        
  "))
  ),
  
  
  
  
  
  
  
  
  
  
  ############### 2.2 Set Title-Panel ###############
  # Define a fluid Row
  fluidRow(
    # Set a Column
    column(3, style = "min-width: 350px;",
           # Define logo and title
           titlePanel(title = span(
             # Add image
             img(src = "logo.png", 
                 height = 90, 
                 style = "position: relative; top: -15px; margin-left: 15px;"
                 ), 
             # Define HTML
             HTML('<span style="font-size: 64px;; margin-left: 20px;">ggpilot</span>'),
             style = "white-space: nowrap;" # kein Zeilenumbruch
             ),
             windowTitle = "ggpilot")),
    # Set a Column
    column(8, align = "center", 
           style = "margin-top: 15px;",
           tags$head(
           ),
           tags$script(HTML("
            Shiny.addCustomMessageHandler('setActiveButton', function(btnId) {
              $('.custom-btn').removeClass('active-btn');
              $('#' + btnId).addClass('active-btn');
            });
         ")),
           # Add action buttons in the title panel
           actionButton("btn_data", label = HTML('<i class="glyphicon glyphicon-folder-open"></i>'), class = "custom-btn"),
           actionButton("btn_plottype", label = HTML('<i class="glyphicon glyphicon-stats"></i>'), class = "custom-btn"),
           actionButton("btn_variables", label = HTML('<i class="glyphicon glyphicon-tasks"></i>'), class = "custom-btn"),
           actionButton("btn_plot_options", label = HTML('<i class="glyphicon glyphicon-wrench"></i>'), class = "custom-btn"),
           actionButton("btn_text", label = HTML('<i class="glyphicon glyphicon-font"></i>'), class = "custom-btn"),
           actionButton("btn_layout", label = HTML('<i class="glyphicon glyphicon-adjust"></i>'), class = "custom-btn"),
           actionButton("btn_download", label = HTML('<i class="glyphicon glyphicon-download"></i>'), class = "custom-btn"),
    )
  ),
  
  # Define Hidden Inputs for the Active Tab
  tags$div(
    textInput("activeTab", label = NULL, value = "data"),
    # Set the Input to be hidden
    style = "display: none;"
  ),  
  
  
  
  
  
  
  
  
  
  
  ############### 2.3 Set Sidebar ###############
  # Define Sidebar-Layout
  sidebarLayout(
    
    # Define Panel in Sidebar with width of 4
    sidebarPanel(width = 4,
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ############### 2.4 Input Fields ###############
                 # Define Conditional-Panel for when data tab is selected
                 conditionalPanel(condition = "input.activeTab == 'data'",
                                  ########## 2.4.1 UI to Select Data ########## 
                                  # Add UI-Object for the Data Input page
                                  tags$head(
                                    tags$script(HTML("
                                    Shiny.addCustomMessageHandler('setFileInputLang', function(x){
                                      var root = $('#file').closest('.shiny-input-container');
                                      // Label über dem Feld
                                      $('#file_label').text(x.label);
                                      // Button-Text (robusteste Variante: Textknoten im Button ersetzen)
                                      var btn = root.find('label.btn');
                                      if(btn.length){
                                        var tn = btn.contents().filter(function(){return this.nodeType === 3;}).first();
                                        if(tn.length){ tn[0].nodeValue = ' ' + x.browse + ' '; }
                                      }
                                      // Placeholder (readonly Textfeld)
                                      root.find('input[type=\"text\"][readonly]').attr('placeholder', x.placeholder);
                                    });
                                  "))
                                  ),
                                  
                                  conditionalPanel(condition = "input.activeTab == 'data'",
                                                   tags$label(id = "file_label", class = "control-label", tr("data.select_dataset")),
                                                   fileInput(
                                                     "file",
                                                     label       = NULL,                                 # Label kommt als separater <label> drüber
                                                     buttonLabel = tr("data.browse"),                    # initiale Sprache
                                                     placeholder = tr("data.no_file"),                   # initiale Sprache
                                                     accept      = c(".csv", ".xlsx", ".rds", ".RData")
                                                   )
                                  )                                  ),
                                  
                 
                 
                 
                 
                 
                 ########## 2.4.2 UI for Plot-Type ##########
                 # Define HTML-Script for handling Plot-Types
                 tags$script(HTML("
                                  Shiny.addCustomMessageHandler('setActivePlot', function(btnId) {
                                  $('.plot-btn').removeClass('active-plot');
                                  $('#' + btnId).addClass('active-plot');
                                  });
                                  ")),                 
                 
                 # Define Conditional-Panel for when plottype tab is selected
                 conditionalPanel(condition = "input.activeTab == 'plottype'",
                                  actionButton("plot_bar", 
                                               label = HTML(sprintf('<img src="Icon_Bar.png" height="100px"> <br> %s', tr("plot.bar"))), 
                                               class = "plot-btn"),
                                  actionButton("plot_line", 
                                               label = HTML(sprintf('<img src="Icon_Line.png" height="100px"> <br> %s', tr("plot.line"))), 
                                               class = "plot-btn"),
                                  actionButton("plot_box", 
                                               label = HTML(sprintf('<img src="Icon_Box.png" height="100px"> <br> %s', tr("plot.box"))), 
                                               class = "plot-btn"),
                                  actionButton("plot_scatter", 
                                               label = HTML(sprintf('<img src="Icon_Scatter.png" height="100px"> <br> %s', tr("plot.scatter"))), 
                                               class = "plot-btn")
                                  ),
                 
                 
                 
                 
                 
                 ########## 2.4.3 UI to Select Variables ##########
                 tags$script(HTML("
                                   Shiny.addCustomMessageHandler('setText', function(x) {
                                   var el = document.getElementById(x.id);
                                   if (el) { el.textContent = x.text; }
                                   });
                                  ")),

                 # Define Conditional-Panel for when Variables tab is selected
                 conditionalPanel(condition = "input.activeTab == 'variables'",
                                  # Set title
                                  h3(span(id = "x_title", tr("variables.x_title"))),
                                  # X-xis Variable
                                  selectInput("x_var", NULL, choices = c(""), selected = ""),
                                  # Create a conditionl-panel for when a variable is selected
                                  conditionalPanel(condition = "output.is_numeric_x == false",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("variables.reorder_levels"), id = "x_reorder_title"),
                                                                # Placeholder for the ranking-UI
                                                                uiOutput("x_factor_rank_list")
                                                              )
                                                   )
                                  ),
                                  # Set title
                                  h3(span(id = "y_title", tr("variables.y_title"))),
                                  # Y-Axis Variable
                                  selectInput("y_var", NULL, choices = c(""), selected = ""),
                                  # Create a conditionl-panel for when a variable is selected
                                  conditionalPanel(condition = "output.is_numeric_y == false",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("variables.reorder_levels"), id = "y_reorder_title"),
                                                                # Placeholder for the ranking-UI
                                                                uiOutput("y_factor_rank_list")
                                                              )
                                                   )
                                  ),
                                  # Set title
                                  h3(span(id = "group_title", tr("variables.group_title"))),
                                  # Grouping Variable
                                  selectInput("group_var", NULL, choices = c(""), selected = ""),
                                  # Create a conditionl-panel for when a variable is selected
                                  conditionalPanel(condition =  "output.is_numeric_group == false",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("variables.reorder_levels"), id = "group_reorder_title"),
                                                                # Placeholder for the ranking-UI
                                                                uiOutput("group_factor_rank_list")
                                                              )
                                                   )
                                  ),
                                  # Set title
                                  h3(span(id = "grid_col_title", tr("variables.grid_col_title"))),
                                  # Facet Grid - Columns
                                  selectInput("grid_col_var", NULL, choices = c(""), selected = ""),
                                  # Create a conditionl-panel for when a variable is selected
                                  conditionalPanel(condition =  "output.is_numeric_col == false",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("variables.reorder_levels"), id = "grid_col_reorder_title"),
                                                                # Placeholder for the ranking-UI
                                                                uiOutput("grid_col_factor_rank_list")
                                                              )
                                                   )
                                  ),
                                  # Set title
                                  h3(span(id = "grid_row_title", tr("variables.grid_row_title"))),
                                  # Facet Grid - Rows
                                  selectInput("grid_row_var", NULL, choices = c(""), selected = ""),
                                  # Create a conditionl-panel for when a variable is selected
                                  conditionalPanel(condition =  "output.is_numeric_row == false",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("variables.reorder_levels"), id = "grid_row_reorder_title"),
                                                                # Placeholder for the ranking-UI
                                                                uiOutput("grid_row_factor_rank_list")
                                                              )
                                                   )
                                  )
                 ),
                 
                 
                 
                 
                 
                 ########## 2.4.4 UI for Plot Options ##########
                 # Define Conditional-Panel for when Plot-Options tab is selected
                 conditionalPanel(condition = "input.activeTab == 'plot_options'",
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             # Create a Collapse-Panel for Theme-Settings
                                             bsCollapsePanel(
                                               # Define Title of Collapse-Panel
                                               title = BSCollapseArrow(tr("options.theme.title"), 
                                                                       id = "opt_theme_title"),
                                               # Define CSS Settings
                                               div(class = ".collapse_panel-settings",
                                                   # Create Input-Options for themes
                                                   selectInput(inputId = "plot_theme", label = "", 
                                                               choices = c("Bw", "Classic", "Gray", "Linedraw", "Light", "Dark", 
                                                                           "Minimal", "Void", "Calc", "the Economist", 
                                                                           "the Economist White", "Excel", "Few", "FiveThirtyEight", 
                                                                           "Google Docs", "Highcharts JS", "Inversed Gray", 
                                                                           "Solarized", "Solarized 2", "Solid", "Stata", "Tufte", 
                                                                           "Wall Street Journal"),
                                                               selected = "Gray")
                                               )
                                             )
                                  ),
                                  conditionalPanel(condition = "output.show_grouping_options",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Color-Palette-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("options.palette.title"), 
                                                                                        id = "opt_palette_title"),
                                                                # Define CSS Settings
                                                                div(class = ".collapse_panel-settings",
                                                                    # Dropdown with options for color-palette
                                                                    selectInput(inputId = "Color_Palette", 
                                                                                label = tr("options.palette.select"), 
                                                                                choices = c("Gemäss Theme", "Eigene Farbpalette erstellen", "viridis", "viridis - magma", "viridis - plasma", "viridis - inferno",
                                                                                            "viridis - cividis", "viridis - mako", "viridis - rocket", "viridis - turbo",
                                                                                            "Accent", "Blues", "Greens", "Greys", "Oranges", "Paired", "Pastel1", 
                                                                                            "Pastel2", "Purples", "Reds", "Set1", "Set2", "Set3", "Spectral", 
                                                                                            "grey", "hue", "ordinal", "aas", "bmj", "cosmic", "d3", "flatui", 
                                                                                            "frontiers", "futurama", "igv", "jama", "lancet", "locuszoom", 
                                                                                            "material", "nejm", "npg", "observable", "rickandmorty", "simpsons", "startrek", 
                                                                                            "tron", "uchicago", "ucscgb", "jco", "calc", "canva", "colorblind", 
                                                                                            "economist", "excel", "excel_new", "few", "fivethirtyeight", "gdocs", 
                                                                                            "hc", "pander", "ptol", "solarized", 
                                                                                            "stata", "tableau", "wsj"),
                                                                                selected = "Gemäss Theme"),
                                                                    # Set UI for individual color palette
                                                                    conditionalPanel(condition = "input.Color_Palette =='Eigene Farbpalette erstellen'",
                                                                                     # Add Button
                                                                                     actionButton("add", tr("options.custom.colorpalette.add")),
                                                                                     actionButton("remove_last", tr("options.custom.colorpalette.remove")),
                                                                                     tags$div(id = "input_container"),
                                                                                     # Create CSS for inavlid colors
                                                                                     tags$style(HTML("
                                                                      .invalid { background-color: #ffcccc !important; }
                                                                      .error-message { color: red; font-size: 14px; margin-left: 10px; display: inline; }
                                                                    ")),
                                                                                     # JS to validate colors
                                                                                     tags$script(HTML("
                                                                      Shiny.addCustomMessageHandler('validColor', function(data) {
                                                                        var inputField = document.getElementById(data.id);
                                                                        var errorText = document.getElementById(data.id + '_error');
                                                                        if (data.valid) {
                                                                          inputField.classList.remove('invalid');
                                                                          if (errorText) errorText.style.display = 'none';
                                                                        } else {
                                                                          inputField.classList.add('invalid');
                                                                          if (errorText) errorText.style.display = 'inline';
                                                                        }
                                                                      });
                                                                    "))
                                                                    ),
                                                                    selectInput(inputId = "color_palette_target", 
                                                                                label = tr("options.palette.apply_to"),
                                                                                choices = setNames(c("Füllung", "Linien", "Füllung und Linien"),
                                                                                                   c(tr("options.palette.target.fill"),tr("options.palette.target.line"),tr("options.palette.target.both"))),
                                                                                selected = "Füllung")
                                                                )
                                                              )
                                                   )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             # Create a Collapse-Panel for Plot-Size
                                             bsCollapsePanel(
                                               # Define Title of Collapse-Panel
                                               title = BSCollapseArrow(tr("options.plotsize.title"), 
                                                                       id = "options_plotsize_title"),
                                               # Define CSS Settings
                                               div(class = ".collapse_panel-settings",
                                                   column(6,
                                                          # Define Plot-Width
                                                          numericInput(inputId = "plot_width_px", 
                                                                       label = tr("options.plotsize.width_px"),
                                                                       value = 800, min = 100),
                                                   ),
                                                   column(6,
                                                          # Define Plot-Height
                                                          numericInput(inputId = "plot_height_px", 
                                                                       label = tr("options.plotsize.height_px"),
                                                                       value = 600, min = 100),
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             # Create a Collapse-Panel for Axis-Range
                                             bsCollapsePanel(
                                               # Define Title of Collapse-Panel
                                               title = BSCollapseArrow(tr("options.range.title"), 
                                                                       id = "opt_range_title"),
                                               # Define CSS Settings
                                               div(class = ".collapse_panel-settings",
                                                   # Set a HTML header for the Y-Axis Range Text
                                                   HTML(sprintf('<label class="control-label"><span id="opt_xaxis_lbl">%s</span></label>', tr("options.range.xaxis"))),
                                                   # Define the min and max value next to each other
                                                   div(
                                                     # Define style
                                                     style = "display: flex; justify-content: space-between; gap: 10px;",
                                                     div(
                                                       style = "flex: 1;",
                                                       # Numeric Input field for the minimal X-Axis value
                                                       div(style="flex:1;", 
                                                           numericInput("x_axis_min", 
                                                                        label = tr("options.range.min"), 
                                                                        step = 0.1, value = "")),
                                                     ),
                                                     div(
                                                       style = "flex: 1;",
                                                       # Numeric Input field for the max X-Axis value
                                                       div(style="flex:1;",
                                                           numericInput("x_axis_max",
                                                                        label = tr("options.range.max"),
                                                                        step = 0.1, value = ""))
                                                     )
                                                   ),
                                                   # Set a HTML header for the X-Axis Range Text
                                                   HTML(sprintf('<label class="control-label"><span id="opt_yaxis_lbl">%s</span></label>', tr("options.range.yaxis"))),
                                                   # Define the min and max value next to each other
                                                   div(
                                                     # Define style
                                                     style = "display: flex; justify-content: space-between; gap: 10px;",
                                                     div(
                                                       style = "flex: 1;",
                                                       # Numeric Input field for the minimal Y-Axis value
                                                       div(style="flex:1;", 
                                                           numericInput("y_axis_min", 
                                                                        label = tr("options.range.min"), 
                                                                        step = 0.1, value = "")),
                                                     ),
                                                     div(
                                                       style = "flex: 1;",
                                                       # Numeric Input field for the max Y-Axis value
                                                       div(style="flex:1;", 
                                                           numericInput("y_axis_max", 
                                                                        label = tr("options.range.max"), 
                                                                        step = 0.1, value = ""))
                                                     )
                                                   )                                
                                               ),
                                               checkboxInput("exact_axis_range", 
                                                             label = tr("options.range.expand"), 
                                                             value = TRUE)
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  conditionalPanel(condition = "output.show_grouping_options",
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Group-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(text = tr("options.grouping.space"), 
                                                                                        id = "options_grouping_space"),
                                                                # Define CSS Settings
                                                                div(class = ".collapse_panel-settings",
                                                                    # Numeric Input for the position-dodge value
                                                                    numericInput(inputId = "dodge_value", 
                                                                                 label = tr("options.dodge.value"), 
                                                                                 min = 0, max = 2, step = 0.1, value = "")
                                                                )
                                                              )
                                                   )
                                                   
                                  ),
                                  # Create a Layout for CollapsePanels
                                  conditionalPanel(condition = "output.show_barplot_options",
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Errorbar-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("options.barplot"), 
                                                                                        id = "options_barplot"),
                                                                # Define CSS Settings
                                                                div(class = ".collapse_panel-settings",
                                                                    # Numeric Input for the width of the Errorbar
                                                                    numericInput(inputId = "barplot_width", 
                                                                                 tr("options.barplot.width"), 
                                                                                 min = 0, step = 0.1, value = "")
                                                                )
                                                              )
                                                   )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  conditionalPanel(condition = "output.show_linepolot_options",
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Lineplot-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("options.lineplot"), 
                                                                                        id = "options_lineplot"),
                                                                # Define CSS Settings
                                                                div(class = ".collapse_panel-settings",
                                                                    # Select linetype
                                                                    selectInput(inputId = "lineplot_line_type", 
                                                                                label = tr("options.linetype"), 
                                                                                choices = setNames(c("Solide","Gestrichelt","Gepunkted","Punktgestrichelt","Langgestrichen","Doppelt gestrichelt"),
                                                                                                   c(tr("options.linetype.solid"), tr("options.linetype.dashed"), tr("options.linetype.dotted"),
                                                                                                     tr("options.linetype.pointdash"), tr("options.linetype.longdash"), tr("options.linetype.twodash"))),
                                                                                selected = "Solide"),                                                                    
                                                                    # Numeric Input for the width of the Errorbar
                                                                    numericInput(inputId = "lineplot_width", 
                                                                                 label = tr("options.lineplot.width"), 
                                                                                 min = 0, step = 0.1, value = "")                                                                
                                                                    )
                                                              )
                                                   )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  conditionalPanel(condition = "output.show_errorbar_options",
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Errorbar-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("options.errorbar"), 
                                                                                        id = "options_errorbars"),
                                                                # Define CSS Settings
                                                                div(class = ".collapse_panel-settings",
                                                                    # Dropdown to select the type of Errorbar
                                                                    selectInput(inputId = "error_type", 
                                                                                label = tr("options.errorbar.unit"),
                                                                                choices = setNames(c("Keiner","Standardabweichung","Konfidenzintervall","Standardfehler"),
                                                                                                   # Tranlsate the options
                                                                                                   c(tr("options.errorbar.none"),
                                                                                                     tr("options.errorbar.sd"),
                                                                                                     tr("options.errorbar.ci"),
                                                                                                     tr("options.errorbar.se"))),
                                                                                # choices = c("Keiner", "Standardabweichung", "Konfidenzintervall", "Standardfehler"), 
                                                                                selected = "Standardabweichung"),
                                                                    # Numeric Input for the width of the Errorbar
                                                                    numericInput(inputId = "error_mult", 
                                                                                 label = tr("options.errorbar.mult"), 
                                                                                 min = 1, step = 1, value = 1),
                                                                    # Numeric Input for the width of the Errorbar
                                                                    numericInput(inputId = "error_width", 
                                                                                 label = tr("options.errorbar.width"), 
                                                                                 min = 0, step = 0.1, value = ""),
                                                                    # Numeric Input for the width of the Errorbar
                                                                    numericInput(inputId = "error_size", 
                                                                                 label = tr("options.errorbar.size"), 
                                                                                 min = 0, step = 0.1, value = "")
                                                                )
                                                              )
                                                   )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  conditionalPanel(condition = "output.show_scatter_options",
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Scatterplot-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("options.scatterplot"), 
                                                                                        id = "options_scatterplot"),
                                                                # Define CSS Settings
                                                                div(class = ".collapse_panel-settings",
                                                                    # Input to define the size of the Points
                                                                    numericInput(inputId = "scatterpoint_size", 
                                                                                 label = "options.scatterpoint.size", 
                                                                                 min = 0, step = .1, value = ""),
                                                                )
                                                              )
                                                   )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  conditionalPanel(condition = "output.show_scatter_options",
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              # Create a Collapse-Panel for Scatterplot-Settings
                                                              bsCollapsePanel(
                                                                # Define Title of Collapse-Panel
                                                                title = BSCollapseArrow(tr("options.regressionline"), 
                                                                                        id = "options_regressionline"),
                                                                checkboxInput(inputId = "show_line",
                                                                              label = tr("options.regressionline.show"),
                                                                              value = FALSE),
                                                                conditionalPanel(condition = "input.show_line",
                                                                                 checkboxInput(inputId = "scater_line_full_range", 
                                                                                               label = tr("options.regressionline.expand"), 
                                                                                               value = FALSE),
                                                                                 selectInput(inputId = "scater_line_type", 
                                                                                             label = tr("options.linetype"), 
                                                                                             choices = setNames(c("Solide","Gestrichelt","Gepunkted","Punktgestrichelt","Langgestrichen","Doppelt gestrichelt"),
                                                                                                                c(tr("options.linetype.solid"), tr("options.linetype.dashed"), tr("options.linetype.dotted"),
                                                                                                                  tr("options.linetype.pointdash"), tr("options.linetype.longdash"), tr("options.linetype.twodash"))),
                                                                                             selected = "Solide"),
                                                                                 numericInput(inputId = "scater_line_size", 
                                                                                              label = tr("options.linewidth"), 
                                                                                              min = 0, max = 50, step = 0.1, value = NA),
                                                                                 checkboxInput(inputId = "scater_line_show_se", 
                                                                                               label = tr("options.regressionline.showse"), 
                                                                                               value = TRUE)
                                                                )
                                                              )
                                                   )
                                  )
                 ),
                 
                 
                 
                 
                 
                 
                 ########## 2.4.5 UI for Text ##########
                 # Define Conditional-Panel for when text tab is selected
                 conditionalPanel(condition = "input.activeTab == 'text'",
                                  # Text-Input for the Title
                                  textInput(inputId = "plot_title", 
                                            label = tr("text.title"), 
                                            value = "", 
                                            placeholder = tr("text.title_placeholder")),
                                  # Text-Input for the Sub-Title
                                  textInput(inputId = "plot_subtitle", 
                                            label = tr("text.subtitle"), 
                                            value = "", 
                                            placeholder = tr("text.subtitle_placeholder")),
                                  # Text-Input for the X-Axis-Title
                                  textInput(inputId = "x_axis_title", 
                                            label = tr("text.x_axis"), 
                                            value = "", 
                                            placeholder = tr("text.x_axis_placeholder")),
                                  # Text-Input for the Y-Axis-Title
                                  textInput(inputId = "y_axis_title", 
                                            label = tr("text.y_axis"), 
                                            value = "", 
                                            placeholder = tr("text.y_axis_placeholder")),
                                  # Text-Input for the Legend-Title
                                  textInput(inputId = "legend_title", 
                                            label = tr("text.legend"), 
                                            value = "", 
                                            placeholder = tr("text.legend_placeholder"))
                 ),
                 
                 
                 
                 
                 
                 ########## 2.4.6 UI for Layout ##########
                 # Define Conditional-Panel for when layout tab is selected
                 conditionalPanel(condition = "input.activeTab == 'layout'",
                                  # Create a conditionalPanel
                                  conditionalPanel(condition = "output.show_title_options",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              bsCollapsePanel(
                                                                title = BSCollapseArrow(tr("layout.collapse.header"), id = "layout_collapse_header"),
                                                                div(class = ".collapse_panel-settings",
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_title", tr("layout.h3.title"))),
                                                                           # Layout-Options for the Title
                                                                           selectInput(inputId = "Title_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Title_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                                           textInput(inputId = "Title_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                                           numericInput(inputId = "Title_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                                           selectInput(inputId = "Title_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                                    ),
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_subtitle", tr("layout.h3.subtitle"))),
                                                                           # Layout-Options for the Subtitle
                                                                           selectInput(inputId = "Subtitle_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Subtitle_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                                           textInput(inputId = "Subtitle_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                                           numericInput(inputId = "Subtitle_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                                           selectInput(inputId = "Subtitle_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                                    )
                                                                )
                                                              )
                                                              )
                                             ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.axis.title"), id = "layout_collapse_axis_title"),
                                               # title = BSCollapseArrow("Achsen-Überschrift"),
                                               div(class = ".collapse_panel-settings", 
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_title_xaxis", tr("layout.h3.xaxis"))),
                                                          # Text-Input for the X-Axis-Title
                                                          selectInput(inputId = "X_Axis_Title_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                          selectInput(inputId = "X_Axis_Title_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                          textInput(inputId = "X_Axis_Title_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                          numericInput(inputId = "X_Axis_Title_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                          selectInput(inputId = "X_Axis_Title_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                   ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_title_yaxis", tr("layout.h3.yaxis"))),
                                                          # Text-Input for the Y-Axis-Title
                                                          selectInput(inputId = "Y_Axis_Title_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                          selectInput(inputId = "Y_Axis_Title_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                          textInput(inputId = "Y_Axis_Title_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                          numericInput(inputId = "Y_Axis_Title_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                          selectInput(inputId = "Y_Axis_Title_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.axis.text"), id = "layout_collapse_axis_text"),
                                               div(class = ".collapse_panel-settings",
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_xaxis_text", tr("layout.h3.xaxis"))),
                                                          # Text-Input for the X-Axis Label
                                                          selectInput(inputId = "Axis_X_Text_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                          selectInput(inputId = "Axis_X_Text_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                          textInput(inputId = "Axis_X_Text_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                          numericInput(inputId = "Axis_X_Text_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                          numericInput(inputId = "Axis_X_Text_Rotation", label = tr("label.rotation"), min = 0, max = 360, step = 1, value = NA),
                                                          selectInput(inputId = "Axis_X_Text_H_Alignment", label = tr("label.align.h"), choices = align_h_choices(), selected = "Gemäss Theme"),
                                                          selectInput(inputId = "Axis_X_Text_V_Alignment", label = tr("label.align.h"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                   ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_yaxis_text", tr("layout.h3.yaxis"))),
                                                          # Text-Input for the Y-Axis Label
                                                          selectInput(inputId = "Axis_Y_Text_Font", label = "Schriftart", choices = c("Gemäss Theme", "Sans Serife", "Serife", "Monospace"), selected = "Gemäss Theme"),
                                                          selectInput(inputId = "Axis_Y_Text_Face", label = "Formatierung", choices = c("Gemäss Theme", "Normal", "Fett", "Kursiv", "Fett & Kursiv"), selected = "Gemäss Theme"),
                                                          textInput(inputId = "Axis_Y_Text_Color", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                                          numericInput(inputId = "Axis_Y_Text_Size", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                                          numericInput(inputId = "Axis_Y_Text_Rotation", label = tr("label.rotation"), min = 0, max = 360, step = 1, value = NA),
                                                          selectInput(inputId = "Axis_Y_Text_H_Alignment", label = tr("label.align.h"), choices = align_h_choices(), selected = "Gemäss Theme"),
                                                          selectInput(inputId = "Axis_Y_Text_V_Alignment", label = tr("label.align.v"), choices = align_v_choices(), selected = "Gemäss Theme")
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.axis.lines"), id = "layout_collapse_axis_lines"),
                                               div(class = ".collapse_panel-settings",
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_xaxis_lines", tr("layout.h3.xaxis"))),
                                                          selectInput(inputId = "Axis_X_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Axis_X_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Axis_X_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                   ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_yaxis_lines", tr("layout.h3.yaxis"))),
                                                          selectInput(inputId = "Axis_Y_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Axis_Y_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Axis_Y_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.axis.ticks"), id = "layout_collapse_axis_ticks"),
                                               div(class = ".collapse_panel-settings",
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_xaxis_ticks", tr("layout.h3.xaxis"))),
                                                          selectInput(inputId = "Axis_X_Ticks_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Axis_X_Ticks_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Axis_X_Ticks_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color")),
                                                          numericInput(inputId = "Axis_X_Ticks_Length", label = tr("options.linelength"), min = 0, max = 50, step = 0.1, value = NA)
                                                   ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_yaxis_ticks", tr("layout.h3.yaxis"))),
                                                          selectInput(inputId = "Axis_Y_Ticks_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Axis_Y_Ticks_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Axis_Y_Ticks_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color")),
                                                          numericInput(inputId = "Axis_Y_Ticks_Length", label = tr("options.linelength"), min = 0, max = 50, step = 0.1, value = NA)
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.major.grid"), id = "layout_collapse_major_grid"),
                                               div(class = ".collapse_panel-settings",
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_xaxis_major_grid", tr("layout.h3.xaxis"))),
                                                          selectInput(inputId = "Major_Grid_X_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Major_Grid_X_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Major_Grid_X_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                   ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_yaxis_major_grid", tr("layout.h3.yaxis"))),
                                                          selectInput(inputId = "Major_Grid_Y_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Major_Grid_Y_Size", label = "Linien-Grösse", min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Major_Grid_Y_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.minor.grid"), id = "layout_collapse_minor_grid"),
                                               div(class = ".collapse_panel-settings",
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_xaxis_minor_grid", tr("layout.h3.xaxis"))),
                                                          selectInput(inputId = "Minor_Grid_X_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Minor_Grid_X_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Minor_Grid_X_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                          ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_yaxis_minor_grid", tr("layout.h3.yaxis"))),
                                                          selectInput(inputId = "Minor_Grid_Y_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Minor_Grid_Y_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Minor_Grid_Y_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                          )
                                                   )
                                               )
                                             ),
                                  # Create a Layout for CollapsePanels
                                  bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                             bsCollapsePanel(
                                               title = BSCollapseArrow(tr("layout.collapse.background"), id = "layout_collapse_background"),
                                               div(class = ".collapse_panel-settings",
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_plot", tr("layout.h3.plot"))),
                                                          textInput(inputId = "Plot_Background_Color", label = tr("options.background.color"),   value = "", placeholder = tr("placeholder.color")),
                                                          selectInput(inputId = "Plot_Background_Linetype", label = tr("options.linetype"),  choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Plot_Background_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Plot_Background_Line_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                   ),
                                                   # Create a column
                                                   column(6,
                                                          h3(span(id = "layout_h3_panel", tr("layout.h3.panel"))),
                                                          textInput(inputId = "Panel_Background_Color", label = tr("options.background.color"),   value = "", placeholder = tr("placeholder.color")),
                                                          selectInput(inputId = "Panel_Background_Linetype", label = tr("options.linetype"),  choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                          numericInput(inputId = "Panel_Background_Size", label = tr("options.linewidth"), min = 0, max = 50, step = 0.1, value = NA),
                                                          textInput(inputId = "Panel_Background_Line_Color", label = tr("options.linecolor"), value = "", placeholder = tr("placeholder.color"))
                                                   )
                                               )
                                             )
                                  ),
                                  # Create a conditional panel for group-variable settings
                                  conditionalPanel(condition = "output.show_grouping_options",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              bsCollapsePanel(
                                                                title = BSCollapseArrow(tr("layout.collapse.legend"), id = "layout_collapse_legend"),
                                                                div(class = ".collapse_panel-settings",
                                                                    # Crate a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_legend_title", tr("layout.h3.legend.title"))),
                                                                           # Text-Input for the Legend-Title
                                                                           selectInput(inputId = "Legend_Title_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Legend_Title_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                                           textInput(inputId = "Legend_Title_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                                           numericInput(inputId = "Legend_Title_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                                           selectInput(inputId = "Legend_Title_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                                    ),
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_items", tr("layout.h3.items"))),
                                                                           # Text-Input for the X-Axis-Title
                                                                           selectInput(inputId = "Legend_Text_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Legend_Text_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                                           textInput(inputId = "Legend_Text_Color", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                                           numericInput(inputId = "Legend_Text_Size", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                                           selectInput(inputId = "Legend_Text_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                                    )
                                                                )
                                                              )
                                                   ),
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              bsCollapsePanel(
                                                                title = BSCollapseArrow(tr("layout.collapse.legend.background"), id = "layout_collapse_legend_background"),
                                                                div(class = ".collapse_panel-settings",
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_legend_box", tr("layout.h3.legend.box"))),
                                                                           textInput(inputId = "Legend_Background_Color", label = tr("options.background.color"),  value = "", placeholder = tr("placeholder.color")),
                                                                           selectInput(inputId = "Legend_Background_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                                           numericInput(inputId = "Legend_Background_Size", label = tr("options.linewidth"),min = 0, max = 50, step = 0.1, value = NA),
                                                                           textInput(inputId = "Legend_Background_Line_Color", label = tr("options.linecolor"),value = "", placeholder = tr("placeholder.color"))
                                                                    )
                                                                )
                                                              )
                                                   ),
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              bsCollapsePanel(
                                                                title = BSCollapseArrow(tr("layout.collapse.legend.options"), id = "layout_collapse_legend_options"),
                                                                div(class = ".collapse_panel-settings",
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_arrangement", tr("layout.h3.arrangement"))),
                                                                           selectInput(inputId = "Legend_Position", label = tr("layout.legend.position"), choices = legend_pos_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Legend_Title_Position", label = tr("layout.legend.title.position"), choices = legend_text_pos_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Legend_Text_Position", label = tr("layout.legend.text.position"), choices = legend_text_pos_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Legend_Text_Direction", label = tr("layout.legend.direction"),choices = legend_dir_choices(), selected = "Gemäss Theme")
                                                                    ),
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_sizes", tr("layout.h3.sizes"))),
                                                                           numericInput(inputId = "Legend_Key_Width", label = tr("layout.legend.key.width"), min = 0, max = 50, step = 0.1, value = NA),
                                                                           numericInput(inputId = "Legend_Key_Height", label = tr("layout.legend.key.height"), min = 0, max = 50, step = 0.1, value = NA),
                                                                           numericInput(inputId = "Legend_Key_Spacing", label = tr("layout.legend.key.spaicng"), min = 0, max = 50, step = 0.1, value = NA),
                                                                           numericInput(inputId = "Legend_Box_Spacing", label = tr("layout.legend.box.spacing"), min = 0, max = 50, step = 0.1, value = NA)
                                                                    )
                                                                )
                                                              )
                                                   )
                                  ),
                                  # Create a conditional panel for facet-settings
                                  conditionalPanel(condition = "output.show_facet_options",
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              bsCollapsePanel(
                                                                title = BSCollapseArrow(tr("layout.collapse.facets.background"), id = "layout_collapse_facets_background"),
                                                                div(class = ".collapse_panel-settings",
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_columns_facets", tr("layout.h3.columns"))),
                                                                           textInput(inputId = "Stripe_X_Color", label = tr("options.background.color"),  value = "", placeholder = tr("placeholder.color")),
                                                                           selectInput(inputId = "Stripe_X_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                                           numericInput(inputId = "Stripe_X_Size", label = tr("options.linewidth"),min = 0, max = 50, step = 0.1, value = NA),
                                                                           textInput(inputId = "Stripe_X_Line_Color", label = tr("options.linecolor"),value = "", placeholder = tr("placeholder.color"))
                                                                    ),
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_rows_facets", tr("layout.h3.rows"))),
                                                                           textInput(inputId = "Stripe_Y_Color", label = tr("options.background.color"),  value = "", placeholder = tr("placeholder.color")),
                                                                           selectInput(inputId = "Stripe_Y_Linetype", label = tr("options.linetype"), choices = linetype_choices_all(), selected = "Gemäss Theme"),
                                                                           numericInput(inputId = "Stripe_Y_Size", label = tr("options.linewidth"),min = 0, max = 50, step = 0.1, value = NA),
                                                                           textInput(inputId = "Stripe_Y_Line_Color", label = tr("options.linecolor"),value = "", placeholder = tr("placeholder.color"))
                                                                    )
                                                                )
                                                              )
                                                   ),
                                                   # Create a Layout for CollapsePanels
                                                   bsCollapse(id = "collapseExample", multiple = FALSE, open = NULL,
                                                              bsCollapsePanel(
                                                                title = BSCollapseArrow(tr("layout.collapse.facets.text"), id = "layout_collapse_facets_text"),
                                                                div(class = ".collapse_panel-settings",
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_columns_facets_text", tr("layout.h3.columns"))),
                                                                           selectInput(inputId = "Stripe_X_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Stripe_X_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                                           textInput(inputId = "Stripe_X_Textcolor", label = tr("label.color"), value = "", placeholder = tr("placeholder.color")),
                                                                           numericInput(inputId = "Stripe_X_Textsize", label = tr("label.size"), min = 0, max = 96, step = 0.1, value = NA),
                                                                           selectInput(inputId = "Stripe_X_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                                    ),
                                                                    # Create a column
                                                                    column(6,
                                                                           h3(span(id = "layout_h3_rows_facets_text", tr("layout.h3.rows"))),
                                                                           selectInput(inputId = "Stripe_Y_Font", label = tr("label.font"), choices = font_choices(), selected = "Gemäss Theme"),
                                                                           selectInput(inputId = "Stripe_Y_Face", label = tr("label.face"), choices = face_choices(), selected = "Gemäss Theme"),
                                                                           textInput(inputId = "Stripe_Y_Textcolor", label = "Farbe", value = "", placeholder = "Farbe eingeben zum Anpassen"),
                                                                           numericInput(inputId = "Stripe_Y_Textsize", label = "Grösse", min = 0, max = 96, step = 0.1, value = NA),
                                                                           selectInput(inputId = "Stripe_Y_Alignment", label = tr("label.align"), choices = align_h_choices(), selected = "Gemäss Theme")
                                                                    )
                                                                )
                                                              )

                                             )
                                  )
                 ),
                 
                 
                 
                 
                 
                 ########## 2.4.7 Download ##########
                 # Define Conditional-Panel for when Download tab is selected
                 conditionalPanel(condition = "input.activeTab == 'download'",
                                  fluidRow(
                                    column(6,
                                           h3(span(id = "download_plot_hdr", tr("download.plot"))),
                                           downloadButton("downloadPlot", label = span(id = "download_plot_btn", tr("download.download_plot"))),
                                           selectInput(
                                             "file_format",
                                             label   = span(id = "download_file_format_lbl", tr("download.file_format")),
                                             choices = c("PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg")
                                           )
                                    ),
                                    column(6,
                                           h3(span(id = "download_code_hdr", tr("download.r_code"))),
                                           downloadButton("downloadCode", label = span(id = "download_code_btn", tr("download.download_code")))
                                    )
                                  )
                 )
    ),
    
    
    
    
    
    
    
    
    ############### 2.5 Main Panel ###############
    ########## 2.5.1 Plot-Output ##########
    # Define the Main-Panel
    mainPanel(
      # Set the plot as output
      uiOutput("dynamic_plot"),
      
      
      
      
      ########## 2.5.2 Code-Output ##########
      # Add a column
      column(11,
             # Add TextOutput for rcode
             verbatimTextOutput("rcode"),
      ),
      # Acc a column
      column(1,
             # Add setup to copy rcode
             rclipboardSetup(),
             
             # UI output for copy-to-clipboard button
             uiOutput("clip"),
      )
    )
  ),
  # Create a UI-element for the footer
  uiOutput("app_footer")
)




















#################### 3. Server ####################
server <- function(input, output, session) {
  
  
  
  
  
  ############### 3.1 Define reactive Values ###############
  # Create reactive Value for active Tab
  activeTab <- reactiveVal("data")
  # Create reactive Value for active Plot
  activePlot <- reactiveVal("data")
  # Variable to observe whether options for barplots should be shown
  show_barplot_options <- reactiveVal(value = FALSE)
  # Variable to observe whether options for lineplots should be shown
  show_linepolot_options <- reactiveVal(value = FALSE)
  # Variable to observe whether options for errorbars should be shown
  show_errorbar_options <- reactiveVal(value = FALSE)
  # Variable to observe whether options for scatterplots should be shown
  show_scatter_options <- reactiveVal(value = FALSE)
  # Variable to check wether x-variable is numeric
  is_numeric_x <- reactiveVal(NULL)
  # Variable to check wether y-variable is numeric
  is_numeric_y <- reactiveVal(value = NA)
  # Variable to check wether grouping-variable is numeric
  is_numeric_group <- reactiveVal(value = NA)
  # Variable to check wether facet-column-variable is numeric
  is_numeric_grid_col <- reactiveVal(value = NA)
  # Variable to check wether facet-row-variable is numeric
  is_numeric_grid_row <- reactiveVal(value = NA)
  # Manual Colors for color palette
  manual_colors <- reactiveValues(values = list(), count = 0)
  # Variable to observe whether options for grouping variable should be shown
  show_grouping_options <- reactiveVal(value = FALSE)
  # Variable to observe whether options for facet variable should be shown
  show_facet_options <- reactiveVal(value = FALSE)
  # Variable to observe whether options for title should be shown
  show_title_options <- reactiveVal(value = FALSE)
  # Variable to contain all selected factor variables
  Factors <- reactiveValues(values = c(""))
  # Variable which includes code to change factors
  factor_code <- reactiveVal(value = "")
  # Variable which includes code to change x-axis factors
  x_factor_code <- reactiveVal(value = "")
  # Variable which includes code to change y-axis factors
  y_factor_code <- reactiveVal(value = "")
  # Variable which includes code to change grouping factors
  group_factor_code <- reactiveVal(value = "")
  # Variable which includes code to change facet-column factors
  grid_col_factor_code <- reactiveVal(value = "")
  # Variable which includes code to change facet-row factors
  grid_row_factor_code <- reactiveVal(value = "")
  
  
  
  
  
  
  
  
  
  
  
  
  ############### 3.2 Update UI ###############
  ########## 3.2.1 Observe buttons for active tab ##########
  # Create a list of tabs
  tab_map <- c(
    btn_data        = "data",
    btn_plottype    = "plottype",
    btn_variables   = "variables",
    btn_plot_options= "plot_options",
    btn_text        = "text",
    btn_layout      = "layout",
    btn_download    = "download"
  )
  
  # Apply for all tabs
  invisible(lapply(names(tab_map), function(id){
    # Observe tabs
    observeEvent(input[[id]], {
      # Set selected tab to current tab
      activeTab(tab_map[[id]])
      # Set Button to active tab
      session$sendCustomMessage("setActiveButton", id)
    })
  }))
  
  # Make active tab accessible in the UI
  observe({
    updateTextInput(session, "activeTab", value = activeTab())
  })
  
  
  
  
  ########## 3.2.2 Observe buttons for plot-type ##########
  # Create a list of plot-types
  plot_map <- c(plot_bar="Bar", 
                plot_box="Box", 
                plot_line="Line", 
                plot_scatter="Scatter")
  
  # Apply for all plot-types
  invisible(lapply(names(plot_map), function(id){
    # Observe plot-types
    observeEvent(input[[id]], {
      # Set selected plot-type
      activePlot(plot_map[[id]])
      # Set Button to active version
      session$sendCustomMessage("setActivePlot", id)
    })
  }))
  
  
  
  
  
  ########## 3.2.3 Update UI for plot options ##########
  # Create a list to check whether a plot-type is the active plot
  plot_visibility_map <- list(
    show_barplot_options   = reactive(identical(activePlot(), "Bar")),
    show_linepolot_options = reactive(identical(activePlot(), "Line")),
    show_errorbar_options  = reactive(activePlot() %in% c("Bar","Line")),
    show_scatter_options   = reactive(identical(activePlot(), "Scatter"))
  )
  
  # Apply for all plot-types
  invisible(lapply(names(plot_visibility_map), function(id){
    # Update variable to control visibility of respective plot
    output[[id]] <- reactive(plot_visibility_map[[id]]());
    outputOptions(output, id, suspendWhenHidden = FALSE) 
    }))

  
  
  
  
  
  ########## 3.2.7 Update UI for grouping-variable options ##########
  # Check whether grouping variable is defined
  observeEvent(input$group_var, {
    if(input$group_var == " "){
      show_grouping_options(FALSE)}
    else{
      show_grouping_options(TRUE)}
  })
  # Update variable to control visibility of grouping-variable options
  output$show_grouping_options <- reactive({ show_grouping_options() })
  outputOptions(output, "show_grouping_options", suspendWhenHidden = FALSE)
  
  
  
  
  
  ########## 3.2.8 Update UI for facet-variable options ##########
  # Check whether a facet variable is defined
  observeEvent(list(input$grid_col_var, input$grid_row_var), {
    if(input$grid_col_var == " " & input$grid_row_var == " "){
      show_facet_options(FALSE)}
    else{
      show_facet_options(TRUE)}
  })
  # Update variable to control visibility of facet-variable options
  output$show_facet_options <- reactive({ show_facet_options() })
  outputOptions(output, "show_facet_options", suspendWhenHidden = FALSE)

  
  

    
  ########## 3.2.9 Update UI for title options ##########
  # Check whether a title is defined
  observeEvent(list(input$plot_title, input$plot_subtitle), {
    if(input$plot_title == "" & input$plot_subtitle ==""){
      show_title_options(FALSE)}
    else{
      show_title_options(TRUE)}
  })
  # Update variable to control visibility of title options
  output$show_title_options <- reactive({ show_title_options() })
  outputOptions(output, "show_title_options", suspendWhenHidden = FALSE)
  
  
  
  
  
  
  
  
  
  
  ############### 3.3 Read Data ###############
  # Create a reactive data with the loaded data
  data <- reactive({
    if (is.null(input$file)) {
      # Return an empty standard dataset if no data is selected
      return(data.frame(
        Placeholder_X = numeric(0),
        Placeholder_Y = numeric(0)
      ))
      # If input-file is selected
    } else {
      # Get Input-File
      req(input$file)
      # Get the type of selected file
      file_ext <- tools::file_ext(input$file$name)
      # Switch function based on file-type
      switch(file_ext,
             # Read CSV-Files
             csv  = read.csv(input$file$datapath, header = TRUE),
             # Read Excel-File
             xlsx = readxl::read_excel(input$file$datapath),
             # Read rds-File 
             rds  = readRDS(input$file$datapath),
             # Read rdata-File
             rdata = {
               # Create new environment
               env <- new.env()
               # Load file
               load(input$file$datapath, envir = env)
               # List all files in environment
               objs <- ls(env)
               # Select first object of .RData-File if there is only one object, give error message when multiple objects are in .RData-File
               if (length(objs) == 1) env[[objs[1]]] else stop(tr("error.rdata_multiple"))
             },
             stop(tr("error.unknown_format"))
      )
    }
  })

  
  
  
  
  
  
  
  
  ############### 3.4 Update Variables ###############
  ########## 3.4.1 Define list of variables ##########
  observeEvent(data(), {
    # Create a list of all variable-dropdowns
    variable_dropdowns <- c("x_var","y_var","group_var","grid_col_var","grid_row_var")
    # Create a list of all available variables
    variable_options <- c("Keine Variable" = " ", names(data()))
    
    # Add variables into each dropdown
    lapply(variable_dropdowns, function(id) updateSelectInput(session, id, choices = variable_options, selected = " "))
  })
  
  
  
  
  
  ########## 3.4.2 Observe selected variables ##########
  observeEvent(list(input$x_var, input$y_var, input$group_var, input$grid_col_var, input$grid_row_var), {
    # Require additional variables
    req(data())  
    req(input$x_var)  
    req(input$y_var)  
    req(input$group_var)  
    req(input$grid_col_var)  
    req(input$grid_row_var)  
    
    # Define selected variables internally
    x_data <- data()[[input$x_var]]
    y_data <- data()[[input$y_var]]
    goup_data <- data()[[input$group_var]]
    grid_col_data <- data()[[input$grid_col_var]]
    grid_row_data <- data()[[input$grid_row_var]]
    
    # Mark if x-axis variable is a factor or character
    if (is.factor(x_data)) {
      Factors$x_values <- levels(x_data)
      is_numeric_x(FALSE)
    } else if (is.character(x_data)) {
      Factors$x_values <- unique(x_data)
      is_numeric_x(FALSE)
    } else {
      Factors$x_values <- c("")
      is_numeric_x(TRUE)
    }
    
    # Mark if y-axis variable is a factor or character
    if (is.factor(y_data)) {
      Factors$y_values <- levels(y_data)
    } else if (is.character(y_data)) {
      Factors$y_values <- unique(y_data)
    } else {
      Factors$y_values <- c("")
    }
    
    # Mark if grouping variable is a factor or character
    if (is.factor(goup_data)) {
      Factors$group_values <- levels(goup_data)
    } else if (is.character(goup_data)) {
      Factors$group_values <- unique(goup_data)
    } else {
      Factors$group_values <- c("")
    }
    
    # Mark if facet-column variable is a factor or character
    if (is.factor(grid_col_data)) {
      Factors$grid_cols_values <- levels(grid_col_data)
    } else if (is.character(grid_col_data)) {
      Factors$grid_cols_values <- unique(grid_col_data)
    } else {
      Factors$grid_cols_values <- c("")
    }
    
    # Mark if facet-row variable is a factor or character
    if (is.factor(grid_row_data)) {
      Factors$grid_row_values <- levels(grid_row_data)
    } else if (is.character(grid_row_data)) {
      Factors$grid_row_values <- unique(grid_row_data)
    } else {
      Factors$grid_row_values <- c("")
    }
  })
  
  # Set is_numeric_boolean for each variable-dropdown
  for (pair in list(
    c("x_var","is_numeric_x"),
    c("y_var","is_numeric_y"),
    c("group_var","is_numeric_group"),
    c("grid_col_var","is_numeric_col"),
    c("grid_row_var","is_numeric_row")
  )) local({
    # Get the pair
    inp <- pair[1]; out <- pair[2]
    # Make reactive output
    output[[out]] <- reactive({
      # Get data and the inputs
      req(input[[inp]], data())
      # get the variable
      this_variable <- data()[[ input[[inp]] ]]
      # check if variable is factor or character
      !(is.factor(this_variable) || is.character(this_variable))
    })
    # Hide the output-variables
    outputOptions(output, out, suspendWhenHidden = FALSE)
  })
  
  
  # For all variables
  for (i in c("x","y","group","grid_col","grid_row")) local({
    # Switch based on current variable
    setter <- switch(i,
                     x = x_factor_code,
                     y = y_factor_code,
                     group = group_factor_code,
                     grid_col = grid_col_factor_code,
                     grid_row = grid_row_factor_code
    )
    
    # Set associated input-variable
    input_variable <- switch(i,
                             x="x_var",
                             y="y_var",
                             group="group_var",
                             grid_col="grid_col_var",
                             grid_row="grid_row_var"
    )
    
    # Observe Input-Variable
    observeEvent(input[[input_variable]], {
      # Reset setter-variable
      setter("")
      # Append factor-code
      factor_code(paste(x_factor_code(), 
                        y_factor_code(), 
                        group_factor_code(),
                        grid_col_factor_code(), 
                        grid_row_factor_code()))
    })
  })
  
  
  # For all variables
  for (i in c("x","y","group","grid_col","grid_row")) local({
    # Get the current variable
    this.variable <- i
    
    # Create the UI for the current variable
    output[[paste0(this.variable, "_factor_rank_list")]] <- renderUI({
      # Set the labels as factors of each variable
      labels <- switch(this.variable,
                       "x"        = Factors$x_values,
                       "y"        = Factors$y_values,
                       "group"    = Factors$group_values,
                       "grid_col" = Factors$grid_cols_values,
                       "grid_row" = Factors$grid_row_values
      )
      # Create dynamic UI for rank_list() of y-axis variable
      rank_list(input_id = paste0(this.variable, "_factor_Order"),
                # Set labels
                labels = labels,
                # Set options
                options = sortable_options(swap = TRUE))
    })
  })
  
  
  

  
  ########## 3.4.3 Check for changes in factor orders ##########
  # For all variables
  for (i in c("x","y","group","grid_col","grid_row")) local({
    # Create factor_order_variable for current variable
    this_variable_order <- paste0(i, "_factor_Order")
    # Create variable for current variable
    this_variable   <- paste0(switch(i,
                                     x="x", y="y", group="group", grid_col="grid_col", grid_row="grid_row"), 
                              # Paste variable-suffix
                              "_var")
    # Switch variable values for current variable
    vals_sym <- switch(i,
                       x="x_values",
                       y="y_values",
                       group="group_values",
                       grid_col="grid_cols_values",
                       grid_row="grid_row_values"
    )
    # Switch variable-order code for current variable
    setter <- switch(i,
                     x = x_factor_code, 
                     y = y_factor_code,
                     group = group_factor_code, 
                     grid_col = grid_col_factor_code,
                     grid_row = grid_row_factor_code
    )
    # Observe the current variable_order
    observeEvent(input[[this_variable_order]], {
      # Require data, variable order and variable
      req(data(), input[[this_variable_order]], input[[this_variable]])
      # Create empty code
      new_code <- ""
      # If changes were made in variable-order
      if (!identical(Factors[[vals_sym]], input[[this_variable_order]])) {
        # Get the respective variable-data
        var_data <- data()[[ input[[this_variable]] ]]
        # If the variable is a factor or character
        if (is.factor(var_data) || is.character(var_data)) {
          # Create new code to change factor levels order
          new_code <- sprintf(
            "\ndf$'%s' <- factor(df$'%s', levels = c(%s))",
            input[[this_variable]], input[[this_variable]],
            paste(sprintf("'%s'", input[[this_variable_order]]), collapse = ", ")
          )
        }
      }
      # Set new code
      setter(new_code)
      # Paste factor code
      factor_code(paste(x_factor_code(),
                        y_factor_code(),
                        group_factor_code(),
                        grid_col_factor_code(),
                        grid_row_factor_code()))
    })
  })
  
  
  
  
  
  
  
  
  
  ############### 3.5 Update Manual Color Palette ###############
  ########## 3.5.1 Add new Colors ##########
  # When "Add" is Pressed
  observeEvent(input$add, {
    # Increase counter of ID's
    manual_colors$count <- manual_colors$count + 1
    # Set new couner ID
    new_id <- manual_colors$count
    # Insert new UI
    insertUI(
      selector = "#input_container",
      where = "beforeEnd",
      ui = tags$div(
        id = paste0("input_row_", new_id),
        style = "display: flex; align-items: center;",
        textInput(paste0("vector_", new_id),
                  label = sprintf("%s %d", tr("label.color"), new_id),
                  placeholder = tr("options.custom.colorpalette.placeholder")
                  ),
        tags$span(
          id = paste0("vector_", new_id, "_error"),
          class = "error-message", 
          tr("options.custom.colorpalette.error"),
          style = "display: none;")
      )
    )
    # Create new entries using a standard-color
    manual_colors$values[[as.character(new_id)]] <- "gray"
    # Observe all entries
    observe({for (id in names(manual_colors$values)) {
      color <- input[[paste0("vector_", id)]]
      if (!is.null(color)) {
        if (is_valid_color(color)) {
          manual_colors$values[[id]] <- color
          session$sendCustomMessage("validColor", list(id = paste0("vector_", id), valid = TRUE))
        } else {
          manual_colors$values[[id]] <- "gray"
          session$sendCustomMessage("validColor", list(id = paste0("vector_", id), valid = FALSE))
        }
      }
    }
    })
  })
  
  
  
  
  ########## 3.5.2 Remove colors ##########
  # When "Remove" is Pressed
  observeEvent(input$remove_last, {
    # If there is at least one color defined
    if (manual_colors$count > 0) {
      # Remove last element of manual_colors
      last_id <- as.character(manual_colors$count)
      # Remove UI
      removeUI(selector = paste0("#input_row_", last_id))
      # Remove Value of manual_colors
      manual_colors$values[[last_id]] <- NULL
      # Reduce counter of manual_colors
      manual_colors$count <- manual_colors$count - 1
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### 4. Generate Basic R-Code ###############
  ############### 4.1 Set reactive R-Code ###############
  # Define Code-Text
  r_code <- reactive({
    req(data())
    
    
    
    
    
    
    
    
    
    ############### 4.2 Set Variables Based on Input-Data ###############
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
    # Errorbar Size
    error_size <- if (!is.na(input$error_size)) input$error_size else NULL 
    # Position-Dodge value
    dodge_value <- if (is.na(input$dodge_value)==TRUE) NA else input$dodge_value
    # Scatter-Point size
    scatterpoint_size <-  if (is.na(input$scatterpoint_size)==TRUE) NA else input$scatterpoint_size
    # Selected Theme
    theme_selected <- input$plot_theme
    # Selected Color-Palette
    palette_selected <- input$Color_Palette
    
    
    
    
    
    
    
    
    
    ############### 4.3 Define Code with X- and Y- Axis variable ###############
    # Define Code Lines
    r_code <- sprintf("q <- ggplot(df, aes(x = %s, y = %s", x_var, y_var)
    
    
    
    
    
    
    
    
    
    ############### 4.4 Add Grouping Variable ###############
    # Define Grouping-variable if selected
    if (!is.null(group_var)) {
      if(input$color_palette_target == "Füllung"){
        r_code <- paste0(r_code, sprintf(", fill = %s))", group_var))
      }
      if(input$color_palette_target == "Linien"){
        r_code <- paste0(r_code, sprintf(", color = %s))", group_var))
      }
      if(input$color_palette_target == "Füllung und Linien"){
        r_code <- paste0(r_code, sprintf(", fill = %s, color = %s))", group_var, group_var))
      }
      # Close first line of Plot-relevant Code if no Grouping Variable is selected
    } else {
      r_code <- paste0(r_code, sprintf("))"))
    }
    
    
    
    
    
    
    
    
    
    ############### 4.5 Define Geoms ###############
    ########## 4.5.1 Bar or Line-Geoms ##########
    if (x_var != "1" && y_var != "1") {
      if (activePlot() == "Bar"|activePlot() == "Line"){
        if (activePlot() == "Bar"){
          r_code <- paste0(r_code, " +\n  stat_summary(fun = mean, geom = 'bar'")
          if (!is.na(input$barplot_width)){
            r_code <- paste0(r_code, sprintf(", width = %.1f", input$barplot_width))
          }
        }
        if (activePlot() == "Line"){
          # r_code <- paste0(r_code, " +\n  stat_summary(fun = mean, geom = 'line'")
          if (!is.null(group_var)) {
            r_code <- paste0(r_code, sprintf(" +\n  stat_summary(aes(group = %s, color = %s), fun = mean, geom = 'line'", group_var, group_var))
          # Close first line of Plot-relevant Code if no Grouping Variable is selected
          }
          if (is.null(group_var)) {
            r_code <- paste0(r_code, sprintf(" +\n  stat_summary(fun = mean, geom = 'line', group = 1"))
            # Close first line of Plot-relevant Code if no Grouping Variable is selected
          }
          if(!is.na(input$lineplot_width)){
            r_code <- paste0(r_code, sprintf(", size = %.1f", input$lineplot_width))
          }
          if(input$lineplot_line_type!="Solide"){
            r_code <- paste0(r_code, sprintf(", linetype = '%s'", switch(input$lineplot_line_type,
                                                                         "Solide" = "solid",
                                                                         "Gestrichelt" = "dashed",
                                                                         "Gepunkted" = "dotted",
                                                                         "Punktgestrichelt" = "dotdash",
                                                                         "Langgestrichen" = "longdash",
                                                                         "Doppelt gestrichelt" = "twodash")))
          }
          
        }
        if (!is.na(dodge_value)) {
          r_code <- paste0(r_code, sprintf(", position = position_dodge(width = %s)", dodge_value))
        }
        r_code <- paste0(r_code, ")")
        
        
        
        
        
        ########## 4.5.2 Errorbar-Geoms ##########
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
            r_code <- paste0(r_code, sprintf(", width = %.1f", error_width))
          }
          if (!is.null(error_size)) {
            r_code <- paste0(r_code, sprintf(", linewidth = %.1f", error_size))
          }
          if (!is.na(dodge_value)) {
            r_code <- paste0(r_code, sprintf(", position = position_dodge(width = %s)", dodge_value))
          }
          if (((input$error_type != "Standardfehler") & (input$error_mult!=2) & !is.na(input$error_mult))|
              ((input$error_type == "Standardfehler") & (input$error_mult!=1) & !is.na(input$error_mult))){
            r_code <- paste0(r_code, sprintf(", fun.args = list(mult = %.0f)", input$error_mult))
          }
          r_code <- paste0(r_code, ")")
        }
      }
    }
    
    
    
    
    
    ########## 4.5.3 Boxplot-Geoms ##########
    if (x_var != "1" && y_var != "1") {
      if (activePlot() == "Box"){
        r_code <- paste0(r_code, " +\n  geom_boxplot(")
        
        if (!is.na(dodge_value)) {
          r_code <- paste0(r_code, sprintf(", position = position_dodge(width = %s)", dodge_value))
        }
        r_code <- paste0(r_code, ")")
        
        
      }
    }
    
    
    
    
    
    ########## 4.5.4 Scatterplot-Geoms ##########
    if (x_var != "1" && y_var != "1") {
      if (activePlot() == "Scatter"){
        r_code <- paste0(r_code, " +\n  geom_point(")
        
        if (!is.na(dodge_value)) {
          r_code <- paste0(r_code, sprintf(", position = position_dodge(width = %s)", dodge_value))
        }
        
        if (!is.na(scatterpoint_size)){
          r_code <- paste0(r_code, sprintf(", size = %s", scatterpoint_size))
        }
        r_code <- paste0(r_code, ")")
        
        
        
        
        
        ########## 4.5.5 Regressionline-Geoms ##########
        if(input$show_line==TRUE){
          r_code <- paste0(r_code, " +\n  geom_smooth(method = 'lm'")
          if(input$scater_line_show_se==FALSE){
            r_code <- paste0(r_code, ", se = FALSE")
          }
          if(!is.na(input$scater_line_size)){
            r_code <- paste0(r_code, ", size = ", input$scater_line_size)
          }
          if (input$scater_line_type != "Solide") {
            r_code <- paste0(r_code, sprintf(", linetype = '%s'",
                                                               switch(input$scater_line_type,
                                                                      "Solide" = "solid",
                                                                      "Gestrichelt" = "dashed",
                                                                      "Gepunkted" = "dotted",
                                                                      "Punktgestrichelt" = "dotdash",
                                                                      "Langgestrichen" = "longdash",
                                                                      "Doppelt gestrichelt" = "twodash"
                                                               )))}
          if(input$scater_line_full_range){
            r_code <- paste0(r_code, ", fullrange = TRUE")
          }
          r_code <- paste0(r_code, ")")
        }
      }
    }
    
    
    
    
    
    
    
    
    
    ############### 4.6 Define Facets ############### 
    # Add Facets if defined
    if (!is.null(grid_col_var) & !is.null(grid_row_var)) {
      r_code <- paste0(r_code, sprintf(" +\n  facet_grid(rows = vars(%s), cols = vars(%s))", grid_row_var, grid_col_var))
    } else if (!is.null(grid_col_var)) {
      r_code <- paste0(r_code, sprintf(" +\n  facet_grid(rows = vars(%s))", grid_col_var))
    } else if (!is.null(grid_row_var)) {
      r_code <- paste0(r_code, sprintf(" +\n  facet_grid(cols = vars(%s))", grid_row_var))
    }
    
    
    
    
    
    
    
    
    
    ############### 4.7 Set Themes ###############
    # Add themes if theme is not gray
    if (theme_selected != "Gray") {
      r_code <- paste0(r_code, sprintf(" +\n  %s",
                                       switch(theme_selected,
                                              "Bw" = "theme_bw()",
                                              "Classic" = "theme_classic()",
                                              "Gray" = "theme_gray()",
                                              "Linedraw" = "theme_linedraw()",
                                              "Light" = "theme_light()",
                                              "Dark" = "theme_dark()",
                                              "Minimal" = "theme_minimal()",
                                              "Void" = "theme_void()",
                                              "Calc" = "theme_calc()",
                                              "the Economist" = "theme_economist()",
                                              "the Economist White" = "theme_economist_white()",
                                              "Excel" = "theme_excel()",
                                              "Few" = "theme_few()",
                                              "FiveThirtyEight" = "theme_fivethirtyeight()",
                                              "Google Docs" = "theme_gdocs()",
                                              "Highcharts JS" = "theme_hc()",
                                              "Inversed Gray" = "theme_igray()",
                                              "Solarized" = "theme_solarized()",
                                              "Solarized 2" = "theme_solarized_2()",
                                              "Solid" = "theme_solid()",
                                              "Stata" = "theme_stata()",
                                              "Tufte" = "theme_tufte()",
                                              "Wall Street Journal" = "theme_wsj()",
                                              paste0("theme_", tolower(theme_selected), "()") # Fallback
                                       )
      )
      )
    }
    
    
    
    
    
    
    
    
    
    ############### 4.8 Set Labs ###############
    # Define code-line for labs
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
          if (labs_code != "")
            ", "
          else
            "",
          if (input$color_palette_target == "Füllung")
            sprintf("fill = '%s'", input$legend_title)
          else if (input$color_palette_target == "Linien")
            sprintf("color = '%s'", input$legend_title)
          else if (input$color_palette_target == "Füllung und Linien")
            sprintf("fill = '%s', color = '%s'", input$legend_title, input$legend_title)
        )
    }

    # Add labs_code when labs were assigned
    if (labs_code != "") {
      r_code <- paste0(r_code, sprintf(" +\n  labs(%s)", labs_code))
    }
    
    
    
    
    
    
    
    
    
    ############### 4.9 X and Y-Axis Range ###############
    # Define empty Code for axis_code
    axis_code <- ""
    # Adjust X-Axis range
    if (!is.na(x_axis_min) || !is.na(x_axis_max) ||! is.na(y_axis_min) || !is.na(y_axis_max) || !input$exact_axis_range) {
      axis_code <- "coord_cartesian("
      
      if (!is.na(x_axis_min) || !is.na(x_axis_max)) {
        axis_code <- paste0(axis_code, sprintf("xlim = c(%s, %s)", 
                                               if (!is.na(x_axis_min)) x_axis_min else "NA", 
                                               if (!is.na(x_axis_max)) x_axis_max else "NA"))
        if(!is.na(y_axis_min) || !is.na(y_axis_max)){
          axis_code <- paste0(axis_code, ", ")}
      }
      if (!is.na(y_axis_min) || !is.na(y_axis_max)) {
        axis_code <- paste0(axis_code, sprintf("ylim = c(%s, %s)", 
                                               if (!is.na(y_axis_min)) y_axis_min else "NA", 
                                               if (!is.na(y_axis_max)) y_axis_max else "NA"))
      }
      if (!input$exact_axis_range) {
        if(!is.na(y_axis_min) || !is.na(y_axis_max) || !is.na(x_axis_min) || !is.na(x_axis_max)){
          axis_code <- paste0(axis_code, ", ")}
        axis_code <- paste0(axis_code, "expand = FALSE")
      }
      # Close axis code
      axis_code <- paste0(axis_code, ")")
      # Paste axis code to r-code
      r_code <- paste0(r_code, sprintf(" +\n  "), axis_code)#
    } else {
      # Set empty axis-code if nothing is defined
      axis_code <- ""
    }
    
    
    
    
    
    
    
    
    
    ############### 4.10 Color-Palette - Fill ###############
    if (input$Color_Palette != "Gemäss Theme" && (input$color_palette_target == "Füllung" | input$color_palette_target == "Füllung und Linien")) {
      # If manual color palette should be created
      if (palette_selected == "Eigene Farbpalette erstellen") {
        # Check if there is at least one value entered
        if (length(manual_colors$values)>0){
          if (length(manual_colors$values)<length(unique(data()[[group_var]]))){
            # Add values
            r_code <- paste0(r_code, sprintf(" +\n  scale_fill_manual(values = rep(c(%s), length(unique(df$%s))))",
                                             paste0(sprintf("'%s'", manual_colors$values), collapse = ", "),
                                             group_var))}
          else{
            # Add values
            r_code <- paste0(r_code, sprintf(" +\n  scale_fill_manual(values = c(%s))",
                                             paste0(sprintf("'%s'", manual_colors$values), collapse = ", ")))}
        }
      } else {
        r_code <- paste0(r_code, sprintf(" +\n  %s",
                                         switch(palette_selected,
                                                "Accent" = "scale_fill_brewer(palette = 'Accent')",
                                                "Blues" = "scale_fill_brewer(palette = 'Blues')",
                                                "Greens" = "scale_fill_brewer(palette = 'Greens')",
                                                "Greys" = "scale_fill_brewer(palette = 'Greys')",
                                                "Oranges" = "scale_fill_brewer(palette = 'Oranges')",
                                                "Paired" = "scale_fill_brewer(palette = 'Paired')",
                                                "Pastel1" = "scale_fill_brewer(palette = 'Pastel1')",
                                                "Pastel2" = "scale_fill_brewer(palette = 'Pastel2')",
                                                "Purples" = "scale_fill_brewer(palette = 'Purples')",
                                                "Reds" = "scale_fill_brewer(palette = 'Reds')",
                                                "Set1" = "scale_fill_brewer(palette = 'Set1')",
                                                "Set2" = "scale_fill_brewer(palette = 'Set2')",
                                                "Set3" = "scale_fill_brewer(palette = 'Set3')",
                                                "Spectral" = "scale_fill_brewer(palette = 'Spectral')",
                                                "grey" = "scale_fill_grey()",
                                                "hue" = "scale_fill_hue()",
                                                "ordinal" = "scale_fill_ordinal()",
                                                "viridis" = "scale_fill_viridis_d(option = 'viridis')",
                                                "viridis - magma" = "scale_fill_viridis_d(option = 'magma')",
                                                "viridis - plasma" = "scale_fill_viridis_d(option = 'plasma')",
                                                "viridis - inferno" = "scale_fill_viridis_d(option = 'inferno')",
                                                "viridis - cividis" = "scale_fill_viridis_d(option = 'cividis')",
                                                "viridis - mako" = "scale_fill_viridis_d(option = 'mako')",
                                                "viridis - rocket" = "scale_fill_viridis_d(option = 'rocket')",
                                                "viridis - turbo" = "scale_fill_viridis_d(option = 'turbo')",
                                                "aas" = "scale_fill_aaas()",
                                                "bmj" = "scale_fill_bmj()",
                                                "cosmic" = "scale_fill_cosmic()",
                                                "d3" = "scale_fill_d3()",
                                                "flatui" = "scale_fill_flatui()",
                                                "frontiers" = "scale_fill_frontiers()",
                                                "futurama" = "scale_fill_futurama()",
                                                "igv" = "scale_fill_igv()",
                                                "jama" = "scale_fill_jama()",
                                                "lancet" = "scale_fill_lancet()",
                                                "locuszoom" = "scale_fill_locuszoom()",
                                                "nejm" = "scale_fill_nejm()",
                                                "npg" = "scale_fill_npg()",
                                                "observable" = "scale_fill_observable()",
                                                "rickandmorty" = "scale_fill_rickandmorty()",
                                                "simpsons" = "scale_fill_simpsons()",
                                                "startrek" = "scale_fill_startrek()",
                                                "tron" = "scale_fill_tron()",
                                                "uchicago" = "scale_fill_uchicago()",
                                                "ucscgb" = "scale_fill_ucscgb()",
                                                "jco" = "scale_fill_jco()",
                                                "calc" = "scale_fill_calc()",
                                                "canva" = "scale_fill_canva()",
                                                "colorblind" = "scale_fill_colorblind()",
                                                "economist" = "scale_fill_economist()",
                                                "excel" = "scale_fill_excel()",
                                                "excel_new" = "scale_fill_excel_new()",
                                                "few" = "scale_fill_few()",
                                                "fivethirtyeight" = "scale_fill_fivethirtyeight()",
                                                "gdocs" = "scale_fill_gdocs()",
                                                "hc" = "scale_fill_hc()",
                                                "pander" = "scale_fill_pander()",
                                                "ptol" = "scale_fill_ptol()",
                                                "solarized" = "scale_fill_solarized()",
                                                "stata" = "scale_fill_stata()",
                                                "tableau" = "scale_fill_tableau()",
                                                "wsj" = "scale_fill_wsj()"
                                                )
                                         )
                         )
        }
    }
    
    
    
    
    
    
    
    
    
    ############### 4.11 Color-Palette - Color ###############
    if (input$Color_Palette != "Gemäss Theme" && (input$color_palette_target == "Linien" | input$color_palette_target == "Füllung und Linien")) {
      # If manual color palette should be created
      if (palette_selected == "Eigene Farbpalette erstellen") {
        # Check if there is at least one value entered
        if (length(manual_colors$values)>0){
          if (length(manual_colors$values)<length(unique(data()[[group_var]]))){
            # Add values
            r_code <- paste0(r_code, sprintf(" +\n  scale_color_manual(values = rep(c(%s), length(unique(df$%s))))",
                                             paste0(sprintf("'%s'", manual_colors$values), collapse = ", "),
                                             group_var))}
          else{
            # Add values
            r_code <- paste0(r_code, sprintf(" +\n  scale_color_manual(values = c(%s))",
                                             paste0(sprintf("'%s'", manual_colors$values), collapse = ", ")))}
        }
      } else {
        r_code <- paste0(r_code, sprintf(" +\n  %s",
                                         switch(palette_selected,
                                                "Accent" = "scale_color_brewer(palette = 'Accent')",
                                                "Blues" = "scale_color_brewer(palette = 'Blues')",
                                                "Greens" = "scale_color_brewer(palette = 'Greens')",
                                                "Greys" = "scale_color_brewer(palette = 'Greys')",
                                                "Oranges" = "scale_color_brewer(palette = 'Oranges')",
                                                "Paired" = "scale_color_brewer(palette = 'Paired')",
                                                "Pastel1" = "scale_color_brewer(palette = 'Pastel1')",
                                                "Pastel2" = "scale_color_brewer(palette = 'Pastel2')",
                                                "Purples" = "scale_color_brewer(palette = 'Purples')",
                                                "Reds" = "scale_color_brewer(palette = 'Reds')",
                                                "Set1" = "scale_color_brewer(palette = 'Set1')",
                                                "Set2" = "scale_color_brewer(palette = 'Set2')",
                                                "Set3" = "scale_color_brewer(palette = 'Set3')",
                                                "Spectral" = "scale_color_brewer(palette = 'Spectral')",
                                                "grey" = "scale_color_grey()",
                                                "hue" = "scale_color_hue()",
                                                "ordinal" = "scale_color_ordinal()",
                                                "viridis" = "scale_color_viridis_d(option = 'viridis')",
                                                "viridis - magma" = "scale_color_viridis_d(option = 'magma')",
                                                "viridis - plasma" = "scale_color_viridis_d(option = 'plasma')",
                                                "viridis - inferno" = "scale_color_viridis_d(option = 'inferno')",
                                                "viridis - cividis" = "scale_color_viridis_d(option = 'cividis')",
                                                "viridis - mako" = "scale_color_viridis_d(option = 'mako')",
                                                "viridis - rocket" = "scale_color_viridis_d(option = 'rocket')",
                                                "viridis - turbo" = "scale_color_viridis_d(option = 'turbo')",
                                                "aas" = "scale_color_aaas()",
                                                "bmj" = "scale_color_bmj()",
                                                "cosmic" = "scale_color_cosmic()",
                                                "d3" = "scale_color_d3()",
                                                "flatui" = "scale_color_flatui()",
                                                "frontiers" = "scale_color_frontiers()",
                                                "futurama" = "scale_color_futurama()",
                                                "igv" = "scale_color_igv()",
                                                "jama" = "scale_color_jama()",
                                                "lancet" = "scale_color_lancet()",
                                                "locuszoom" = "scale_color_locuszoom()",
                                                "nejm" = "scale_color_nejm()",
                                                "npg" = "scale_color_npg()",
                                                "observable" = "scale_color_observable()",
                                                "rickandmorty" = "scale_color_rickandmorty()",
                                                "simpsons" = "scale_color_simpsons()",
                                                "startrek" = "scale_color_startrek()",
                                                "tron" = "scale_color_tron()",
                                                "uchicago" = "scale_color_uchicago()",
                                                "ucscgb" = "scale_color_ucscgb()",
                                                "jco" = "scale_color_jco()",
                                                "calc" = "scale_color_calc()",
                                                "canva" = "scale_color_canva()",
                                                "colorblind" = "scale_color_colorblind()",
                                                "economist" = "scale_color_economist()",
                                                "excel" = "scale_color_excel()",
                                                "excel_new" = "scale_color_excel_new()",
                                                "few" = "scale_color_few()",
                                                "fivethirtyeight" = "scale_color_fivethirtyeight()",
                                                "gdocs" = "scale_color_gdocs()",
                                                "hc" = "scale_color_hc()",
                                                "pander" = "scale_color_pander()",
                                                "ptol" = "scale_color_ptol()",
                                                "solarized" = "scale_color_solarized()",
                                                "stata" = "scale_color_stata()",
                                                "tableau" = "scale_color_tableau()",
                                                "wsj" = "scale_color_wsj()"
                                                )
                                         )
                         )
        }
    }    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ############### 5. Generate Basic Theme-Code ###############
    ############### 5.1 Generate Theme-Code ###############
    # Create Variable for Theme Code
    theme_code <- ""
    
    
    
    
    
    
    
    
    
    ############### 5.2 Title-Theme ###############
    # Check if theme for title should be adjusted
    if (input$Title_Font != "Gemäss Theme" || input$Title_Face != "Gemäss Theme" ||
        input$Title_Color != "" || !is.na(input$Title_Size) || input$Title_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for plot-title-theme
      theme_code <- paste0(theme_code, "plot.title = element_text(")
      # Paste theme settings from UI
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
    
    
    
    
    
    
    
    
    
    ############### 5.3 Subtitle-Theme ###############
    # Check if theme for subtitle should be adjusted
    if (input$Subtitle_Font != "Gemäss Theme" || input$Subtitle_Face != "Gemäss Theme" ||
        input$Subtitle_Color != "" || !is.na(input$Subtitle_Size) || input$Subtitle_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for plot-subtitle-theme
      theme_code <- paste0(theme_code, "plot.subtitle = element_text(")
      # Paste theme settings from UI
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
    
    
    
    
    
    
    
    
    
    ############### 5.4 X-Axis Title Theme ###############
    # Check if X-Axis Title Theme should be adjusted
    if (input$X_Axis_Title_Font != "Gemäss Theme" || input$X_Axis_Title_Face != "Gemäss Theme" ||
        input$X_Axis_Title_Color != "" || !is.na(input$X_Axis_Title_Size) || input$X_Axis_Title_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for X-Axis Title Theme
      theme_code <- paste0(theme_code, "axis.title.x = element_text(")
      # Paste theme settings from UI
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
    
    
    
    
    
    
    
    
    
    ############### 5.5 Y-Axis TitleTheme ###############
    # Check if Y-Axis Title theme should be adjusted
    if (input$Y_Axis_Title_Font != "Gemäss Theme" || input$Y_Axis_Title_Face != "Gemäss Theme" ||
        input$Y_Axis_Title_Color != "" || !is.na(input$Y_Axis_Title_Size) || input$Y_Axis_Title_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for plot-title-theme
      theme_code <- paste0(theme_code, "axis.title.y = element_text(")
      # Paste theme settings from UI
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
    
    
    
    
    
    
    
    
    
    ############### 5.6 X-Axis-Labels ###############
    # Check if theme for X-Axis-Labels should be adjusted
    if (input$Axis_X_Text_Font != "Gemäss Theme" || input$Axis_X_Text_Face != "Gemäss Theme" ||
        input$Axis_X_Text_Color != "" || !is.na(input$Axis_X_Text_Size) || input$Axis_X_Text_H_Alignment != "Gemäss Theme"
        || input$Axis_X_Text_V_Alignment != "Gemäss Theme" || !is.na(input$Axis_X_Text_Rotation)) {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for X-Axis-Labels theme
      theme_code <- paste0(theme_code, "axis.text.x = element_text(")
      # Paste theme settings from UI
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
                           if (!is.na(input$Axis_X_Text_Rotation)) sprintf("angle = %.0f, ", input$Axis_X_Text_Rotation) else "")
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.7 Y-Axis-Labels Theme ###############
    # Check if theme for Y-Axis-Labels should be adjusted
    if (input$Axis_Y_Text_Font != "Gemäss Theme" || input$Axis_Y_Text_Face != "Gemäss Theme" ||
        input$Axis_Y_Text_Color != "" || !is.na(input$Axis_Y_Text_Size) || input$Axis_Y_Text_H_Alignment != "Gemäss Theme"
        || input$Axis_Y_Text_V_Alignment != "Gemäss Theme" || !is.na(input$Axis_Y_Text_Rotation)) {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for Y-Axis-Labels Theme
      theme_code <- paste0(theme_code, "axis.text.y = element_text(")
      # Paste theme settings from UI
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
                           if (!is.na(input$Axis_Y_Text_Rotation)) sprintf("angle = %.0f, ", input$Axis_Y_Text_Rotation) else "")
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.8 X Axis Lines Theme ###############
    # Check if theme should be adjusted for X Axis Lines
    if (input$Axis_X_Linetype != "Gemäss Theme" || !is.na(input$Axis_X_Size) || input$Axis_X_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for X Axis Lines Theme
      if (input$Axis_X_Linetype == "Keine"){
        theme_code <- paste0(theme_code, "axis.line.x = element_blank(")
      } else {
        theme_code <- paste0(theme_code, "axis.line.x = element_line(")
        # Paste theme settings from UI
        theme_code <- paste0(theme_code,
                             if (input$Axis_X_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                  switch(input$Axis_X_Linetype,
                                                                                         "Solide" = "solid",
                                                                                         "Gestrichelt" = "dashed",
                                                                                         "Gepunkted" = "dotted",
                                                                                         "Punktgestrichelt" = "dotdash",
                                                                                         "Langgestrichen" = "longdash",
                                                                                         "Doppelt gestrichelt" = "twodash"
                                                                                  )) else "",
                             if (!is.na(input$Axis_X_Size)) sprintf("size = %.1f, ", input$Axis_X_Size) else "",
                             if (input$Axis_X_Color != "") sprintf("colour = '%s', ", input$Axis_X_Color) else "")
      }
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.9 Y Axis Lines Theme ###############
    # Check if theme should be adjusted for Y Axis Lines
    if (input$Axis_Y_Linetype != "Gemäss Theme" || !is.na(input$Axis_Y_Size) || input$Axis_Y_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for Y Axis Lines Theme
      if (input$Axis_Y_Linetype == "Keine"){
        theme_code <- paste0(theme_code, "axis.line.y = element_blank(")
      } else {
        theme_code <- paste0(theme_code, "axis.line.y = element_line(")
        # Paste theme settings from UI
        theme_code <- paste0(theme_code,
                             if (input$Axis_Y_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                  switch(input$Axis_Y_Linetype,
                                                                                         "Solide" = "solid",
                                                                                         "Gestrichelt" = "dashed",
                                                                                         "Gepunkted" = "dotted",
                                                                                         "Punktgestrichelt" = "dotdash",
                                                                                         "Langgestrichen" = "longdash",
                                                                                         "Doppelt gestrichelt" = "twodash"
                                                                                  )) else "",
                             if (!is.na(input$Axis_Y_Size)) sprintf("size = %.1f, ", input$Axis_Y_Size) else "",
                             if (input$Axis_Y_Color != "") sprintf("colour = '%s', ", input$Axis_Y_Color) else "")
      }
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.10 X Axis Ticks Theme ###############
    # Check if theme should be adjusted for X Axis Ticks length
    if (!is.na(input$Axis_X_Ticks_Length)) {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      theme_code <- paste0(theme_code, sprintf("axis.ticks.length.x = unit('%.1f', 'pt')", input$Axis_X_Ticks_Length))
    }
    # Check if theme should be adjusted for X Axis Ticks
    if (input$Axis_X_Ticks_Linetype != "Gemäss Theme" || !is.na(input$Axis_X_Ticks_Size) || input$Axis_X_Ticks_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for X Axis Ticks theme
      theme_code <- paste0(theme_code, "axis.ticks.x = element_line(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Axis_X_Ticks_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                      switch(input$Axis_X_Ticks_Linetype,
                                                                                             "Solide" = "solid",
                                                                                             "Gestrichelt" = "dashed",
                                                                                             "Gepunkted" = "dotted",
                                                                                             "Punktgestrichelt" = "dotdash",
                                                                                             "Langgestrichen" = "longdash",
                                                                                             "Doppelt gestrichelt" = "twodash"
                                                                                      )) else "",
                           if (!is.na(input$Axis_X_Ticks_Size)) sprintf("size = %.1f, ", input$Axis_X_Ticks_Size) else "",
                           if (input$Axis_X_Ticks_Color != "") sprintf("colour = '%s', ", input$Axis_X_Ticks_Color) else "")
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.11 Y Axis Ticks Theme ###############
    # Check if theme should be adjusted for Y Axis Ticks length
    if (!is.na(input$Axis_Y_Ticks_Length)) {
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      theme_code <- paste0(theme_code, sprintf("axis.ticks.length.y = unit('%.1f', 'pt')", input$Axis_Y_Ticks_Length))
    }
    # Check if theme should be adjusted for Y Axis Ticks
    if (input$Axis_Y_Ticks_Linetype != "Gemäss Theme" || !is.na(input$Axis_Y_Ticks_Size) || input$Axis_Y_Ticks_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for Y Axis Ticks theme
      theme_code <- paste0(theme_code, "axis.ticks.y = element_line(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Axis_Y_Ticks_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                      switch(input$Axis_Y_Ticks_Linetype,
                                                                                             "Solide" = "solid",
                                                                                             "Gestrichelt" = "dashed",
                                                                                             "Gepunkted" = "dotted",
                                                                                             "Punktgestrichelt" = "dotdash",
                                                                                             "Langgestrichen" = "longdash",
                                                                                             "Doppelt gestrichelt" = "twodash"
                                                                                      )) else "",
                           if (!is.na(input$Axis_Y_Ticks_Size)) sprintf("size = %.1f, ", input$Axis_Y_Ticks_Size) else "",
                           if (input$Axis_Y_Ticks_Color != "") sprintf("colour = '%s', ", input$Axis_Y_Ticks_Color) else "")
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.12 Major Gridlines X Theme ###############
    # Check if theme should be adjusted
    if (input$Major_Grid_X_Linetype != "Gemäss Theme" || !is.na(input$Major_Grid_X_Size) || input$Major_Grid_X_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Check if lines are wished
      if (input$Major_Grid_X_Linetype == "Keine"){
        # Set start of code for panel.grid.major.x
        theme_code <- paste0(theme_code, "panel.grid.major.x = element_blank(")
      } else {
        # Set start of code for panel.grid.major.x
        theme_code <- paste0(theme_code, "panel.grid.major.x = element_line(")
        # Paste theme settings from UI
        theme_code <- paste0(theme_code,
                             if (input$Major_Grid_X_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                        switch(input$Major_Grid_X_Linetype,
                                                                                               "Solide" = "solid", 
                                                                                               "Gestrichelt" = "dashed", 
                                                                                               "Gepunkted" = "dotted", 
                                                                                               "Punktgestrichelt" = "dotdash", 
                                                                                               "Langgestrichen" = "longdash", 
                                                                                               "Doppelt gestrichelt" = "twodash"
                                                                                        )) else "",
                             if (!is.na(input$Major_Grid_X_Size)) sprintf("size = %.1f, ", input$Major_Grid_X_Size) else "",
                             if (input$Major_Grid_X_Color != "") sprintf("colour = '%s', ", input$Major_Grid_X_Color) else "")
      }
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.13 Major Gridlines Y Theme ###############
    # Check if theme should be adjusted
    if (input$Major_Grid_Y_Linetype != "Gemäss Theme" || !is.na(input$Major_Grid_Y_Size) || input$Major_Grid_Y_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Check if lines are wished
      if (input$Major_Grid_Y_Linetype == "Keine"){
        theme_code <- paste0(theme_code, "panel.grid.major.y = element_blank(")
      } else {
        # Set start of code for panel.grid.major.y
        theme_code <- paste0(theme_code, "panel.grid.major.y = element_line(")
        # Paste theme settings from UI
        theme_code <- paste0(theme_code,
                             if (input$Major_Grid_Y_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                        switch(input$Major_Grid_Y_Linetype,
                                                                                               "Solide" = "solid", 
                                                                                               "Gestrichelt" = "dashed", 
                                                                                               "Gepunkted" = "dotted", 
                                                                                               "Punktgestrichelt" = "dotdash", 
                                                                                               "Langgestrichen" = "longdash", 
                                                                                               "Doppelt gestrichelt" = "twodash"
                                                                                        )) else "",
                             if (!is.na(input$Major_Grid_Y_Size)) sprintf("size = %.1f, ", input$Major_Grid_Y_Size) else "",
                             if (input$Major_Grid_Y_Color != "") sprintf("colour = '%s', ", input$Major_Grid_Y_Color) else "")
      }
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.14 Minor Gridlines X Theme ###############
    # Check if theme should be adjusted
    if (input$Minor_Grid_X_Linetype != "Gemäss Theme" || !is.na(input$Minor_Grid_X_Size) || input$Minor_Grid_X_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Check if lines are wished
      if (input$Minor_Grid_X_Linetype == "Keine"){
        theme_code <- paste0(theme_code, "panel.grid.minor.x = element_blank(")
      } else {
        # Set start of code for panel.grid.minor.x
        theme_code <- paste0(theme_code, "panel.grid.minor.x = element_line(")
        # Paste theme settings from UI
        theme_code <- paste0(theme_code,
                             if (input$Minor_Grid_X_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                        switch(input$Minor_Grid_X_Linetype,
                                                                                               "Solide" = "solid", 
                                                                                               "Gestrichelt" = "dashed", 
                                                                                               "Gepunkted" = "dotted", 
                                                                                               "Punktgestrichelt" = "dotdash", 
                                                                                               "Langgestrichen" = "longdash", 
                                                                                               "Doppelt gestrichelt" = "twodash"
                                                                                        )) else "",
                             if (!is.na(input$Minor_Grid_X_Size)) sprintf("size = %.1f, ", input$Minor_Grid_X_Size) else "",
                             if (input$Minor_Grid_X_Color != "") sprintf("colour = '%s', ", input$Minor_Grid_X_Color) else "")
      }
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.15 Major Gridlines Y Theme ###############
    # Check if theme should be adjusted
    if (input$Minor_Grid_Y_Linetype != "Gemäss Theme" || !is.na(input$Minor_Grid_Y_Size) || input$Minor_Grid_Y_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Check if lines are wished
      if (input$Minor_Grid_Y_Linetype == "Keine"){
        theme_code <- paste0(theme_code, "panel.grid.minor.y = element_blank(")
      } else {
        # Set start of code for panel.grid.minor.y
        theme_code <- paste0(theme_code, "panel.grid.minor.y = element_line(")
        # Paste theme settings from UI
        theme_code <- paste0(theme_code,
                             if (input$Minor_Grid_Y_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                        switch(input$Minor_Grid_Y_Linetype,
                                                                                               "Solide" = "solid", 
                                                                                               "Gestrichelt" = "dashed", 
                                                                                               "Gepunkted" = "dotted", 
                                                                                               "Punktgestrichelt" = "dotdash", 
                                                                                               "Langgestrichen" = "longdash", 
                                                                                               "Doppelt gestrichelt" = "twodash"
                                                                                        )) else "",
                             if (!is.na(input$Minor_Grid_Y_Size)) sprintf("size = %.1f, ", input$Minor_Grid_Y_Size) else "",
                             if (input$Minor_Grid_Y_Color != "") sprintf("colour = '%s', ", input$Minor_Grid_Y_Color) else "")
      }
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.16 Plot Background Theme ###############
    # Check if theme should be adjusted
    if (input$Plot_Background_Color != "" || input$Plot_Background_Linetype != "Gemäss Theme" 
        || !is.na(input$Plot_Background_Size) || input$Plot_Background_Line_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for plot.background
      theme_code <- paste0(theme_code, "plot.background = element_rect(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Plot_Background_Color != "") sprintf("fill = '%s', ", input$Plot_Background_Color) else "",
                           if (input$Plot_Background_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                         switch(input$Plot_Background_Linetype,
                                                                                                "Solide" = "solid", 
                                                                                                "Gestrichelt" = "dashed", 
                                                                                                "Gepunkted" = "dotted", 
                                                                                                "Punktgestrichelt" = "dotdash", 
                                                                                                "Langgestrichen" = "longdash", 
                                                                                                "Doppelt gestrichelt" = "twodash"
                                                                                         )) else "",
                           if (!is.na(input$Plot_Background_Size)) sprintf("linewidth = %.1f, ", input$Plot_Background_Size) else "",
                           if (input$Plot_Background_Line_Color != "") sprintf("colour = '%s', ", input$Plot_Background_Line_Color) else "")
      # Remove trailing comma and close the element-function
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.17 Panel Background Theme ###############
    # Check if theme should be adjusted
    if (input$Panel_Background_Color != "" || input$Panel_Background_Linetype != "Gemäss Theme" 
        || !is.na(input$Panel_Background_Size) || input$Panel_Background_Line_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for panel.background
      theme_code <- paste0(theme_code, "panel.background = element_rect(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Panel_Background_Color != "") sprintf("fill = '%s', ", input$Panel_Background_Color) else "",
                           if (input$Panel_Background_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                          switch(input$Panel_Background_Linetype,
                                                                                                 "Solide" = "solid", 
                                                                                                 "Gestrichelt" = "dashed", 
                                                                                                 "Gepunkted" = "dotted", 
                                                                                                 "Punktgestrichelt" = "dotdash", 
                                                                                                 "Langgestrichen" = "longdash", 
                                                                                                 "Doppelt gestrichelt" = "twodash"
                                                                                          )) else "",
                           if (!is.na(input$Panel_Background_Size)) sprintf("linewidth = %.1f, ", input$Panel_Background_Size) else "",
                           if (input$Panel_Background_Line_Color != "") sprintf("colour = '%s', ", input$Panel_Background_Line_Color) else "")
      # Remove trailing comma and close the element-function
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.18 Legend-Title Theme ###############
    # Check if theme should be adjusted
    if (input$Legend_Title_Font != "Gemäss Theme" || input$Legend_Title_Face != "Gemäss Theme" ||
        input$Legend_Title_Color != "" || !is.na(input$Legend_Title_Size) || input$Legend_Title_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for legend.title
      theme_code <- paste0(theme_code, "legend.title = element_text(")
      # Paste theme settings from UI
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
    
    
    
    
    
    
    
    
    
    ############### 5.19 Legend-Text Theme ###############
    # Check if theme should be adjusted
    if (input$Legend_Text_Font != "Gemäss Theme" || input$Legend_Text_Face != "Gemäss Theme" ||
        input$Legend_Text_Color != "" || !is.na(input$Legend_Text_Size) || input$Legend_Text_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for legend.text
      theme_code <- paste0(theme_code, "legend.text = element_text(")
      # Paste theme settings from UI
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
    
    
    
    
    
    
    
    
    
    ############### 5.20 Legend Background Theme ###############
    # Check if theme should be adjusted
    if (input$Legend_Background_Color != "" || input$Legend_Background_Linetype != "Gemäss Theme" 
        || !is.na(input$Legend_Background_Size) || input$Legend_Background_Line_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for legend.background
      theme_code <- paste0(theme_code, "legend.background = element_rect(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Legend_Background_Color != "") sprintf("fill = '%s', ", input$Legend_Background_Color) else "",
                           if (input$Legend_Background_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                         switch(input$Legend_Background_Linetype,
                                                                                                "Solide" = "solid", 
                                                                                                "Gestrichelt" = "dashed", 
                                                                                                "Gepunkted" = "dotted", 
                                                                                                "Punktgestrichelt" = "dotdash", 
                                                                                                "Langgestrichen" = "longdash", 
                                                                                                "Doppelt gestrichelt" = "twodash"
                                                                                         )) else "",
                           if (!is.na(input$Legend_Background_Size)) sprintf("linewidth = %.1f, ", input$Legend_Background_Size) else "",
                           if (input$Legend_Background_Line_Color != "") sprintf("colour = '%s', ", input$Legend_Background_Line_Color) else "")
      # Remove trailing comma and close the element-function
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.21 Legend Options Theme ###############
    # Check if legend position should be adjusted
    if (input$Legend_Position != "Gemäss Theme"){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for legend.position
      theme_code <- paste0(theme_code, sprintf("legend.position = '%s'",
                                               switch(input$Legend_Position,
                                                      "Keine" = "none", 
                                                      "Rechts" = "right", 
                                                      "Unten" = "bottom", 
                                                      "Links" = "left", 
                                                      "Oben" = "top", 
                                                      "Im Plot" = "inside", ")")))
    }
    
    
    # Check if Legend_Title_Position position should be adjusted
    if (input$Legend_Title_Position != "Gemäss Theme"){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for legend.title.position
      theme_code <- paste0(theme_code, sprintf("legend.title.position = '%s'",
                                               switch(input$Legend_Title_Position,
                                                      "Oben" = "top", 
                                                      "Rechts" = "right", 
                                                      "Unten" = "bottom", 
                                                      "Links" = "left", ")")))
    }
    
    
    # Check if Legend_Text_Position should be adjusted
    if (input$Legend_Text_Position != "Gemäss Theme"){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for legend.text.position
      theme_code <- paste0(theme_code, sprintf("legend.text.position = '%s'",
                                               switch(input$Legend_Text_Position,
                                                      "Oben" = "top", 
                                                      "Rechts" = "right", 
                                                      "Unten" = "bottom", 
                                                      "Links" = "left", ")")))
    }
    
    
    # Check if Legend_Text_Direction should be adjusted
    if (input$Legend_Text_Direction != "Gemäss Theme"){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Create code for legend.direction
      theme_code <- paste0(theme_code, sprintf("legend.direction = '%s'",
                                               switch(input$Legend_Text_Direction,
                                                      "Vertikal" = "vertical",
                                                      "Horizontal" = "horizontal", ")")))
    }
    
    
    # Check if Legend_Key_Width should be adjusted
    if (!is.na(input$Legend_Key_Width)){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Create code for legend.key.width
      theme_code <- paste0(theme_code, sprintf("legend.key.width = unit(%.1f, 'pt'", input$Legend_Key_Width))
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    # Check if Legend_Key_Height should be adjusted
    if (!is.na(input$Legend_Key_Height)){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Create code for legend.key.height
      theme_code <- paste0(theme_code, sprintf("legend.key.height = unit(%.1f, 'pt'", input$Legend_Key_Height))
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    # Check if Legend_Key_Spacing should be adjusted
    if (!is.na(input$Legend_Key_Spacing)){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Create code for legend.key.spacing
      theme_code <- paste0(theme_code, sprintf("legend.key.spacing = unit(%.1f, 'pt'", input$Legend_Key_Spacing))
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    # Check if Legend_Box_Spacing should be adjusted
    if (!is.na(input$Legend_Box_Spacing)){
      if (theme_code!=""){
        # Create new Line in Theme-Code if needed
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Create code for legend.box.spacing
      theme_code <- paste0(theme_code, sprintf("legend.box.spacing = unit(%.1f, 'pt'", input$Legend_Box_Spacing))
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.22 Facet-Row Background Theme ###############
    # Check if theme should be adjusted
    if (input$Stripe_X_Color != "" || input$Stripe_X_Linetype != "Gemäss Theme" 
        || !is.na(input$Stripe_X_Size) || input$Stripe_X_Line_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for strip.background.x
      theme_code <- paste0(theme_code, "strip.background.x = element_rect(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Stripe_X_Color != "") sprintf("fill = '%s', ", input$Stripe_X_Color) else "",
                           if (input$Stripe_X_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                  switch(input$Stripe_X_Linetype,
                                                                                         "Keine" = "blank", 
                                                                                         "Solide" = "solid", 
                                                                                         "Gestrichelt" = "dashed", 
                                                                                         "Gepunkted" = "dotted", 
                                                                                         "Punktgestrichelt" = "dotdash", 
                                                                                         "Langgestrichen" = "longdash", 
                                                                                         "Doppelt gestrichelt" = "twodash"
                                                                                  )) else "",
                           if (!is.na(input$Stripe_X_Size)) sprintf("linewidth = %.1f, ", input$Stripe_X_Size) else "",
                           if (input$Stripe_X_Line_Color != "") sprintf("colour = '%s', ", input$Stripe_X_Line_Color) else "")
      # Remove trailing comma and close the element-function
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.23 Facet-Column Background Theme ###############
    # Check if theme should be adjusted
    if (input$Stripe_Y_Color != "" || input$Stripe_Y_Linetype != "Gemäss Theme" 
        || !is.na(input$Stripe_Y_Size) || input$Stripe_Y_Line_Color != "") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for strip.background.y
      theme_code <- paste0(theme_code, "strip.background.y = element_rect(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Stripe_Y_Color != "") sprintf("fill = '%s', ", input$Stripe_Y_Color) else "",
                           if (input$Stripe_Y_Linetype != "Gemäss Theme") sprintf("linetype = '%s', ",
                                                                                  switch(input$Stripe_Y_Linetype,
                                                                                         "Keine" = "blank",
                                                                                         "Solide" = "solid", 
                                                                                         "Gestrichelt" = "dashed", 
                                                                                         "Gepunkted" = "dotted", 
                                                                                         "Punktgestrichelt" = "dotdash", 
                                                                                         "Langgestrichen" = "longdash", 
                                                                                         "Doppelt gestrichelt" = "twodash"
                                                                                  )) else "",
                           if (!is.na(input$Stripe_Y_Size)) sprintf("linewidth = %.1f, ", input$Stripe_Y_Size) else "",
                           if (input$Stripe_Y_Line_Color != "") sprintf("colour = '%s', ", input$Stripe_Y_Line_Color) else "")
      # Remove trailing comma and close the element-function
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }    
    
    
    
    
    
    
    
    
    
    ############### 5.24 Facet-Row Text Theme ###############
    # Check if theme should be adjusted
    if (input$Stripe_X_Font != "Gemäss Theme" || input$Stripe_X_Face != "Gemäss Theme" ||
        input$Stripe_X_Textcolor != "" || !is.na(input$Stripe_X_Textsize) || input$Stripe_X_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for strip.text.x
      theme_code <- paste0(theme_code, "strip.text.x = element_text(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Stripe_X_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                              switch(input$Stripe_X_Font,
                                                                                     "Sans Serife" = "sans",
                                                                                     "Serife" = "serif",
                                                                                     "Monospace" = "mono")) else "",
                           if (input$Stripe_X_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                              switch(input$Stripe_X_Face,
                                                                                     "Normal" = "plain",
                                                                                     "Fett" = "bold",
                                                                                     "Kursiv" = "italic",
                                                                                     "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Stripe_X_Textsize)) sprintf("size = %.1f, ", input$Stripe_X_Textsize) else "",
                           if (input$Stripe_X_Textcolor != "") sprintf("colour = '%s', ", input$Stripe_X_Textcolor) else "",
                           if (input$Stripe_X_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                   switch(input$Stripe_X_Alignment,
                                                                                          "Linksbündig" = 0,
                                                                                          "Mittig" = 0.5,
                                                                                          "Rechtsbündig" = 1)) else "")
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.25 Facet-Column Theme ###############
    # Check if theme should be adjusted
    if (input$Stripe_Y_Font != "Gemäss Theme" || input$Stripe_Y_Face != "Gemäss Theme" ||
        input$Stripe_Y_Textcolor != "" || !is.na(input$Stripe_Y_Textsize) || input$Stripe_Y_Alignment != "Gemäss Theme") {
      # Create new Line in Theme-Code if needed
      if (theme_code!=""){
        theme_code <- paste0(theme_code, ",\n        ")
      }
      # Set start of code for strip.text.y
      theme_code <- paste0(theme_code, "strip.text.y = element_text(")
      # Paste theme settings from UI
      theme_code <- paste0(theme_code,
                           if (input$Stripe_Y_Font != "Gemäss Theme") sprintf("family = '%s', ", 
                                                                              switch(input$Stripe_Y_Font,
                                                                                     "Sans Serife" = "sans",
                                                                                     "Serife" = "serif",
                                                                                     "Monospace" = "mono")) else "",
                           if (input$Stripe_Y_Face != "Gemäss Theme") sprintf("face = '%s', ", 
                                                                              switch(input$Stripe_Y_Face,
                                                                                     "Normal" = "plain",
                                                                                     "Fett" = "bold",
                                                                                     "Kursiv" = "italic",
                                                                                     "Fett & Kursiv" = "bold.italic")) else "",
                           if (!is.na(input$Stripe_Y_Textsize)) sprintf("size = %.1f, ", input$Stripe_Y_Textsize) else "",
                           if (input$Stripe_Y_Textcolor != "") sprintf("colour = '%s', ", input$Stripe_Y_Textcolor) else "",
                           if (input$Stripe_Y_Alignment != "Gemäss Theme") sprintf("hjust = %s, ", 
                                                                                   switch(input$Stripe_Y_Alignment,
                                                                                          "Linksbündig" = 0,
                                                                                          "Mittig" = 0.5,
                                                                                          "Rechtsbündig" = 1)) else "")
      # Remove trailing comma and close `element_text()`
      theme_code <- sub(", $", "", theme_code)
      theme_code <- paste0(theme_code, ")")
    }
    
    
    
    
    
    
    
    
    
    ############### 5.26 Append Theme-Code ###############
    # Check if theme-code was created
    if (theme_code!=""){
      # set beginning of theme code
      theme_code <- paste0(" +\n  theme(", sprintf(theme_code), ")")
      # Append theme-code to r-code
      r_code <- paste0(r_code, sprintf(theme_code))
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ############### 6. Return R-Code ###############
    r_code
  }
  )  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### 8. Set Output ###############
  ############### 8.1 Generate Plot ###############
  # Define Plot-Output
  output$dynamic_plot <- renderUI({

    #Set Plot-Output
    plotOutput(
      "plot",
      # Set dpi of plot-width
      width = paste0(input$plot_width_px, "px"),
      # Set dpi of plot-height
      height = paste0(input$plot_height_px, "px")
    )
  })
  
  # Render plot
  output$plot <- renderPlot({
    # Require data() and r_code()
    req(data(), r_code())
    
    # Save data as dataframe df
    df <- data()
    
    # Check if factor code is defined
    if (!is.null(factor_code()) && !identical(factor_code(), "")) {
      # Run factor code
      eval(parse(text = factor_code()))
    }
    
    # Run r_code to create the plot
    eval(parse(text = r_code()))
    
    # Return Plot
    return(q)
    # Set plot-resolution
  }, res = 96)
  
  
  
  
  
  
  
  
  
  ############### 8.2 Generate Code ###############
  ########## 8.2.1 Set variables ##########
  # Create reactive full code
  reactive_full_code <- reactive({
    
    # Save data as dataframe df
    df <- data()
    
    # Require r_code exists
    req(r_code())
    
    
    
    
    
    ########## 8.2.2 Set Code for packages ##########
    # Define Code-Line for ggplot2
    full_code <- "library(ggplot2)"
    
    # Define Code-Line for ggthemes if needed
    # Check if theme is by ggthemes
    if(input$plot_theme %in% c("Calc", "the Economist", "the Economist White", "Excel", "Few", "FiveThirtyEight", 
                               "Google Docs", "Highcharts JS", "Inversed Gray", "Solarized", "Solarized 2", "Solid", 
                               "Stata", "Tufte", "Wall Street Journal") |
       # Check if color-palette is by ggthemes
       input$Color_Palette %in% c("calc", "canva", "colorblind", 
                                  "economist", "excel", "excel_new", "few", "fivethirtyeight", "gdocs", 
                                  "hc", "pander", "ptol", "solarized", 
                                  "stata", "tableau", "wsj")) {
      # Add line to load ggthemes
      full_code <- paste0(full_code, "\nlibrary(ggthemes)")
    }
    
    # Define Code-Line for ggsci if needed
    # Check if color-palette is by ggsci
    if(input$Color_Palette %in% c("aas", "bmj", "cosmic", "d3", "flatui", 
                                  "frontiers", "futurama", "igv", "jama", "lancet", "locuszoom", 
                                  "material", "nejm", "npg", "observable", "rickandmorty", "simpsons", "startrek", 
                                  "tron", "uchicago", "ucscgb", "jco")) {
      # Add line to load ggsci
      full_code <- paste0(full_code, "\nlibrary(ggsci)")
    }
    
    
    
    
    
    ########## 8.2.3 Set Code to adjust Factors ##########
    # Check if factor_code exists
    if (!is.null(factor_code()) && !identical(factor_code(), "")) {
      # Paste additional lines
      full_code <- paste0(full_code, "\n")
      
      # Paste factor-code
      full_code <- paste0(full_code, factor_code())
    }
    
    
    
    
    
    ########## 8.2.4 Add r_code ##########
    # Add empty lines in code
    full_code <- paste0(full_code, "\n\n", r_code())
    
    
    
    
    
    ########## 8.2.5 Prepare final code ##########
    # Replace data() with data
    full_code <- gsub("data\\(\\)", "data", full_code)
    
    # Return full_code
    full_code
  })
  
  
  
  
  
  ########## 8.2.6 Print Code ##########
  # Print the reactive r_code
  output$rcode <- renderText({
    reactive_full_code()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### 9. Copy/Download ###############
  ############### 9.1 Add Code to clipboad ###############
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "",
      clipText = reactive_full_code(), 
      icon = icon("clipboard", class = "fa-3x"),
      placement = "top",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
    )
  })
  
  
  
  
  
  
  
  
  
  ############### 9.2 Download Plot ###############
  # Function to download Plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # Define File-name
      paste("ggpilot-", Sys.Date(), ".", input$file_format, sep = "")
    },
    content = function(file) {
      # Require r_code
      req(r_code())
      # Require that q exists
      req(exists("q"))
      # Require data
      req(df)
      # Save df for creating plot
      df <- data()
      
      # Rund factor code if defined
      if (!is.null(factor_code()) && factor_code() != "") {
        eval(parse(text = factor_code()))
      }
      
      # Create Plot by evaluating r_code
      eval(parse(text = r_code()))
      
      # Calculate the correct width and height for saving
      width_inch <- input$plot_width_px / 96
      height_inch <- input$plot_height_px / 96
      
      # If Plot should be downloaded as SVG image
      if (input$file_format == "svg") {
        svglite::svglite(file, width = width_inch, height = height_inch)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(q)
      # If Plot should be downloaded as JPEG or PNG image
      } else {
        # Save Plot
        ggsave(
          # Save under the defined Name
          filename = file,
          # Get plot (q) and save it
          plot = q,
          # Save in the Calculated Width
          width = width_inch,
          # Save in the Calculated Height
          height = height_inch,
          # DPI is set to 96
          dpi = 96, # DPI is set to 96
          # Save in the requested file format
          device = input$file_format
        )
      }
    }
  )
  
  
  
  
  
  
  
  
  
  ############### 9.3 Download Code ###############
  # Function to download r-code
  output$downloadCode <- downloadHandler(
    filename = function() {
      # Define File-name
      paste("ggpilot_code-", Sys.Date(), ".r", sep = "")
    },
    content = function(file) {
      # Save the Code
      code <- reactive_full_code()
      # Write the Code
      writeLines(code, file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############### 10. Set Footer ###############
  ############### 11.1 Footer UI ###############
  output$app_footer <- renderUI({
    # Get the selected language
    i18n$set_translation_language(if (!is.null(input$language)) input$language else "en")
    
    # Set Footer
    tags$footer(
      class = "app-footer",
      # Add Hyperlink
      tags$a(
        # Set link
        href   = "https://github.com/Mrihs/ggpilot",
        # Open in new window
        target = "_blank",
        # Set github-buttong
        class  = "github-btn",
        title = "Github",
        icon("github"),
        span(tr("footer.github"))
      ),
      div(
        class = "right-tools",
        # neuer runder Info-Button
        actionLink(
          inputId = "info_btn",
          label   = icon("info-circle"),
          class   = "info-btn",
          title = tags$span(class = "info-modal-title", tr("footer.info.title")),
          `aria-label` = tr("footer.info.tooltip")
        ),
        shinyWidgets::pickerInput(
          inputId  = "language",
          label    = NULL,
          choices  = c("en","de"),
          selected = if (!is.null(input$language)) input$language else "en",
          width    = "70px",
          choicesOpt = list(
            content = c(
              '<img src="en.svg" class="lang-icon" alt="EN" />',
              '<img src="de.svg" class="lang-icon" alt="DE" />'
            )
          ),
          options = list(dropupAuto = FALSE)
        )
      )
    )
  })
  
  
  
  
  
  
  
  
  
  ############### 11.2 Ino-Dialogue ###############
  observeEvent(input$info_btn, {
    showModal(modalDialog(
      title = tags$span(class = "info-modal-title", tr("footer.info.title")),  
      size = "l",
      easyClose = TRUE,
      footer = modalButton(tr("footer.info.close")),
      div(style = "white-space: pre-line;", tr_html("footer.info.text.html"))
    ))
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################### 11. Language Localization ####################
  ############### 11.1 Update Language ###############
  observeEvent(input$language, {
    req(input$language)
    i18n$set_translation_language(if (!is.null(input$language)) input$language else "en")
    
    
    
    
    
    
    
    
    
    ############### 11.2 Define Helper-Functions for Dropdown-Options ###############
    LINETYPES <- c("Solide","Gestrichelt","Gepunkted","Punktgestrichelt","Langgestrichen","Doppelt gestrichelt")
    ERRTYPES  <- c("Keiner","Standardabweichung","Konfidenzintervall","Standardfehler")
    TARGETS   <- c("Füllung","Linien","Füllung und Linien")
    
    
    
    
    
    
    
    
    
    ############### 11.2 Update Language Selection ###############
    shinyWidgets::updatePickerInput(
      session, "language",
      label    = NULL,
      choices  = c("en","de"),
      selected = input$language,
      choicesOpt = list(
        content = c(
          '<img src="en.svg" class="lang-icon" alt="EN" />',
          '<img src="de.svg" class="lang-icon" alt="DE" />'
        )
      )
    )
    
    
    
    
    
    
    
    
    
    ############### 11.4 Update Navigation-Buttons ###############
    updateActionButton(session, "btn_data",
                       label = tr("nav.data"))
    updateActionButton(session, "btn_plottype",
                       label = tr("nav.plottype"))
    updateActionButton(session, "btn_variables",
                       label = tr("nav.variables"))
    updateActionButton(session, "btn_plot_options",
                       label = tr("nav.options"))
    updateActionButton(session, "btn_text",
                       label = tr("nav.text"))
    updateActionButton(session, "btn_layout",
                       label = tr("nav.layout"))
    updateActionButton(session, "btn_download",
                       label = tr("nav.download"))
    
    
    
    
    
    
    
    
    ############### 11.9 Update Data Sidebar ###############
    session$sendCustomMessage(
      "setFileInputLang",
      list(
        label      = tr("data.select_dataset"),
        browse     = tr("data.browse"),
        placeholder= tr("data.no_file")
      )
    )

    
    
    
    
    
    ############### 11.3 Update Buttons for Plot-Type ###############
    updateActionButton(session, "plot_bar",
                       label = HTML(sprintf('<img src="Icon_Bar.png" height="100px"> <br> %s', tr("plot.bar"))))
    updateActionButton(session, "plot_line",
                       label = HTML(sprintf('<img src="Icon_Line.png" height="100px"> <br> %s', tr("plot.line"))))
    updateActionButton(session, "plot_box",
                       label = HTML(sprintf('<img src="Icon_Box.png" height="100px"> <br> %s', tr("plot.box"))))
    updateActionButton(session, "plot_scatter",
                       label = HTML(sprintf('<img src="Icon_Scatter.png" height="100px"> <br> %s', tr("plot.scatter"))))
        
    
    
    
    
    
    
    
    
    ############### 11.4 Update Variables Sidebar ###############
    # Create Helper function to change text
    setTxt <- function(id, key) {
      session$sendCustomMessage('setText', list(id = id, text = tr(key)))
    }
    
    # Headers
    setTxt("x_title",        "variables.x_title")
    setTxt("y_title",        "variables.y_title")
    setTxt("group_title",    "variables.group_title")
    setTxt("grid_col_title", "variables.grid_col_title")
    setTxt("grid_row_title", "variables.grid_row_title")
    
    # Instructions to reorder levels
    setTxt("x_reorder_title",        "variables.reorder_levels")
    setTxt("y_reorder_title",        "variables.reorder_levels")
    setTxt("group_reorder_title",    "variables.reorder_levels")
    setTxt("grid_col_reorder_title", "variables.reorder_levels")
    setTxt("grid_row_reorder_title", "variables.reorder_levels")
    
    
    
    
    
    
    
    
    
    ############### 11.5 Update Options Sidebar ###############
    ### Change UI-Text
    setTxt("opt_theme_title", "options.theme.title")
    setTxt("opt_palette_title", "options.palette.title")
    setTxt("options_plotsize_title", "options.plotsize.title")
    setTxt("opt_range_title", "options.range.title")
    setTxt("options_grouping_space", "options.grouping.space")
    setTxt("options_barplot", "options.barplot")
    setTxt("options_lineplot", "options.lineplot")
    setTxt("options_errorbars", "options.errorbar")
    setTxt("options_scatterplot", "options.scatterplot")
    setTxt("options_regressionline", "options.regressionline")
    
    
    ### Change Options
    updateSelectInput(
      session, "Color_Palette",
      label    = tr("options.palette.select"),
      selected = input$Color_Palette
    )    
    updateSelectInput(session, "color_palette_target",
                      label   = tr("options.palette.apply_to"),
                      choices = setNames(TARGETS, c(tr("options.palette.target.fill"),
                                                    tr("options.palette.target.line"),
                                                    tr("options.palette.target.both"))),
                      selected = input$color_palette_target
    )
    updateSelectInput(session, "error_type",
                      label   = tr("options.errorbar.unit"),
                      choices = setNames(ERRTYPES, c(tr("options.errorbar.none"),
                                                     tr("options.errorbar.sd"),
                                                     tr("options.errorbar.ci"),
                                                     tr("options.errorbar.se"))),
                      selected = input$error_type
    )
    updateSelectInput(session, "lineplot_line_type",
                      label   = tr("options.linetype"),
                      choices = setNames(LINETYPES, c(tr("options.linetype.solid"), tr("options.linetype.dashed"), tr("options.linetype.dotted"),
                                                      tr("options.linetype.pointdash"), tr("options.linetype.longdash"), tr("options.linetype.twodash"))),
                      selected = input$lineplot_line_type
    )
    updateSelectInput(session, "scater_line_type",
                      label   = tr("options.linetype"),
                      choices = setNames(LINETYPES, c(tr("options.linetype.solid"), tr("options.linetype.dashed"), tr("options.linetype.dotted"),
                                                      tr("options.linetype.pointdash"), tr("options.linetype.longdash"), tr("options.linetype.twodash"))),
                      selected = input$scater_line_type
    )
    updateNumericInput(
      session, "plot_width_px",
      label    = tr("options.plotsize.width_px"),
      value = input$plot_width_px
    )
    updateNumericInput(
      session, "plot_height_px",
      label    = tr("options.plotsize.height_px"),
      value = input$plot_height_px
    )
    updateNumericInput(session, 
                       inputId = "x_axis_min", 
                       label = tr("options.range.min"), 
                       value = input$x_axis_min )
    updateNumericInput(session,
                       inputId = "x_axis_max", 
                       label = tr("options.range.max"),
                       value = input$x_axis_max )
    updateNumericInput(session, 
                       inputId = "y_axis_min", 
                       label = tr("options.range.min"),
                       value = input$y_axis_min )
    updateNumericInput(session,
                       inputId = "y_axis_max", 
                       label = tr("options.range.max"),
                       value = input$y_axis_max )
    updateNumericInput(session, 
                       inputId = "dodge_value", 
                       label = tr("options.dodge.value"),
                       value = input$dodge_value)
    updateNumericInput(session, 
                       inputId = "barplot_width", 
                       label = tr("options.barplot.width"),
                       value = input$barplot_width)
    updateNumericInput(session, 
                       inputId = "lineplot_width", 
                       label = tr("options.lineplot.width"),
                       value = input$lineplot_width)
    updateNumericInput(session, 
                       inputId = "error_mult", 
                       label = tr("options.errorbar.mult"),
                       value = input$error.mult)
    updateNumericInput(session, 
                       inputId = "error_width", 
                       label = tr("options.errorbar.width"),
                       value = input$error_width)
    updateNumericInput(session, 
                       inputId = "error_size", 
                       label = tr("options.errorbar.size"),
                       value = input$error_size)
    updateNumericInput(session, 
                       inputId = "scatterpoint_size", 
                       label = tr("options.scatterpoint.size"),
                       value = input$scatterpoint_size)
    updateNumericInput(session, 
                       inputId = "scater_line_size", 
                       label = tr("options.linewidth"),
                       value = input$scater_line_size)
    updateCheckboxInput(session,
                        inputId = "exact_axis_range", 
                        label = tr("options.range.expand"))
    updateCheckboxInput(session, 
                        inputId = "show_line", 
                        label = tr("options.regressionline.show"))
    updateCheckboxInput(session, 
                        inputId = "scater_line_full_range", 
                        label = tr("options.regressionline.expand"))
    updateCheckboxInput(session, 
                        inputId = "scater_line_show_se", 
                        label = tr("options.regressionline.showse"))
    
    session$sendCustomMessage('setText', list(id="opt_xaxis_lbl", text=tr("options.range.xaxis")))
    session$sendCustomMessage('setText', list(id="opt_yaxis_lbl", text=tr("options.range.yaxis")))

    ### Change UI of Custom Color Palette
    # Change language of Buttons
    updateActionButton(session, "add",         
                       label = tr("options.custom.colorpalette.add"))
    updateActionButton(session, "remove_last", 
                       label = tr("options.custom.colorpalette.remove"))
    
    # Localise Error Message
    if (manual_colors$count > 0) {
      for (i in seq_len(manual_colors$count)) {
        updateTextInput(
          session,
          paste0("vector_", i),
          label = sprintf("%s %d", tr("label.color"), i),
          placeholder = tr("options.custom.colorpalette.placeholder")
        )
        session$sendCustomMessage(
          "setText",
          list(id = paste0("vector_", i, "_error"), text = tr("options.custom.colorpalette.error"))
        )
      }
    }
    

    
    
    
    
    
    
    ############### 11.6 Update Text Sidebar ###############
    # Title
    updateTextInput(session, "plot_title",
                    label = tr("text.title"),
                    placeholder = tr("text.title_placeholder"))
    # Subtitle
    updateTextInput(session, "plot_subtitle",
                    label = tr("text.subtitle"),
                    placeholder = tr("text.subtitle_placeholder"))
    # X-Axis Title
    updateTextInput(session, "x_axis_title",
                    label = tr("text.x_axis"),
                    placeholder = tr("text.x_axis_placeholder"))
    # Y-Axis Title
    updateTextInput(session, "y_axis_title",
                    label = tr("text.y_axis"),
                    placeholder = tr("text.y_axis_placeholder"))
    # Legend-Title
    updateTextInput(session, "legend_title",
                    label = tr("text.legend"),
                    placeholder = tr("text.legend_placeholder"))
    
    
    
    
    
    
    
    
    ############### 11.8 Update Layout Sidebar ###############
    setTxt(id = "layout_collapse_header", key = "layout.collapse.header")
    setTxt(id = "layout_h3_title", key = "layout.h3.title")
    setTxt(id = "layout_h3_subtitle", key = "layout.h3.subtitle")
    setTxt(id = "layout_collapse_axis_title", key = "layout.collapse.axis.title")
    setTxt(id = "layout_h3_title_xaxis", key = "layout.h3.xaxis")
    setTxt(id = "layout_h3_title_yaxis", key = "layout.h3.yaxis")
    setTxt(id = "layout_collapse_axis_text", key = "layout.collapse.axis.text")
    setTxt(id = "layout_h3_xaxis_text", key = "layout.h3.xaxis")
    setTxt(id = "layout_h3_yaxis_text", key = "layout.h3.yaxis")
    setTxt(id = "layout_collapse_axis_lines", key = "layout.collapse.axis.lines")
    setTxt(id = "layout_h3_xaxis_lines", key = "layout.h3.xaxis")
    setTxt(id = "layout_h3_yaxis_lines", key = "layout.h3.yaxis")
    setTxt(id = "layout_collapse_axis_ticks", key = "layout.collapse.axis.ticks")
    setTxt(id = "layout_h3_xaxis_ticks", key = "layout.h3.xaxis")
    setTxt(id = "layout_h3_yaxis_ticks", key = "layout.h3.yaxis")
    setTxt(id = "layout_collapse_major_grid", key = "layout.collapse.major.grid")
    setTxt(id = "layout_h3_xaxis_major_grid", key = "layout.h3.xaxis")
    setTxt(id = "layout_h3_yaxis_major_grid", key = "layout.h3.yaxis")
    setTxt(id = "layout_collapse_minor_grid", key = "layout.collapse.minor.grid")
    setTxt(id = "layout_h3_xaxis_minor_grid", key = "layout.h3.xaxis")
    setTxt(id = "layout_h3_yaxis_minor_grid", key = "layout.h3.yaxis")
    setTxt(id = "layout_collapse_background", key = "layout.collapse.background")
    setTxt(id = "layout_h3_plot", key = "layout.h3.plot")
    setTxt(id = "layout_h3_panel", key = "layout.h3.panel")
    setTxt(id = "layout_collapse_legend", key = "layout.collapse.legend")
    setTxt(id = "layout_h3_legend_title", key = "layout.h3.legend.title")
    setTxt(id = "layout_h3_items", key = "layout.h3.items")
    setTxt(id = "layout_collapse_facets_background", key = "layout.collapse.facets.background")
    setTxt(id = "layout_collapse_legend_background", key = "layout.collapse.legend.background")
    setTxt(id = "layout_h3_legend_box", key = "layout.h3.legend.box")
    setTxt(id = "layout_h3_arrangement", key = "layout.h3.arrangement")
    setTxt(id = "layout_h3_sizes", key = "layout.h3.sizes")
    setTxt(id = "layout_collapse_legend_options", key = "layout.collapse.legend.options")
    setTxt(id = "layout_h3_columns_facets", key = "layout.h3.columns")
    setTxt(id = "layout_h3_rows_facets", key = "layout.h3.rows")
    setTxt(id = "layout_collapse_facets_text", key = "layout.collapse.facets.text")
    setTxt(id = "layout_h3_columns_facets_text", key = "layout.h3.columns")
    setTxt(id = "layout_h3_rows_facets_text", key = "layout.h3.rows")
    
    # For all Font-Settings
    for (id in c("Title_Font","Subtitle_Font","X_Axis_Title_Font","Y_Axis_Title_Font",
                 "Stripe_X_Font", "Stripe_Y_Font", "Axis_X_Text_Font", "Axis_Y_Text_Font",
                 "Legend_Title_Font", "Legend_Text_Font")) {
      # Update the respective Input
      updateSelectInput(session, id, label = tr("label.font"), choices = font_choices(), selected = input[[id]])
    }
    # For all Face-Settings
    for (id in c("Title_Face","Subtitle_Face","X_Axis_Title_Face","Y_Axis_Title_Face",
                 "Axis_X_Text_Face", "Axis_Y_Text_Face", "Legend_Title_Face", "Legend_Text_Face",
                 "Stripe_X_Face", "Stripe_Y_Face")) {
      # Update the respective Input
      updateSelectInput(session, id, label = tr("label.face"), choices = font_choices(), selected = input[[id]])
    }
    # For all Color-Settings
    for (id in c("Title_Color","Subtitle_Color","X_Axis_Title_Color","Y_Axis_Title_Color",
                 "Axis_X_Text_Color", "Axis_Y_Text_Color", "Legend_Title_Color", "Legend_Text_Color",
                 "Stripe_X_Textcolor", "Stripe_Y_Textcolor")) {
      # Update the respective Input
      updateTextInput(session, id, label = tr("label.color"), placeholder = tr("placeholder.color"))
    }
    # For all Text-Size Settings
    for (id in c("Title_Size","Subtitle_Size","X_Axis_Title_Color","Y_Axis_Title_Color",
                 "X_Axis_Title_Size", "Y_Axis_Title_Size", "Axis_X_Text_Size", "Axis_Y_Text_Size", "Legend_Title_Size", "Legend_Text_Size",
                 "Stripe_X_Textsize", "Stripe_Y_Textsize")) { 
      # Update the respective Input
      updateNumericInput(session, id, label = tr("label.size"))
    }
    # For all Text-Rotation Settings
    for (id in c("Axis_X_Text_Rotation","Axis_Y_Text_Rotation")) { 
      # Update the respective Input
      updateNumericInput(session, id, label = tr("label.rotation"))
    }
    # For all Text-Alignment Settings
    for (id in c("Title_Alignment","Subtitle_Alignment","X_Axis_Title_Face","Y_Axis_Title_Face",
                 "X_Axis_Title_Alignment", "Y_Axis_Title_Alignment", "Legend_Title_Alignment", "Legend_Text_Alignment",
                 "Stripe_X_Alignment", "Stripe_Y_Alignment")) {
      # Update the respective Input
      updateSelectInput(session, id, label = tr("label.align"), choices = align_h_choices(), selected = input[[id]])
    }    
    # For all Horicontal Text-Alignment Settings
    for (id in c("Axis_X_Text_H_Alignment", "Axis_Y_Text_H_Alignment")) {
      # Update the respective Input
      updateSelectInput(session, id, label = tr("label.align.h"), choices = align_h_choices(), selected = input[[id]])
    }    
    # For all Vertical Text-Alignment Settings
    for (id in c("Axis_X_Text_V_Alignment", "Axis_Y_Text_V_Alignment")) {
      # Update the respective Input
      updateSelectInput(session, id, label = tr("label.align.v"), choices = align_v_choices(), selected = input[[id]])
    }
    # For all Linetype Settings
    for (id in c("Axis_X_Linetype","Axis_Y_Linetype","Axis_X_Ticks_Linetype","Axis_Y_Ticks_Linetype",
                 "Major_Grid_X_Linetype", "Major_Grid_Y_Linetype", "Minor_Grid_X_Linetype", "Minor_Grid_Y_Linetype",
                 "Plot_Background_Linetype", "Panel_Background_Linetype", "Legend_Background_Linetype", 
                 "Stripe_X_Linetype", "Stripe_Y_Linetype")) {
      # Update the respective Input
      updateSelectInput(session, id, label = tr("options.linetype"), choices = linetype_choices_all(), selected = input[[id]])
    } 
    # For all Linewidth-Settings
    for (id in c("Axis_X_Size","Axis_Y_Size","Axis_X_Ticks_Size","Axis_Y_Ticks_Size",
                 "X_Axis_Title_Alignment", "Major_Grid_Y_Size", "Minor_Grid_X_Size", "Minor_Grid_Y_Size",
                 "Plot_Background_Size", "Panel_Background_Size", "Legend_Background_Size", 
                 "Stripe_X_Size", "Stripe_Y_Size")) {
      # Update the respective Input
      updateNumericInput(session, id, label = tr("options.linewidth"))
    }     
    # For all Linecolor Settings
    for (id in c("Axis_X_Color","Axis_Y_Color","Axis_X_Ticks_Color","Axis_Y_Ticks_Color",
                 "Major_Grid_X_Color", "Major_Grid_Y_Color", "Minor_Grid_X_Color", "Minor_Grid_Y_Color",
                 "Plot_Background_Line_Color", "Panel_Background_Line_Color", "Legend_Background_Line_Color", 
                 "Stripe_X_Line_Color", "Stripe_Y_Line_Color")) {
      # Update the respective Input
      updateTextInput(session, id, label = tr("options.linecolor"), placeholder = tr("placeholder.color"))
    } 
    # For all Background-Color Settings
    for (id in c("Plot_Background_Color","Panel_Background_Color","Legend_Background_Color",
                 "Stripe_X_Color", "Stripe_Y_Color", "Y_Axis_Title_Alignment", "Legend_Title_Alignment", "Legend_Text_Alignment",
                 "Stripe_X_Alignment", "Stripe_Y_Alignment")) {
      # Update the respective Input
      updateTextInput(session, id, label = tr("options.background.color"), placeholder = tr("placeholder.color"))
    } 
    # For all Linelength-Settings
    for (id in c("Axis_X_Ticks_Length","Axis_Y_Ticks_Length")) {
      # Update the respective Input
      updateNumericInput(session, id, label = tr("options.linelength"))
    }
    # Legend Arrangement Settings
    updateSelectInput(session, "Legend_Position",    label = tr("layout.legend.position"),        choices = legend_pos_choices(),     selected = input$Legend_Position)
    updateSelectInput(session, "Legend_Title_Position",    label = tr("layout.legend.title.position"),        choices = legend_text_pos_choices(),     selected = input$Legend_Title_Position)
    updateSelectInput(session, "Legend_Text_Position",    label = tr("layout.legend.text.position"),        choices = legend_text_pos_choices(),     selected = input$Legend_Text_Position)
    updateSelectInput(session, "Legend_Text_Direction",    label = tr("layout.legend.direction"),        choices = legend_dir_choices(),     selected = input$Legend_Text_Direction)
    # Legend Sizes Settings
    updateNumericInput(session,"Legend_Key_Width",    label = tr("layout.legend.key.width"))
    updateNumericInput(session,"Legend_Key_Height",    label = tr("layout.legend.key.height"))
    updateNumericInput(session,"Legend_Key_Spacing",    label = tr("layout.legend.key.spaicng"))
    updateNumericInput(session,"Legend_Box_Spacing",    label = tr("layout.legend.box.spacing"))
    
    
    
    
    
    
    
    
    ############### 11.9 Update Download Sidebar ###############
    setTxt("download_plot_hdr",        "download.plot")
    setTxt("download_code_hdr",        "download.r_code")
    setTxt("download_plot_btn",        "download.download_plot")
    setTxt("download_code_btn",        "download.download_code")
    setTxt("download_file_format_lbl", "download.file_format")
    setTxt("file_label",               "data.select_dataset")
  })
}




















#################### 12. Run App ####################
shinyApp(ui = ui, server = server)