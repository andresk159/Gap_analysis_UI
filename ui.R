#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(pacman)
pacman::p_load(shiny, shinydashboard, leaflet, raster, rgdal, rgeos, sp, rsconnect, ggplot2, shinyFiles, shinyBS, shinyjs, yaml, shinyWidgets )
# Define UI for application that draws a histogram
urls <- read.csv("www/downloadable_files.csv")
source("www/helpers.R", local = TRUE)

header <- dashboardHeader(
  title = "Gap analysis UI",
  dropdownMenuOutput("messageMenu")
  # tags$li(a(href = 'http://shinyapps.company.com',
  #           icon("power-off"),
  #           title = "Back to Apps Home"),
  #         class = "dropdown")
)

sidebar <- dashboardSidebar(
              sidebarMenu(id = "menu", 
                           menuItem(" Introduction", tabName = "intro", icon = icon("far fa-sun"), selected = T),
                           menuItem(" Assistants", tabName = "tab1", icon = icon("far fa-cogs")),
                           menuItem(" Cost Distance", tabName = "tab2", icon = icon("fab fa-contao")),
                           menuItem(" SDM", tabName = "tab3", icon = icon("fas fa-globe-americas"))
              ) 
                           
                           )
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "intro", tags$html('<p> Nothing to see here now </p>') 
            
            
            ),
    tabItem(tabName = "tab1", 
            
            navbarPage(title = icon("far fa-cogs"),
              tabPanel(title = "Arrange Dirs system",
                       sidebarLayout(
                         sidebarPanel(width = 4, id = "write_crop_info",
                           h3("Dir creator assistant"),
                           textInput(inputId =  "set.crop.name", label = "Please write Crop name"),
                           textInput(inputId =  "set.level.name", label = "Please write Race name"),
                           textInput(inputId =  "selected.root.folder", label = "Please select a dir path", value = "No dir path selected"),
                           shinyDirButton(id = "select_path_btn", label = "Choose Dir", title = "Select folder to store all file system", buttonType = "default",
                                          class = NULL, icon = NULL, style = NULL),
                           div(id ="separator", style = "width:100px; height:15px"),
                           radioGroupButtons(inputId = "update_scripts", label = "Update scripts", choices = c("No","Yes"), individual = F, status = "primary",
                                             checkIcon = list( yes = tags$i(class = "fa fa-circle", 
                                                                            style = "color: darkgray"),
                                                               no = tags$i(class = "fa fa-circle-o", 
                                                                           style = "color: darkgray"))
                           ),
                           bsButton("create_dirs", size="default",label = "Create dirs", block = F, style="primary"),
                           bsTooltip(id = "create_dirs", title = "Create directories", placement = "right", trigger = "hover")
                           
                         ),
                         mainPanel(
                           tags$head(HTML(
                             '<style> 
                             .card {
                             box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
                             padding: 8px;
                             border: 1px solid #ccc;
                             border-radius: 5px; /* 5px rounded corners */
                             max-width: 800px;
                             margin: 0 auto;
                             background-color:white;
                             }
                             </style>'
                           )),
                           div(id = "text_1", class= "card",
                               h3(tags$strong("****Welcome to the Dir creator assistant***")),
                               tags$hr(),
                               em("The assistant will guide you through all process to set up working directories necessaries for 
                                  the Gap analysis."),
                               h4("1. Write the Crop name in the Crop name field."),
                               h4("2. On the race name field please write only one name for the race, genetic structure or group 
                                  to analyze (This app only can run one race per time)."),
                               h4("3. Click the button \"choose Dir\" and search the destination folder in which the directory structure will
                                  be created."),
                               h4("4. Finally, click on \"Create dirs\" button, this will create all necessary folder to store inputs, results, etc... 
                                  and also will download the most updated scripts from the GitHub", a("CIAT repository", href = "https://github.com/CIAT-DAPA/gap_analysis_landraces"), ".")
                               )
                           
                         )#end main panel
                         
                         
                       )
  
            ),
            tabPanel("Define study area",
                     sidebarLayout(
                       sidebarPanel(
                         h3("Region creator assistant"),
                         radioGroupButtons("choose_1", label = "", choices  = c("Create new mask" = 1, "Import existing mask"= 2), status = "primary", justified = T ),
                         conditionalPanel(condition = "input.choose_1 == '1'",
                                          
                                          selectInput("area_selector", label = "Select one region:", choices = c("Wolrd" = 0,
                                                                                                                 "America" = 19,
                                                                                                                 "Europe" = 150,
                                                                                                                 "Asia" = 142,
                                                                                                                 "Africa" = 2,
                                                                                                                 "Oceania" = 9,
                                                                                                                 "---Custom region---" = 8), selected = 0),
                                          #haciendo el conditional panel
                                          conditionalPanel( condition = "input.area_selector == 8",
                                                            checkboxGroupInput("chk_bx_gr", "Countries selected:", choices = "None", selected = "None")
                                                            
                                          ),
                                          textInput("mask_name", label = "Set a name for raster's mask"),
                                          useShinyjs(),
                                          withBusyIndicatorUI(
                                            button = bsButton("create_mask", size="default",label = "Create Mask", style="primary")
                                            
                                          ),
                                          bsTooltip(id = "create_mask", title = "Create Raster Mask", placement = "left", trigger = "hover")),
                         conditionalPanel(condition = "input.choose_1 == '2'",
                                          fileInput("mask_path", "Select mask file:",multiple = FALSE,accept = c(".tif"))
                                          
                                          )
                         
                       ),
                       mainPanel(
                         div(id= "text_2", class = "card",
                             h3(tags$strong("***Welcome to the Mask area creator Assistant***")),
                             tags$hr(),
                             em("the assistant will support you to define the strategic areas in which the Gap Analysis has
                                to be performed."),
                             h5("Feel free to create a continental mask selecting it from the", tags$strong("Region Menu"), "or
                                create a customized study region clicking over the countries, and do not forget to write a name for the mask."),
                             h5(tags$strong("file output:")),
                             h5(tags$strong(".../input_data/mask/mask_<mask_name>.tif"))
                             
                             ),
                         leafletOutput("map_selector")
                         
                       )
                     )
              
              
            ),
            tabPanel("Download Inputs",
              sidebarLayout(
                sidebarPanel(
                  h3("Download Helper"),
                  selectInput("download_spam", "Download MapSPAM data?", choices = c( "No" = 1, "Yes" = 2)),
                  conditionalPanel(condition = "input.download_spam == 2",
                                   selectInput("slct_crop_name", "Select MapSPAM Crop:", choices = unique(urls$screen_name) )
                                   ),
                  selectInput("download_clim", "Download ENVIREM, WorldClim and anthropogenic data?", choices = c("No" = "FALSE", "Yes" = "TRUE")),
                  div(style="float:right;width:95px;background-color:lightblue",
                      withBusyIndicatorUI(
                        bsButton("crop_rasters", size="urldefault",label = "Crop rasters", block = F, style="primary")
                      ),bsTooltip(id = "crop_rasters", title = "Crop all rasters using mask", placement = "right", trigger = "hover")
                  ),
                  withBusyIndicatorUI(
                    button = bsButton("download_data", size="default",label = "Download", style="primary")
                  ),
                  bsTooltip(id = "download_data", title = "Download input rasters", placement = "left", trigger = "hover")
                  
                  
                             
                             ),
                mainPanel(
                  div(id= "text_dw",  class = "card",
                      h3(tags$strong("***Download external data and arrange it***")),
                      em("This tool allows you to download external data from different sources and crop them acording to the raster mask created before, Here are a brief description of data sources:"),
                      h5(tags$strong("Important aspects:")),
                      h5("Gridded data comes from different sources encompassing different perspectives, it has to be in a 2.5 arc-min spatial or 5km resolution, below are specified the source for each data:"),
                      h5(tags$strong("Climate approach:"),"We have use 19 bioclimatic variables at 2.5 arc-min spatial resolution from  ", tags$a("WoldClim", href = "http://www.worldclim.org"),
                         ". Raster files for an additional 16 climate variables were also used at the same spatial resolution from the Environmental Rasters for Ecological Modeling", tags$a("ENVIREM", href = "https://envirem.github.io") ,"database. "),
                      h5(tags$strong("Topography:"), "To characterize topography, we used elevation above sea level from the Shuttle Radar Topography Mission",tags$a("SRTM", href = "http://srtm.csi.cgiar.org") ,"dataset of the CGIAR-Consortium on Geospatial Information (CSI) portal."),
                      h5(tags$strong("Climate and Topography data will be downloaded in the path \'.../input_data/generic_rasters/world/\'")),
                      h5(tags$strong("anthropogenic and/r socio-economic:"), "namely, crop yield, total harvested area, total crop production, percentage of area under irrigation, population accessibility, distance to rivers, and distance to ancient human settlements."),
                      h5("-Crop yield, harvested area and crop production data were gathered from the", tags$a("MapSPAM", href = "http://mapspam.info/about/") ,"2005 database."),
                      h5("-Percentage of area under irrigation from the", tags$a("FAO", href = "http://www.fao.org/land-water/outreach/graphs-and-maps/details/en/c/237286/") ,"Global Map of Irrigation Areas."),
                      h5("-Population accessibility from the Global Map of Accessibility published by the ", tags$a("Joint Research Center", href = "https://forobs.jrc.ec.europa.eu/products/gam/",".")),
                      h5("-Distance to rivers map was created by computing the distance from each pixel (in a 2.5 arc-min grid) to the closest river."),
                      h5("-Geographic distance to the primary genepool wild relatives was created by computing the distance between every pixel (in a 2.5 arc-min grid) to the closest known observation of any wild relative within the primary genepool."),
                      h5(tags$strong("anthropogenic data will be downloaded in the path \'.../input_data/by_crop/<crop name>/lvl_1/raster/world/\'")),
                      h4(tags$strong("After all main inputs are stored in their respective folder, you will be able to crop each input according to 
                                     the raster mask already created. "))
                      
                      
                      )
                  
                )
              )
            ),
            tabPanel("Massage the database",
                     sidebarLayout(
                       sidebarPanel(
                         h3("Data base creator Assistant"),
                         fileInput("data_in",
                                   label = "1. Select .cvs database",
                                   multiple = FALSE,
                                   buttonLabel = icon("far fa-search"),
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                   ),
                         fileInput("mask",
                                   label = "2. Select region raster mask",
                                   multiple = FALSE,
                                   buttonLabel = icon("far fa-search"),
                                   accept = c(".tif")
                         ),
                         numericInput("col_number", label = "3. Write the column number of race variable", value = 0),
                         selectInput("do_ensemble_models", label = "4. Should we Train ensemble models?", choices = c("No" = 1, "Yes" = 2)),
                         conditionalPanel(condition = "input.do_ensemble_models == 2", 
                                          selectInput("do.predictions", label = "5. Should we predict Missing accessions classes?", choices = c("No" = FALSE, "Yes" = TRUE)),
                                          selectInput("add.latitude", label = "6. Should we use Latitude to train models?", choices = c("No" = FALSE, "Yes" = TRUE)),
                                          selectInput("add.longitude", label = "7. Should we use Longitude to train models?", choices = c("No" = FALSE, "Yes" = TRUE)),
                                          selectInput("sampling_mthd", label = "8. If data is unbalanced, select one sampling method", choices = c("None" = "none", "Up sampling" = "up", "Down sampling" = "down"))
                                          ),
                         withBusyIndicatorUI(
                         bsButton("prepare_data", size="default",label = "Set up database", block = F, style="primary")
                         
                         )

                       ),
                       mainPanel(
                         tabBox(width = 12,
                           tabPanel("Description",
                                    div(id= "text_3", class = "card",
                                        h3(tags$strong("***Database set up Assistant***")),
                                        em("The assistant will support you to select, clean, and standardize your database (passport data) to a specific format supported
                                            by this application."),
                                        h5(tags$strong("Important aspects:")),
                                        h5("The database (passport data) need three or 4 columns specifying: races, latitude and longitude, and an optional
                                            column with the type of accession(H for Herbarium accession and G for Germplasm accessions)."),
                                        h5("The only file type supported in this app is a CSV (comma separated values) format, once you have upload a valid file it
                                           can be previewed below."),
                                        h5(tags$strong("Final output:"), "CSV file with the class names, latitude, longitude and, all variables extracted from the raster files mentioned in \"Download input\"."),
                                        h5(tags$strong("File path: .../input_data/by_crop/<crop>/lvl_1/classification/<crop>_lvl_1_bd.csv")),
                                        tags$hr(),
                                        h4("In database preview"),
                                        dataTableOutput("data_prev"),
                                        tags$hr(),
                                        h4("Database Output"),
                                        dataTableOutput("data_out")
                                        
                                    )
                                    
                                    
                                    ),
                           tabPanel("Parameters",
                                    div(id = "param_1", class = "card",
                                        h3(tags$strong("*** Parameters Especification***")),
                                        h4("The function parameters are described below:"),
                                        h5(tags$strong("1. Select .csv database:"), "Select from your computer a valid csv file with passport data (only CSV files are supported)."),
                                        h5(tags$strong("2. Select region raster mask: "),"Select from your computer the raster mask created before (usually it has to be found in: \"
                                           .../input_data/mask/\" folder)."),
                                        h5(tags$strong("3. write the column number of race variable:")," Tell to the app the column number where crop races or structure is defined."),
                                        h5(tags$strong("4. Should we Train ensemble models:")," If is for your interest to train models in order to detect trends between drivers please select \"Yes\" 
                                          and a serie of new parameters will be deployed, otherwise select \"No\"."),
                                        h5(tags$strong("5. Should we predict Missing Accessions Classes:"), "If your database has missing accessions this function allow you to predict
                                           those accession without class information."),
                                        h5(tags$strong("6. Should we use Latitude to train models:"), "For some crops the latitude is an indicator for suitability and this could be
                                           important for the models ensemble performance, if it's the case then select \"Yes\". If not then please select \"No\"."),
                                        h5(tags$strong("7. Should we use Longitude to train models:"), "For some crops the longitude is an indicator for suitability and this could be
                                           important for the models ensemble performance, if it's the case then select \"Yes\". If not then please select \"No\"."),
                                        h5(tags$strong("8. If data is unbalance, select one sampling method: "),"For the models is very important that the amount of data per class being approximately equal
                                                       if not, the models performance might be affected. To deal with these issues the function allows choosing a sampling method to 
                                                       balance the data.")
                                        )
                                    
                                    )
                         )
                        
                         
                       )
                       
                     )
                     )
            
            )#end navarpage
  ),
  tabItem(tabName = "tab2",
          navbarPage(title = icon("fab fa-contao"),
                     tabPanel("Cost distance",
                              sidebarLayout(
                                sidebarPanel(
                                  h3(tags$strong("Cost distance function")),
                                  fileInput("occ_in",
                                            label = "1. Select Occurrence data",
                                            multiple = FALSE,
                                            buttonLabel = icon("far fa-search"),
                                            accept = c(".csv")
                                            ),
                                  fileInput("friction_in",
                                            label = "2. Select Friction surface raster",
                                            multiple = FALSE,
                                            buttonLabel = icon("far fa-search"),
                                            accept = c(".tif")
                                  ),
                                  withBusyIndicatorUI(
                                    bsButton("calculate_cost", size="default",label = "Create", block = F, style="primary")
                                    
                                  )
                                  
                                ),
                                mainPanel(
                                  tabBox(width = 12,
                                         tabPanel("Description",
                                           div(id = "desc_1", class = "card",
                                             h3(tags$strong("*** Create cost distance raster***")),
                                             h5("This function creates two files:"),
                                             h5(tags$strong("1. Shapefile of occurrence:"), "it is a spatial format for the latitude and longitude of each accession in the database, it can be found in"),
                                             h6(tags$strong("\"... / input_data / by_crop / <crop> / lvl_1 / <class> / < mask_name> / occurrences/ Occ.shp\" ")),
                                             h5(tags$strong("2. Cost distance raster:"), "It is a raster file where each pixel is computed from the friction surface and the occurrence shapefile created before,  as the cost of movement from each pixel in the friction raster to the nearest occurrence. The output file is saved in "),
                                             h6(tags$strong("\".../results/<crop>/lvl_1/<class>/<mask_name>/gap_models/cost_dist.tif\" "))
                                           
                                           ),
                                           div(class = "card",
                                               h4(tags$strong("Cost distance preview")),
                                               plotOutput("out_1", height = 250)
                                           )
                                           ),
                                         tabPanel("Parameters",
                                                  div(id = "params_2", class = "card",
                                                      h3(tags$strong("*** Parameters Especification***")),
                                                      h5(tags$strong("1. Select Occurrence data:"), "choose from your computer a valid CSV file with the passport data, latitude, longitude, and class/race information."),
                                                      h5("This file is created by the \"Database creator assistant\" from the assistant menu and you can find it in the next file path."),
                                                      h5(tags$strong(".../input_data/by_crop/<crop>/lvl_1/classification/<crop>_lvl_1_bd.csv")),
                                                      h5(tags$strong("2. Select Friction surface raster:"), "It is a raster file whose values represents the time spent to walk through one pixel of 5 km squared, taking 
                                                         into account the terrain type if it has roads or if it has a slope or if it is a forest, jungle, etc."),
                                                      h5(tags$strong("File path: .../input_data/auxiliar_raster/friction_surface.tif"))
                                                  )
                                                  
                                                  )
                                         
                                  )
                                          )
                              )
                     )
                     )
          
          
          ),
  tabItem(tabName = "tab3",
          navbarPage(title = icon("fas fa-globe-americas"),
                     tabPanel("Pseudo-absences",
                              sidebarLayout(
                                sidebarPanel(
                                  h3(tags$strong("Pseudo-absences generator")),
                                  h5(tags$strong("1. Database status")),
                                  div(id= "ss", htmlOutput("occ_exists")),
                                  radioButtons("pseudo_method", "2. Pseudo-absences method avaliables:",
                                               choices = list("Eco-regions" = "ecoreg",
                                                              "All the extent" = "all_area"),
                                               selected = "all_area"
                                               ),
                                  radioButtons("cor_method", "3. Method to remove covariates:",
                                               choices = list("Correlation matrix" = 1,
                                                              "VIF" = 2,
                                                              "PCA + VIF" = 3),
                                               selected = 3
                                  ),
                                  withBusyIndicatorUI(
                                    bsButton("create_pseudo", size="default",label = "Create", block = F, style="primary")
                                    
                                  )
                                  
                                ),
                                mainPanel(
                                  tabBox(width = 12,
                                         tabPanel("Description",
                                                  div(id = "pesudo_desc", class =  "card",
                                                      h3(tags$strong("*** Pseudo-absences generator function***")),
                                                      h4("Aiming to determine the spatial distribution of the landrace variety the model here implemented need some 
                                                         points(locations, coordinates), chosen randomly from where to compare against the occurrences but they have to 
                                                         be different and encompass a wide variety of environments over the extent. These random points are known as 
                                                         Background points or Pseudo-absences."),
                                                      h4("This Function implements two different methods to select points randomly over the raster mask which was created 
                                                         before by the assistants/helpers, and also to extract the values from the variables(Crop diversity drivers) and compile 
                                                         them into a single CSV file."),
                                                      h4("After all pesudo-absences are generated and filtered then the correlation between crop diversity drivers is
                                                         evaluated and the most correlated variables will be removed in order to avoid multicolinearity problems with the model."),
                                                      h4(tags$strong("Output File:"), "CSV file whit occurrences and pseudo-absences identified by 1 and 0 respectively."),
                                                      h4(tags$strong("File path:"), ".../input_data/by_crop/<crop_name>/lvl_1/<class_name>/<mask_name>/swd/swd_<class_name>.csv")
                                                      
                                                      )
                                                  ),
                                         tabPanel("Parameters",
                                                  h3(tags$strong("*** Parameter specification ***")),
                                                  h4("Chek if the database already exists and complies with requirements."),
                                                  h3(tags$strong("Pseudo-absences Method")),
                                                  h4(tags$strong("1- Eco-regions method:"), "Basically it consists in choose random points filtering first the places who have an ecology
                                                         similar to the occurrences through a",  tags$a("Ecology raster", href = "https://www.esri.com/about/newsroom/insider/the-first-detailed-ecological-land-unitsmap-in-the-world/"), ", and after
                                                         detect which pseudo-absences has similar bioclimatic conditions as occurrences and remove them, lastly draw buffers of 5 km around each occurrence and remove all pseudo-absences inside." ),
                                                  h4(tags$strong("2- All the extent:"), "Generate randomly Pseudo-absences over all the mask, and remove the ones with equal coordinates as occurrences."),
                                                  h3(tags$strong("Correlated variables filter")),
                                                  h4(tags$strong("Correlation matrix:"), "remove correlated variables through inspection to the correlation matrix."),
                                                  h4(tags$strong("VIF(Variance Inflation Factor):"), "Detect which variables are more correlated using the VIF criterion and remove them."),
                                                  h4(tags$strong("PCA + VIF:"), "Perform a Principal Component Analysis and identify the variables most correlated
                                                     with the first principal component, then Detect which of those variables are more correlated using the VIF criterion and remove them.")
                                                  
                                                  
                                                  
                                                  ),
                                         tabPanel("Resulst",
                                                  leafletOutput("map2")
                                                  )
                                )
                              )
                              )
                     ),
                     tabPanel("Model",
                              sidebarLayout(
                                sidebarPanel(
                                  h3(tags$strong("Landrace distribution")),
                                  selectInput("calib","Perform Calibration step:", choices = c("Yes" = 1, "No" = 2)),
                                  conditionalPanel(condition = "input.calib == 2",
                                                   numericInput("betamp", "Set a default value for Regularity:", value = 1 ,min = 0, max = 20),
                                                   checkboxGroupInput("feat", "select a default features combination:" ,
                                                                choices = c("Linear" = "l",
                                                                            "Product" = "p",
                                                                            "Quadratic" = "q",
                                                                            "Hinge"  = "h"
                                                                               ) )
                                                   ),
                                  h3(tags$strong("Model settings")),
                                  radioButtons("use.maxnet","Select Model:", 
                                               choices = list("MaxNet(R package)" = "TRUE", "MaxEnt.jar"= "FALSE"))
                                  
                                  
                                  
                                 
                                             ),
                                mainPanel(
                                  tabBox(width = 12,
                                         tabPanel("Description"),
                                         tabPanel("Parameters"),
                                         tabPanel("Results")
                                         )
                                )
                                
                              )
                              )
          )
    
  ) )#end tabitems

)#end body


dashboardPage(skin = "green",
  header,
  sidebar,#dashboardSidebar(disable = F),
  body
)

