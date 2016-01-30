library(shiny)
library(leaflet)

shinyUI(
        #fluidPage(
    #fluidRow(
#         column(3,
#             wellPanel(
#                 textInput("address", label = "Enter The Address (Need No Apt. Number)",  value = "6109 39th Avenue"),
#                 uiOutput("choose_neighborhood"),
#                 selectInput("borough", "Which Borough?", choices = c("QUEENS", "BROOKLYN", "MANHATTAN",  "BRONX", "STATEN ISLAND"), selected = 2),
#                 uiOutput('choose_zipcode'),
#                 numericInput("resident",
#                          label = "How many residential units does the property contain?",
#                          value =1),
#                 numericInput("commerce",
#                          label = "How many commerical units does the property contain?", value = 0),
#                 numericInput("land_sqft", label = "Land Square Feet", value =1000),
#                 numericInput("gross_sqft", label = "Gross Land Square Feet", value = 1000)
#                 
#                 
#         )),
        #column(10,
            #titlePanel("NYC Real Estate Price Estimator"),
            #titlePanel("Guess how much does your house worth at current market?"),
            #leafletOutput("general_view_map"),
            navbarPage("NYC Real Estate Price Estimator", id="nav", theme="cerulean_bootstrap.min.css",
                
                tabPanel("Describe Your Property", id="describe",
                         tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css")
                             #includeScript("gomap.js")
                         ),
                         
                         column(2,id = "controls", class = "panel panel-default", fixed = FALSE,
                                       draggable = FALSE, top = "auto", left = 200, right = "auto", bottom = "auto",
                                       #width = 300, height = "auto", 
                                wellPanel(margin = 0,
                                    textInput("address", label = "Enter The Address (No Apt. Number)", value="6109 39th Avenue", placeholder = "such as 6109 39th Avenue"),
                                    uiOutput("choose_neighborhood"),
                                    selectInput("borough", "Which Borough?", choices = c("QUEENS", "BROOKLYN", "MANHATTAN",  "BRONX", "STATEN ISLAND"), selected = 2),
                                    uiOutput('choose_zipcode'),
                                    numericInput("resident",
                                                 label = "How many residential units (one unit for one family) does the property contain?",
                                                 value =1),
                                    numericInput("commerce",
                                                 label = "How many commerical units does the property contain?", value = 0),
                                    numericInput("land_sqft", label = "Land Area (The land area of the property listed in square feet)", value =1000),
                                    numericInput("gross_sqft", label = "Gross Floor Area (The total
                                                 area of all the floors of a building as measured
                                                 from the exterior surfaces of the outside walls of the building,
                                                 including the land area and space within any building or structure on the property)", value = 1000)
                                    
                                    
                                )),
                    column(10, id="describe", class = "panel panel-default",
                                  left = 400, #width = "67%", color="black",
                        wellPanel(
                                 em(h3(id="header",
                                     "Introduction")),
                            p(h4("    The problem that this estimator
                                 tries to solve is to estimate the current market price of a potential real
                                 estate item for sale using machine learning models trained with the publicly
                                 available NYC real estate transaction records from May 2013 to December 2015.
                                 When fed with a property’s information, the estimator generates estimations 
                                    under other three tab bars of this page: under the tab bar of “Interpret the Outcome”
                                are listed a predicted market price, its 95% confidence interval and
                                 a brief interpretation about the outcome; under the tab bar of “KNN Data Explorer” 
                                 are showcased the top 5 sold properties that are the most similar to the property
                                 of interest in terms of location, number of units, land and floor areas;
                                 and under the tab bar of “Show On Map” are illustrated locations on the map of the five
                                 similar sold properties and the property of interest.")),
                        #wellPanel(
                                em(h3(id="header",
                                    "Load The Data")),
                            h4("Fill the form to the left with information of a property of interest.
                               Then push the button below to load."),
                            br(),
                            actionButton("loadIn", "Load Your Entry into the Estimator"),
                        
                        #wellPanel(
                            em(h3(id="header",
                                "Ready To Go")),
                        h4(textOutput("text_isThis")),
                        br(),
                        actionButton("goButton", "Estimate The Property!"),
                        h5("For detailed outcome, check out the other three columns to the right."),
                        DT::dataTableOutput("test_input")
                        )
                        )
                    ),
                
                tabPanel("Interprete The Outcome",
                         wellPanel(
                             em(h3(id="header",
                                   "Interpretation")),
                             h4("This part of estimation is done by predicting the market price
                                of the property according to what you have filled in the form.
                                The prediction model behind is a linear regression model,
                                which not only gives out an suggested market value,
                                but also a 95% confidence interval about its true market value, as you see in the outcome table below.
                                In the estimator's suggestion listed below, the “Lower Bound” and “Upper Bound”
                                represent the two ends of the 95% Confidence Interval.
                                As for this jargon of Confidence Interval that we have mentioned several times, it indicates that
                                with a probability as high as 95 percent (100 percent indicates being absolutely sure), the market price bracket between the 
                                Lower and Upper Bounds contain the true market value of the property of interest.
                                In another word, you need be wary of a price of the property outside of the bracket.")
                         ),
                         wellPanel(
                            h4("This is the estimator's suggested market price for your property."),
                            tableOutput("lm_prediction")),
                         br(),
                         br(),
                         wellPanel(
                             h4("Done with this part? Go on to “KNN Table Explorer” to see similar properties.")
                         )
                         ),

                tabPanel(" KNN Table Explorer",
                         wellPanel(
                             em(h3(id="header",
                                   "What Is KNN?")),
                             h4("KNN is the acronym of K Nearest Neighbors, which is a commonly used algorithm to compare similarity of items.
                                In this case, the estimator uses Euclidean distance as distance metric to compare the property of interest with each of sold properties
                                in the past two and a half years in NYC. After the estimator picks out the top 5 similar properties, it lists them below and illustrates
                                them on an NYC map, which you can find under the tab bar of “Show on Map”. Features, including zip code, land area, total area of all floors,
                                and numbers of residential and commerical units in a property are given high weights as the estimator compare properties.")
                         ),
                         wellPanel(h4("Top 5 is not enough? No problem, You can specify a K number here, and the estimator will give you that number of most similar properties."),
                                   sliderInput("knumber", label = h3("Specify a K number"),
                                               min = 1, max = 50, value = 5),
                                   br(),
                                   actionButton("showButton", "Show Me More!")
                                   ),
                             DT::dataTableOutput("knn_prediction", width="auto")
                        ),
                tabPanel(" Show On Map  ",
                         wellPanel(
                             em(h3(id="header",
                                "check them out on map"
                         )),
                                h4("The estimator marks your property of interest (in green) and those (in blue) similar to it on the map to give you a better idea of their locational relations."),
                                br(),
                                h4("Note: when you pick a big K number to show more similar properties, please give the estimator patience since the server needs time to calculate their geographical coordinates.")
                         ),
                    leafletOutput("knn_map", width = "auto"),
                    verbatimTextOutput("writeindata")
                        )
            )
    
    )

