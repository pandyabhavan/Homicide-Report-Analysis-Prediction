library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("Team-1", id="nav",
           
           tabPanel("Map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        conditionalPanel("0=1",id = "controls", class = "panel panel-default", fixed = TRUE,
                                         width = 330, height = "auto",
                                         
                                         h2("ZIP explorer"),
                                         
                                         selectInput("color", "Color", vars),
                                         selectInput("size", "Size", vars, selected = "adultpop"),
                                         conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                          # Only prompt for threshold when coloring or sizing by superzip
                                                          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                         ),
                                         
                                         plotOutput("histCentile", height = 200),
                                         plotOutput("scatterCollegeIncome", height = 250)
                        )
                    )
           ),
           tabPanel("Charts",
                    h3("Top 4 Victim race VS perpetrator race 18+"),
                    plotOutput("victimRacevsperpetrator"),
                    h3("Top 20 Female VS Male VS Weapon Used 18+"),
                    plotOutput("femalevsmalevsweapon"),
                    h3("Weapon Used"),
                    plotOutput("weaponUsed"),
                    h3("Female Killed by: Male killed by: 18+"),
                    plotOutput("femalevsmalekilledby"),
                    h3("Who Killed Who! Number of Incidents VS Family Relationship"),
                    plotOutput("whokilledwho"),
                    h3("Friends vs Sex"),
                    plotOutput("friendsvssex"),
                    h3("Employee VS Employer"),
                    plotOutput("employeevsemployer"),
                    h3("Ex-Husband VS Ex-Wife"),
                    plotOutput("exhusbandvsexwife"),
                    h3("Girlfriend vs Boyfriend"),
                    plotOutput("gfvsbf"),
                    h3("Incidents per Year"),
                    plotOutput("incidents")
           ),
           tabPanel("Prediction",
                    h3("Top 5 Perpetrator Age vs Victim Age vs Weapon"),
                    plotOutput("perpetratorvsvictimAgevsweapon"),
                    h3("Incident Prediciton"),
                    plotOutput("incidentprediction")
                    ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      DT::dataTableOutput("homicideData")
                    )
           )
)
