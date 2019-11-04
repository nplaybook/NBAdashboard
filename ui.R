ui <- fluidPage(
  tags$head(
    tags$script(src = "https://d3js.org/d3.v5.min.js"),
    includeCSS("www/style.css")
  ),
  setBackgroundColor(
    color = "white"
  ),
  fluidRow(
    column(
      width = 6,
      headerPanel("nplaybook")
    ),
    column(
      width = 6,
      align = "right",
      br(),
      dropdown(
        right = TRUE,
        HTML(
          paste(
            h3("Welcome!"), 
            tags$p(
              "This is NBA analytics dashboard, made for you who read this help message. This dashboard consist of several
        menu where you can compare team, customize roster, and everything you want to do with your roster."),
            tags$div(
              tags$strong(
                "Match Analytics"
              )
            ),
            tags$p(
              "Your team is Milwaukee Bucks and you are able to compare your roster to other 6 teams. Be carefull! 
              The polar chart shown doesn't mean to tell exact value of team statistics since it is a standardized value. Set aside
              polar chart, I have some previous game data where you can play with. You can evaluate team strategy based on recorded 
              player movements and dig deeper through players statistics."), 
        
            tags$div(
              tags$strong(
                "Player Finder"
              )
            ),
            tags$p("This menu enables you to create your own roster while keep paying attention to how much money you will spend
               on your players and how's your likelihood to get into playoff stage. You might
               pick as many players as you want to be considered just by clicking on the data table (clicked player will be shown 
               under value box). You can also filter players by their cluster which unsupervised machine learning takes part 
               on this process."),
            tags$div(
              tags$strong(
                "Player Magnifier"
              )
            ),
            tags$p("Knowing your player ability is a must. This menu enables you to analyze player's behavior on the court such as their 
               shot tendency, whether they tend to shot from right or left, and how your player take shooting decision per distance
               to the rack.")
          )
        ),
        style = "unite", icon = icon("basketball-ball"),
        status = "success", width = "500px",
        animate = animateOptions(
          enter = "fadeInDown", 
          exit = "fadeOutUp"
          )
        )
      )
  ),
  navlistPanel(
    well = TRUE,
    fluid = TRUE,
    widths = c(2, 10),
    tabPanel(
      title = "About",
      value = "about",
      icon = icon("id-badge"),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          align = "center",
          tags$img(src = "profpict.jpg", 
                   width = "270px", 
                   height = "270px", 
                   style ="display: block; margin-left: auto; margin-right: auto;"),
          HTML(
            paste(
              tags$h3(tags$strong("Naufal Abdila"))
              )
            ),
          HTML(
            paste(
              tags$h4("Data Guy")
              )
            )
          )
      ),
      br(),
      fluidRow(
        column(
          width = 1
        ),
        column(
          width = 10,
          HTML(
            paste(
              tags$p("This project is carried out to utilize data in sports industry. As people keep talking about Industrial Revolution 4.0
                     coming quick and massive, I've been wondering as a basketball geek", 
                     tags$strong(" what can we possibly do with this tech stuff and sports?"), "Then, I realize sports industry has
                     unlimited potential in utilizing data. We may use the data to simply help us create a strategy for the upcoming
                     matchup, scouting some players in the league, monitor player performances, fans engagement, and ticket sales. Thus, 
                     I create this dashboard to give a little example on how we may do it."),
              br(),
              tags$p("Data set sources:",
                     tags$ul(
                       tags$li(tags$a("basketball-reference.com", href = "https://www.basketball-reference.com/")),
                       tags$li(tags$a("NBA stats", href = "https://stats.nba.com/" )),
                       tags$li("SportVU")
                       )
                     )
              )
            )
          )
      ),
      fluidRow(
        column(
          width = 9
        ),
        column(
          width = 1,
          offset = 0,
          align = "right",
          tags$p(tags$a(tags$img(class = "image", 
                                 src = "https://icons-for-free.com/iconfiles/png/512/linkedin+logo+media+professional+profile+social+icon-1320168598882107551.png",
                                 height = "40px"),
                        href = "https://www.linkedin.com/in/naufal-abdila-a44757b8/")
                 )
        ),
        column(
          width = 1,
          offset = 0,
          align = "left",
          tags$p(tags$a(tags$img(class = "image", 
                                 src = "https://image.flaticon.com/icons/svg/25/25231.svg",
                                 height = "40px"),
                        href = "https://github.com/nplaybook")
                 )
          )
        )
      ),
    HTML("<p>Game Strategy</p>"),
    tabPanel(
      title = "Match Analytics",
      value = "match analytics",
      fluidRow(
        column(width = 6,
               div(style = "text-align-last:center !important;",
                   pickerInput(inputId = "team_a", 
                               label = "My team:",
                               width = "260px",
                               choices = "Milwaukee"
                   )
               )
               
        ),
        column(width = 6,
               align = "right",
               div(style = "text-align-last:center !important; ",
                   pickerInput(inputId = "team_b",
                               label = "Compare to:",
                               width = "260px",
                               choices = list("Chicago","Houston", "Philadelphia", 
                                              "Golden_State", "Boston", "Lakers")
                   )
               )
        )
      ),
      fluidRow(
        column(width = 3,
               #logo
               tags$img(src = "Milwaukee.png", 
                        width = "150px", 
                        height = "180px", 
                        style="display: block; margin-left: auto; margin-right: auto;"),
               #box team
               htmlOutput("box_a")
        ),
        column(width = 6,
               highchartOutput("hc_compare")
        ),
        column(width = 3,
               br(),
               #logo
               htmlOutput("logo_b"),
               br(),
               #box team
               htmlOutput("box_b")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 3,
          HTML(
            paste(
              tags$p(tags$strong("FGP"),": Field Goal Percentage",
                     tags$br(),
                     tags$strong("3PP"),": 3 Point Percentage",
                     tags$br(),
                     tags$strong("FTP"),": Free Throw Percentage",
                     tags$br(),
                     tags$strong("ORB"),": Offensive Rebound",
                     tags$br(),
                     tags$strong("AST"),": Assists")
            )
          )
        ),
        column(
          width = 3,
          HTML(
            paste(
            tags$p(tags$strong("STL"),": Steals", 
                   tags$br(),
                   tags$strong("BLK"),": Blocks",
                   tags$br(),
                   tags$strong("DRB"),": Defensive Rebound",
                   tags$br(),
                   tags$strong("TOV"),": Turnovers",
                   tags$br(),
                   tags$strong("PF"),": Personal Fouls")
            )
            )
          )
      ),
      fluidRow(
        column(
          width = 12,
          tags$hr()
        )
      ),
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "gif",
            label = "Select animated play by play:", 
            choices = c("event4", "event125", "event461"),
            selected = NULL 
          )
        )
      ),
      fluidRow(
        column(
          width = 6
        ),
        column(
          width = 6,
          htmlOutput("gif")
        )
      ),
      fluidRow(
        column(
          width = 2,
          pickerInput(inputId = "matchup.date",
                      label = "Select game date:",
                      choices = ""
          )
        ),
        column(
          width = 3,
          prettyRadioButtons(inputId = "quarter",
                             label = "Quarter:",
                             choices = c("All", 1, 2, 3, 4),
                             selected = "All",
                             inline = TRUE,
                             status = "success"
          )
        ),
        column(
          width = 4
        ),
        column(
          br(),
          width = 3,
          align = "right",
          radioGroupButtons(
            inputId = "switch",
            label = NULL, 
            choices = c("Bucks", "OPP"),
            selected = "OPP",
            direction = "horizontal" 
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          formattableOutput("matchsummary")
        )
      )
    ),
    HTML("<p>Scouting</p>"),
    tabPanel(
      title = "Player Magnifier",
      value = "player magnifier",
      fluidRow(
        column(
          width = 3,
          align = "center",
          #profile picture
          htmlOutput(outputId = "closeup", align = "center"),
          br(),
          pickerInput(inputId = "player",
                      label = "Select player:",
                      choices = unique(shot.plot$PLAYER_NAME),
                      selected = shot.plot$PLAYER_NAME[1]
          ),
          formattableOutput("bio")
        ),
        column(
          width = 9,
          align = "center",
          plotOutput(outputId = "shot_tracker", height = 100), #posisi belom bener
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          # column(
          #   width = 3
          # ),
          column(
            width = 1
          ),
          column(
            width = 3,
            div(style = "text-align-last:center !important;",
                pickerInput(inputId = "versus",
                            label = "Pick opposing team:",
                            choices = ""
                )
            )
          ),
          column(
            width = 3,
            align = "left",
            div(style = "text-align-last:center !important;",
                pickerInput(inputId = "date_available",
                            label = "Select game date:",
                            choices = ""
                )
            )
          ),
          column(
            width = 2,
            offset = 0,
            align = "left",
            awesomeCheckboxGroup(inputId = "event_type",
                                 label = "Event type:",
                                 choices  = unique(shot.plot$EVENT_TYPE),
                                 selected = c("Made Shot", "Missed Shot"),
                                 status = "success",
                                 inline = TRUE
            )
          ),
          column(
            width = 3,
            offset = 0,
            align = "left",
            awesomeCheckboxGroup(inputId = "period",
                                 label = "Period:",
                                 choices = unique(shot.plot$PERIOD),
                                 selected = c(1, 2, 3, 4),
                                 status = "success",
                                 inline = TRUE
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          tags$hr()
        )
      ),
      fluidRow(
        column(
          width = 12,
          highchartOutput("against")
        )
      ),
      fluidRow(
        column(
          width = 6,
          highchartOutput("perdistance")
        ),
        column(
          width = 6,
          highchartOutput("perposition")
        ),
        column(
          width = 3,
          highchartOutput("shot_tend")
        )
      )
    ),
    tabPanel(
      title = "Player Finder",
      value = "player finder",
      fluidRow(
        column(
          width = 4, 
          tags$div(
            class = "card detail-card",
            style = "background-color: #00471B; color: #EEE1C6;",
            fluidRow(
              column(
                width = 6,
                htmlOutput("salary")
              ),
              column(
                width = 6,
                tags$img(class = "image-card", 
                         src = "https://cdn0.iconfinder.com/data/icons/finance-android-l-lollipop-icon-pack/24/money_bag-256.png",
                         height = "8px")
              )
            )
          )
        ),
        column(
          width = 4,
          tags$div(
            class = "card detail-card",
            style = "background-color: #00471B; color: #EEE1C6;",
            fluidRow(
              column(
                width = 6,
                htmlOutput("probs")
              ),
              column(
                width = 6,
                tags$img(class = "image-card",
                         src = "https://cdn1.iconfinder.com/data/icons/ios-11-glyphs/30/statistics-128.png",
                         height = "8px"))
            )
          )
        ),
        column(
          width = 4,
          tags$div(
            class = "card detail-card",
            style = "background-color: #00471B; color: #EEE1C6;",
            fluidRow(
              column(
                width = 6,
                htmlOutput("age.ave")
              ),
              column(
                width = 6,
                tags$img(class = "image-card",
                         src = "https://cdn0.iconfinder.com/data/icons/elasto-online-store/26/00-ELASTOFONT-STORE-READY_user-128.png",
                         height = "8px"))
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          div(style = "text-align-last:left !important;",
              selectizeInput(inputId = "custom.roster", 
                             label = NULL,
                             choice = merge$Player,
                             selected = c("Giannis Antetokounmpo", "Khris Middleton", "Brook Lopez",
                                          "Eric Bledsoe", "Wesley Matthews", "Robin Lopez", 
                                          "Kyle Korver", "Sterling Brown", "D.J. Wilson",
                                          "Donte Divincenzo", "George Hill", "Pat Connaughton",
                                          "Ersan Ilyasova"),
                             multiple = TRUE,
                             width = "1200px")
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 2,
          htmlOutput(outputId = "player.list")
        ),
        column(
          width = 4,
          htmlOutput("customrostertable")
        ),
        column(
          width = 6,
          highchartOutput("worth")
        )
      ),
      fluidRow(
        column(
          width = 12,
          highchartOutput("cluster")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 3,
          pickerInput(inputId = "cluster.filter",
                      label = "Cluster:",
                      choices  = c("All", unique(sort(cluster.pole$cluster)))
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          dataTableOutput("tableout")
        )
      )
    )
  )
)