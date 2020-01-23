# Load packages -------------------------------------------------------------------
library(shiny)
library(ncaahoopR)
library(DT)
library(readr)
library(shinydashboard)
library(dplyr)
library(ggthemes)
library(reshape2)
library(ggplot2)
library(ggvis)
library(htmlwidgets)

# Load data -----------------------------------------------------------------------
gumdad <- read_csv("data/boxes.csv", col_types = 
  cols(
  season = col_character(),
  date = col_character(),
  opponent = col_character(),
  result = col_character(),
  unc = col_double(),
  opp = col_double(),
  OT = col_character(),
  location = col_character(),
  type = col_character(),
  realtype = col_character(),
  box = col_character()
  )
  )

factors <- read_csv("data/factors.csv", col_types = 
  cols(
  season = col_character(),
  opponent = col_character(),
  result = col_character(),
  pace = col_number(), 
  ppp = col_number(),
  efg = col_number(),
  to = col_number(),
  or = col_number(),
  ft = col_number(),
  oppp = col_number(),
  oefg = col_number(),
  oto = col_number(),
  oor = col_number(),
  oft = col_number()
  )
)

refs <- read_csv("data/refs.csv", col_types = 
  cols(
  season = col_character(),
  opponent = col_character(),
  result = col_character(),
  unc = col_number(), 
  opp = col_number(),
  off1 = col_character(),
  off2 = col_character(),
  off3 = col_character(),
  uncfouls = col_number(),
  oppfouls = col_number(),
  sumfouls = col_number()
  )
)
offshots <- read_csv("data/offshots.csv", col_types = 
  cols( 
  season = col_character(),
  opponent = col_character(),
  result = col_character(),
  efg = col_number(),
  pm = col_number(),
  pa = col_number(),
  pmp = col_number(),
  tpm = col_number(),
  tpa = col_number(),
  tmp = col_number(),
  tmpa = col_number(),
  fpa = col_number(),
  fpt = col_number(),
  tft = col_number()
  )
)

defshots <- read_csv("data/defshots.csv", col_types = 
  cols( 
  season = col_character(),
  opponent = col_character(),
  result = col_character(),
  oefg = col_number(),
  opm = col_number(),
  opa = col_number(),
  opmp = col_number(),
  otpm = col_number(),
  otpa = col_number(),
  otmp = col_number(),
  otmpa = col_number(),
  ofpa = col_number(),
  ofpt = col_number(),
  otft = col_number()
)
    )

# Load data for plots ----------------------------------------------------------------------
plot <- read.csv("data/plot.csv")
  

# Define the UI ----------------------------------------------------------------------------

ui <- dashboardPage(
# Add header -------------------------------------------------------------------------------
dashboardHeader(
  title= "Dadgum Box Scores",
  titleWidth = 300,
tags$li(class = "dropdown", 
  tags$a(class="fab fa-twitter-square", 
    href="https://twitter.com/dadgumboxscores/"))),
  
# Add sidebar -----------------------------------------------------------------------------
 dashboardSidebar(
   width = 300,
sidebarMenu(
    menuItem("Box Scores", tabName = "boxscores", icon = icon("th")),
    menuItem("Four Factors", tabName = "factors", icon = icon("dice-four")),
    menuItem("Referees and Fouls", icon = icon("users"), tabName = "refs"),
    menuItem("Offense: Shooting Stats", tabName = "shots", icon = icon("arrow-circle-up")),
    menuItem("Defense: Shooting Stats", tabName = "def", icon = icon("arrow-circle-down")), 
    menuItem("Plot Party", tabName = "party", icon = icon("chart-line")), 
    menuItem("2019-20 Shot Charts", tabName = "chart", icon = icon("basketball-ball")), 
    menuItem("Dadgum Data", tabName = "datadump", icon = icon("database")), 
    tags$br(), 
    tags$br(),
    menuItem("FAQs", icon = icon("question"), tabName = "purpose"),
    tags$li(class="leaky", 
      tags$a(href="https://cbbstatshelp.com/resources/stats-glossary/", 
        class="yeahyeah", target="_blank", "📓 Stat Glossary")
      ),
    tags$li(class="leaky", 
      tags$a(href="https://dadgumboxscores.substack.com/", 
        class="yeahyeah", target="_blank", "👀 The Dadgum Dispatch")
      ),
    tags$li(class="leaky", 
      tags$a(href="https://twitter.com/dadgumboxscores/", 
        class="yeahyeah", target="_blank", "👉 @dadgumboxscores")
      ),
    tags$br()
  )
  ),
# Start Dashboard body ---------------------------------------------------------------------
dashboardBody(style="background-color: #ecf0f5",
  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "dadgum.css")
    ), 
   tabItems(
# BOX SCORES TAB ----------------------------------------------------------------------------
tabItem(tabName = "boxscores", 
fluidRow(column(width = 12,
# HTML for box scores tab -------------------------------------------------------------------  
  fluidRow(
    div(class="center", h2("Every UNC Box Score Since 2003-04"), 
      h3("Filter by season, wins, losses, or opponent"),
      p(class="d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
      ),
# Filters for box scores tab ----------------------------------------------------------------  
     box(width = 4,
      height = 125, 
      selectInput("season",
           label = "Season:",
          c("All",
         unique(as.character(gumdad$season))))
),
box(
      width = 4,  
      height = 125,    
selectInput("opponent",
           label = "Opponent:",
          c("All",
         unique(as.character(gumdad$opponent))))
),
box(
      width = 4,
      height = 125, 
      checkboxGroupInput("result", 
      label = "Result:", 
      c(unique(as.character(gumdad$result))))
  ),
# Datatable for box scores tab ----------------------------------------------------------------  
    box( width= 12,
      DT::dataTableOutput('tableone')
    )
    )
  )
)
),

# FACTORS TAB ----------------------------------------------------------------------------------  
      tabItem(tabName = "factors",
        fluidRow(column(width=12,
# HTML for factors tab ------------------------------------------------------------------------- 
          fluidRow(
    div( class="center", h2("Four Factors Game-By-Game"), 
      h3("Filter by season, wins, losses, or opponent"),
      p(class="d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
      ),
           box(
      width = 4,
      height = 125,  
selectInput("seasonfactors",
           label = "Season:",
          c("All",
         unique(as.character(factors$season))))
),
           box(
      width = 4, 
      height = 125,      
selectInput("opponentfactors",
           label = "Opponent:",
          c("All",
         unique(as.character(factors$opponent))))
),
      box(
      width = 4,
      height = 125,  
      checkboxGroupInput("resultfactors", 
      label = "Result:", 
      c(unique(as.character(factors$result))))
  ),
# Datatables for factors tab ----------------------------------------------------------------------
    box( width= 12,
      DT::dataTableOutput('tabletwo')
      )
      )
          )
        )
          ), 
# REFS TAB ----------------------------------------------------------------------------------------
      tabItem(tabName = "refs",
        fluidRow(column( width=12, 
# HTML for refs tab ------------------------------------------------------------------------------- 
            fluidRow(
    div( class="center", h2("Refs Game-By-Game"), 
      h3("Filter by season, wins, losses, or opponent"), 
      h4("Most Frequent Ref (Total Games): ROGER AYERS, 85"),
      p(class="d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
      )
    ),
# Filters for refs tab ----------------------------------------------------------------------------             
       fluidRow(
          box(
      width = 4,
      height = 125,  
selectInput("seasonrefs",
           label = "Season:",
          c("All",
         unique(as.character(refs$season))))
),
           box(
      width = 4,   
      height = 125,    
selectInput("opponentrefs",
           label = "Opponent:",
          c("All",
         unique(as.character(refs$opponent))))
),
      box(
      width = 4,
      height = 125,  
      checkboxGroupInput("resultrefs", 
      label = "Result:", 
      c(unique(as.character(refs$result))))
  ),
# Datatables for refs tab -------------------------------------------------------------------------- 
    box( width= 12,
      DT::dataTableOutput('tablethree')
      )
    )
            )
        )
        ),
# OFFENSIVE SHOTS TAB ------------------------------------------------------------------------------ 
      tabItem(tabName = "shots",
        fluidRow(column( width=12, 
# HTML for offensive shots tab --------------------------------------------------------------------- 
  fluidRow(
    div( class="center", h2("Offense: Shots Game-By-Game"), 
      h3("Filter by season, wins, losses, or opponent"),
      p(class="d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
      ),
# Filters for offensive shots tab ------------------------------------------------------------------ 
     column(width = 12, offset = 3, class = "center",
       box(
                      width = 6,
            sliderInput( 
              "efgshots", label = "Opponent Effective Field Goal Percentage (eFG)", min = 0, 
        max = 90, value = c(0, 80))
            )
    ),
          box(
      width = 4,
      height = 125,  
selectInput("seasonshots",
           label = "Season:",
          c("All",
         unique(as.character(offshots$season))))
),
           box(
      width = 4, 
      height = 125,      
selectInput("opponentshots",
           label = "Opponent:",
          c("All",
         unique(as.character(offshots$opponent))))
),
      box(
      width = 4,
      height = 125,  
      checkboxGroupInput("resultshots", 
      label = "Result:", 
      c(unique(as.character(offshots$result))))
  ),
# Datatables for offensive shots tab ----------------------------------------------------------------- 
    box( width= 12,
      DT::dataTableOutput('tablefour')
      )
    )
  )
        )
        ),
# DEFENSIVE SHOTS TAB -------------------------------------------------------------------------------- 
      tabItem(tabName = "def",
       fluidRow(column( width=12, 
# HTML for defensive shots tab ----------------------------------------------------------------------- 
  fluidRow(
    div( class="center", h2("Defense: Shots Game-By-Game"), 
      h3("Filter by season, wins, losses, or opponent"),
      p(class="d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
      ),
# Filters for defensive shots tab -------------------------------------------------------------------- 
     column(width = 12, offset = 3, class = "center",
       box(
            width = 6,
            sliderInput( 
              "oefgshots", label = "Effective Field Goal Percentage (eFG)", min = 0, 
        max = 90, value = c(0, 80))
            )
    ),
          box(
      width = 4,
      height = 125, 
selectInput("seasondef",
           label = "Season:",
          c("All",
         unique(as.character(defshots$season))))
),
           box(
      width = 4,   
      height = 125,   
selectInput("opponentdef",
           label = "Opponent:",
          c("All",
         unique(as.character(defshots$opponent))))
),
      box(
      width = 4,
      height = 125, 
      checkboxGroupInput("resultdef", 
      label = "Result:", 
      c(unique(as.character(defshots$result))))
  ),
#Datatables for defensive shots tab -------------------------------------------------------------------
    box( width= 12,
      DT::dataTableOutput('tablefive')
      )
    )
  )
)
),
# PLOT PARTY TAB -------------------------------------------------------------------------------------- 
  tabItem(tabName = "party",
    fluidRow(column(width=8,
  h2("Plots using individual game data"),
  p("Data shows game-by-game numbers for effective field goal (eFG), turnover (TO%), 
          offensive rebounding (OR%), two-point shots (2PT%), three-point shots (3PT%), 
          and free throws (FT%). Rates/attempts for three-point shots and free throws."),
  p(class="d-sm-none", "You're on a mobile screen, these plots will be a bit tougher to use.")
  )
),
    fluidRow(
     box(title = "The Plot Thickens", solidHeader=TRUE,  ggvisOutput("plot1"),uiOutput("plot1_ui"), width=8),
     box(title = "Filters", solidHeader=TRUE,  
         
         
         selectInput("yearinput", label = h5("Select Season"), 
                     choices = c("All", as.list(levels(as.factor(plot$season)))), selected = "All"),

         selectInput("opponentinput", label = h5("Select Opponent"), 
                     choices = c("All", as.list(levels(as.factor(plot$opponent)))), selected = "All"),
         
         selectInput("selectx1", label = h5("X-Axis"), 
                     choices = list("Pace" = "pace", 
                                    "UNC PPP" = "ppp", 
                                    "UNC eFG" = "efg",
                                    "UNC TO%" = "to", 
                                    "UNC OR%" = "or", 
                                    "UNC FTRate" = "ft",
                                    "UNC 2PT%" = "pmp", 
                                    "UNC 3PT%" = "tmp", 
                                    "UNC 3PTRate" = "tmpa",
                                    "UNC FTM" = "fpa", 
                                    "UNC FTA" = "fpt",
                                    "UNC FT%" = "tft", 
                                    "OPP PPP" = "oppp", 
                                    "OPP eFG" = "oefg",
                                    "OPP TO%" = "oto", 
                                    "OPP OR%" = "oor", 
                                    "OPP FTRate" = "oft",
                                    "OPP 2PT%" = "opmp", 
                                    "OPP 3PT%" = "otmp", 
                                    "OPP 3PTRate" = "otmpa",
                                    "OPP FTM" = "ofpa",
                                    "OPP FTA" = "ofpt",
                                    "OPP FT%" = "otft"                  
                     ), 
                     selected = "efg"),
         
         
         
         selectInput("selecty1", label = h5("Y-Axis"), 
                     choices = list("Pace" = "pace", 
                                    "UNC PPP" = "ppp", 
                                    "UNC eFG" = "efg",
                                    "UNC TO%" = "to", 
                                    "UNC OR%" = "or", 
                                    "UNC FTRate" = "ft",
                                    "UNC 2PT%" = "pmp", 
                                    "UNC 3PT%" = "tmp", 
                                    "UNC 3PTRate" = "tmpa",
                                    "UNC FTM" = "fpa", 
                                    "UNC FTA" = "fpt",
                                    "UNC FT%" = "tft", 
                                    "OPP PPP" = "oppp", 
                                    "OPP eFG" = "oefg",
                                    "OPP TO%" = "oto", 
                                    "OPP OR%" = "oor", 
                                    "OPP FTRate" = "oft",
                                    "OPP 2PT%" = "opmp", 
                                    "OPP 3PT%" = "otmp", 
                                    "OPP 3PTRate" = "otmpa",
                                    "OPP FTM" = "ofpa",
                                    "OPP FTA" = "ofpt",
                                    "OPP FT%" = "otft"  
                                    
                     ), 
                     selected = "or"),
         
         width=4)
    
          )

      ), 
# SHOT CHART TAB ----------------------------------------------------------------------------------------
 tabItem(tabName = "chart",
  h2("2019-20 Shot Charts"),
  p("Charts courtesy of ncaahoopR. Please be patient for the updates as the information is being pulled via ESPN."),
  # Sidebar with a checkbox input for game selection 
      fluidRow(width = 4,
    box(
      selectInput("dateBoxes", 
                         label = "2019-20 Games:",
                         choices = c("Game 01: Notre Dame" = 401168159,
                                     "Game 02: at UNCW" = 401168164,
                                     "Game 03: Gardner-Webb" = 401168185,
                                     "Game 04: Elon" = 401168201,
                                     "Game 05: Alabama" = 401168222,
                                     "Game 06: Michigan" = 401183482,
                                     "Game 07: Oregon" = 401183485,
                                     "Game 08: Ohio State" = 401168241,
                                     "Game 09: UVA" = 401184943, 
                                     "Game 10: Wofford" = 401168255, 
                                     "Game 11: Gonzaga" = 401168261, 
                                     "Game 12: UCLA" = 401168270,
                                     "Game 13: Yale" = 401168285,
                                     "Game 14: Georgia Tech" = 401168484, 
                                     "Game 15: Pittsburgh" = 401168489,
                                     "Game 16: Clemson" = 401168491,
                                     "Game 17: Pittsburgh" = 401168493,
                                     "Game 18: Virginia Tech" = 401168494
                                     
                         ),
                         selected = c(401168494)
      ),
     
      #Radio button input for presenting shot chart as heatmap
      radioButtons("heatmapSelector",
                   label = "Show Heatmap?",
                   choices = c("Yes" = TRUE,
                               "No" = FALSE),
                   selected = c(FALSE)
      )
      )
    ),
    
    # Show a plot of the generated distribution
      fluidRow(width= 12,
        box(
        plotOutput("GameShotChart", height = 600, width = 900), width=10)
      )
 ),
# DATADUMP TAB ----------------------------------------------------------------------------------------
      tabItem(tabName = "datadump",
        fluidRow(column(width=6, 
        h2("Download some dadgum data"),
p("Below is a list of data sets provided in CSV format."),

h4("Box Scores and Factors"),
p("Includes game-by-game information and four factors:"),
tags$a(href="https://dadgumboxscores.com/download/box_score_data.csv", "box_score_data.csv"),
tags$br(), 
tags$a(href="https://dadgumboxscores.com/download/four_factors_all.csv", "four_factors_all.csv"),
tags$br(), 
tags$hr(), 

h4("Referee Data"),
p("Includes game-by-game list of refs, fouls, and spreads:"),
tags$a(href="https://dadgumboxscores.com/download/refs.csv", "refs.csv"),
tags$br(), 
tags$hr(), 

h4("Offensive Shooting Numbers"),
p("Includes game-by-game offensive shooting numbers:"),
tags$a(href="https://dadgumboxscores.com/download/offensive_shooting.csv", "offensive_shooting.csv"),
tags$br(), 
tags$hr(), 

h4("Defensive Shooting Numbers"),
p("Includes game-by-game defensive shooting numbers:"),
tags$a(href="https://dadgumboxscores.com/download/defensive_shooting.csv", "defensive_shooting.csv"),
tags$br(), 
tags$hr(), 

h4("Bonus Data"),
p("This data isn't yet included in the app for one reason or another:"),
tags$a(href="https://dadgumboxscores.com/download/point_spreads.csv", "point_spreads.csv"),
tags$br(), 
tags$a(href="https://dadgumboxscores.com/download/tech_fouls.csv", "tech_fouls.csv")

)
        )
        ),
# PURPOSE TAB -----------------------------------------------------------------------------------------
      tabItem(tabName = "purpose",
        fluidRow(column(width=6,
                h2("What is this dadgum thing?"),
p("Dadgum box scores is a collection of every UNC men’s basketball box score since the 
  2003-04 season."),

p("I put together this site because I wanted it to be easier to find old UNC box scores. 
  I was tired of navigating crappy sites and not finding what I wanted. It shouldn’t be that 
  hard to find a box score. It should be fast, easy, and organized."), 

tags$br(), 
h4("What else will you find on this site?"), 
p("The focus of this dadgum doodad has expanded to include other data, including:"), 
tags$li("officials: a list of all refs for every game since '03-04"), 
tags$li("four factors: game-by-game numbers of the building blocks of efficiency"), 
tags$li("shooting data: game-by-game shooting stats since '03-04"), 
tags$li("plots: some of this data can be used to generate charts or plots or graphs"), 
p("This site doesn’t include schedules, player stats, or interviews. I’m not a reporter, and I’m not going 
  to break any news."),
tags$br(), 

h4("Why only data since the 2003-2004 season?"), 
p("Couple reasons. First, the difficulty goes way up trying to find box scores prior to this date.
 Even finding box scores from the '03-04 season took some elbow grease."), 
p("Second, the cutoff date makes it a bit more current. This was Roy Williams first season, and it felt
 like a clean break and good starting point."), 
tags$br(), 

h4("Can I use the data on this site?"), 
p("Yes. Feel free to use this data or link to it on your own sites or for your own research. If you want 
  to refer to dadgumboxscores.com as the source, you deserve a high-five."),

tags$br(), 
h4("I found an error, what should I do next?"), 
p("If you can’t find something you’re looking for or spot an typo or if data looks blatantly wrong, 
  please let me know via", 
tags$a(href="mailto:cbbstatshelp.com", "email.")), 
tags$br(), 
h4("Anything else?"), 
p("This tool is built in R using the Shiny Dashboard, Readr, DT, and several other packages. All data is 
from box scores via goheels.com and spreads via covers.com. If you have 
  suggestions on how to improve it or make it better, I'm all ears."), 
tags$br()
)
        )   
  )
  )
)
)

# OPEN SERVER ------------------------------------------------------------------------------------------
server <- function(input, output) {

# BOX SCORES SERVER TAB --------------------------------------------------------------------------------
output$tableone = renderDT({
  data <- gumdad 
  if (input$season != "All") {
    data <- data[data$season == input$season,]
  }
   if (input$opponent != "All") {
    data <- data[data$opponent == input$opponent,]
  }
 if(!is.null(input$result)) {
      data <- data[data$result == input$result,]
    }
  datatable(data,  rownames = FALSE, escape = FALSE, selection = 'none',
                    colnames = c('Season', 'Date', 'Opponent', 'Result', 'UNC', 'Opp', 'OT', 
                      'Location','RType','Type','Box Score'),
                    options = list(paging = FALSE, scrollX = TRUE, columnDefs = list(list(visible=FALSE, targets=c(1,8))),
                      sDom = '<"top"lif<"clear">>rt<"bottom"ipl<"clear">>')) %>%
  formatStyle(
    'result',
    backgroundColor = styleEqual(
      c("W", "L"), c('lightgreen', 'lightpink')))

})
 
gumdad$box <- sapply(gumdad$box, function(x)
            toString(tags$a(href=paste0("https://dadgumboxscores.com/", x), target="_blank", "Box Score")))


# FACTORS SERVER TAB -----------------------------------------------------------------------------------
output$tabletwo = renderDT({
    datatwo <- factors
  if (input$seasonfactors != "All") {
    datatwo <- datatwo[datatwo$season == input$seasonfactors,]
  }
   if (input$opponentfactors != "All") {
    datatwo <- datatwo[datatwo$opponent == input$opponentfactors,]
  }
 
 if(!is.null(input$resultfactors)) {
      datatwo <- datatwo[datatwo$result == input$resultfactors,]
    }
  datatable(datatwo, rownames = FALSE, escape = FALSE, selection = 'none',
                    colnames = c('Season', 'Opponent', 'Result', 'Pace', 'UNC PPP', 'eFG', 'TO%', 
                      'OR%', 'FTR', 'Opp PPP', 'eFG', 'TO%', 'DR%', 'FTR'),
                    options = list(paging = FALSE, scrollX = TRUE,
                      sDom = '<"top"lif<"clear">>rt<"bottom"ipl<"clear">>') ) %>%
        formatRound(c('ppp', 'oppp'), 2) %>%
formatRound(c('efg', 'to', 'or', 'ft', 'oefg', 'oto', 'oor', 'oft'), 1) %>%
  formatStyle(
    'result',
    backgroundColor = styleEqual(
      c("W", "L"), c('lightgreen', 'lightpink')))

})



# REFS SERVER TAB --------------------------------------------------------------------------------------

output$tablethree = renderDT({
  datathree <- refs
  if (input$seasonrefs != "All") {
    datathree <- datathree[datathree$season == input$seasonrefs,]
  }
   if (input$opponentrefs != "All") {
    datathree <- datathree[datathree$opponent == input$opponentrefs,]
  }
 
 if(!is.null(input$resultrefs)) {
      datathree <- datathree[datathree$result == input$resultrefs,]
    }

  datatable(datathree, rownames = FALSE, escape = TRUE, selection = 'none',
                    colnames = c('Season', 'Opponent', 'Result', 'UNC', 'Opp', 'Official', 'Official', 
                      'Official', 'UNC Fouls', 'Opp Fouls', 'Total Fouls'),
                    options = list(paging = FALSE, scrollX = TRUE,
                      sDom = '<"top"lif<"clear">>rt<"bottom"ipl<"clear">>')) %>%
  formatStyle(
    'result',
    backgroundColor = styleEqual(
      c("W", "L"), c('lightgreen', 'lightpink')))
})


# OFFENSIVE SHOTS SERVER TAB -----------------------------------------------------------------------------

output$tablefour = renderDT({
  datafour <- offshots
  if (!is.null(input$efgshots)) {
    datafour <- datafour[datafour$efg >= input$efgshots[1] & datafour$efg <= input$efgshots[2],]
  }
  if (input$seasonshots != "All") {
    datafour <- datafour[datafour$season == input$seasonshots,]
  }
   if (input$opponentshots != "All") {
    datafour <- datafour[datafour$opponent == input$opponentshots,]
  }
 
 if(!is.null(input$resultshots)) {
      datafour <- datafour[datafour$result == input$resultshots,]
    }
  datatable(datafour, rownames = FALSE, escape = FALSE, selection = 'none',
                    colnames = c('Season', 'Opponent', 'Result', 'eFG', '2PTM', '2PTA', '2PT%', 
                      '3PTM', '3PTA', '3PT%', '3PTA%', 'FTM', 'FTA', 'FT%'),
                    options = list(paging = FALSE, scrollX = TRUE,
                      sDom = '<"top"lif<"clear">>rt<"bottom"ipl<"clear">>') ) %>%
        formatRound(c('efg', 'pmp', 'tmp', 'tmpa', 'tft'), 1) %>%
  formatStyle(
    'result',
    backgroundColor = styleEqual(
      c("W", "L"), c('lightgreen', 'lightpink')))
})

# DEFENSIVE SHOTS SERVER TAB -----------------------------------------------------------------------------
 
output$tablefive = renderDT({
  datafive <- defshots
  if (!is.null(input$oefgshots)) {
    datafive <- datafive[datafive$oefg >= input$oefgshots[1] & datafive$oefg <= input$oefgshots[2],]
  }
  if (input$seasondef != "All") {
    datafive <- datafive[datafive$season == input$seasondef,]
  }
   if (input$opponentdef != "All") {
    datafive <- datafive[datafive$opponent == input$opponentdef,]
  }
 
 if(!is.null(input$resultdef)) {
      datafive <- datafive[datafive$result == input$resultdef,]
    }

  datatable(datafive, escape = FALSE, rownames = FALSE, selection = 'none',
                    colnames = c('Season', 'Opponent', 'Result', 'eFG', '2PTM', '2PTA', '2PT%', '3PTM', 
                      '3PTA', '3PT%', '3PTA%', 'FTM', 'FTA', 'FT%'),
                    options = list(paging = FALSE, scrollX = TRUE,
                      sDom = '<"top"lif<"clear">>rt<"bottom"ipl<"clear">>') ) %>%
        formatRound(c('oefg', 'opmp', 'otmp', 'otmpa', 'otft'), 1) %>%
  formatStyle(
    'result',
    backgroundColor = styleEqual(
      c("W", "L"), c('lightgreen', 'lightpink')))


})

# PLOT PARTY SERVER TAB ----------------------------------------------------------------------------------

titledf <- data.frame(var1 = c("pace",  "ppp", "efg", "to", "or", "ft", "pmp", "tmp", "tmpa", "fpa", "fpt", 
                                  "tft", "oppp", "oefg", "oto", "oor", "oft", "opmp", "otmp", 
                                  "otmpa", "ofpa", "ofpt", "otft"),
                        
                        var2 = c("Pace", "UNC PPP", "UNC eFG", "UNC TO%", "UNC OR%", "UNC FTRate", 
                                  "UNC 2PT%", "UNC 3PT%", "UNC 3PTRate", "UNC FTM", "UNC FTA", "UNC FT%", "OPP PPP", 
                                  "OPP eFG", "OPP TO%", "OPP OR%", "OPP FTRate", "OPP 2PT%", "OPP 3PT%", 
                                  "OPP 3PTRate", "OPP FTM", "OPP FTA", "OPP FT%") 
  )
  

# Ensure factor levels are set for color consistency:



# For Dynamic Titles
titlex1 <- reactive({  as.character(titledf$var2[match(input$selectx1, titledf$var1)])    })
titley1 <- reactive({  as.character(titledf$var2[match(input$selecty1, titledf$var1)])    })

### Scatter 1 Get Data
mydf1 <- reactive({
  
  mydf1  <- plot
  
  if (input$yearinput=="All") { 
    mydf1
  }
  
  else
 
    if (input$yearinput!="All"){
      
      mydf1 <- mydf1 %>% filter(season %in% input$yearinput)
      
    }


     if (input$opponentinput=="All") { 
    mydf1
  }
  
  else
    
    if (input$opponentinput!="All"){
      
      mydf1 <- mydf1 %>% filter(opponent==input$opponentinput)
      
    }
  
  mydf1 <-  mydf1 %>% select(x1 = which(colnames(plot)==input$selectx1), y1=which(colnames(plot)==input$selecty1),
                             label, blind) 
  
})



reactive({
  mydf1 <- data.frame() 
    mydf1() %>% 
    ggvis(~x1, ~y1, key:= ~label,  opacity := 0.65) %>% 
    layer_points(size.hover := 200, fill := ~blind) %>% 
    scale_nominal("fill", domain = c("L", "W"), range =  c('lightpink', 'lightgreen')) %>% 
    add_legend(c("fill"), title="Result") %>%
    set_options(width = "auto", height = "auto") %>%
    add_tooltip(function(data) data$label) %>% 
    add_axis("x", title = titlex1(), 
             properties = axis_props(
      title = list(fontSize = 20, dx=-5),
      labels = list(fontSize = 16)) )  %>% 
    add_axis("y", title = titley1(), 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),
               labels = list(fontSize = 16)) )  
  
  })  %>%  bind_shiny("plot1", "plot1_ui")

# CHART ART SERVER TAB ----------------------------------------------------------------------------------

  output$GameShotChart <- renderPlot({
    game_shot_chart(game_id = input$dateBoxes,
                    heatmap = input$heatmapSelector)
  })

}
shinyApp(ui = ui, server = server)



