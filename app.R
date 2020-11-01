# Load packages -------------------------------------------------------------------
library(shiny)
library(reactable)
library(readr)
library(shinydashboard)
library(dplyr)
library(htmlwidgets)
library(htmltools)

# Load data -----------------------------------------------------------------------
gumdad <- read_csv(
  "data/boxes.csv",
  col_types =
    cols(
      season = col_character(),
      date = col_skip(),
      opponent = col_character(),
      result = col_factor(),
      location = col_character(),
      type = col_character(),
      longtype = col_skip(),
      unc = col_number(),
      opp = col_number(),
      OT = col_skip(),
      box = col_character()
    )
)
factors <- read_csv(
  "data/factors.csv",
  col_types =
    cols(
      season = col_character(),
      opponent = col_character(),
      result = col_factor(),
      pace = col_number(),
      ppp = col_number(),
      oppp = col_number(),
      efg = col_number(),
      oefg = col_number(),
      to = col_number(),
      oto = col_number(),
      or = col_number(),
      oor = col_number(),
      ft = col_number(),
      oft = col_number()
    )
)

refs <- read_csv(
  "data/refs.csv",
  col_types =
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

offshots <- read_csv(
  "data/offshots.csv",
  col_types =
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

defshots <- read_csv(
  "data/defshots.csv",
  col_types =
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

spread <- read_csv(
  "data/point_spreads.csv",
  col_types =
    cols(
      season = col_character(),
      opponent = col_character(),
      result = col_factor(),
      unc = col_number(),
      opp = col_number(),
      pointspread = col_number(),
      refone = col_character(),
      reftwo = col_character(),
      refthree = col_character()
    )
)

rafters <- read_csv(
  "data/rafters.csv",
  col_types =
    cols(
      player = col_character(),
      retired = col_skip(),
      games = col_number(),
      fgm = col_number(),
      fga = col_number(),
      pct = col_number(),
      tfgm = col_number(),
      tfga = col_number(),
      tfgpct = col_number(),
      ftm = col_number(),
      fta = col_number(),
      ftpct = col_number(),
      reb = col_number(),
      pf = col_number(),
      assists = col_number(),
      tos = col_number(),
      blocks = col_number(),
      stls =  col_number()
    )
)

# Load UI -----------------------------------------------------------------------
ui <- dashboardPage(
  # Add header -------------------------------------------------------------------------------
  dashboardHeader(
    title = "dadgumboxscores",
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      tags$a(class = "fab fa-twitter-square",
             href = "https://twitter.com/dadgumboxscores/")
    )
  ),
  
  # Add sidebar -----------------------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Box Scores", tabName = "boxscores", icon = icon("th")),
      menuItem(
        "Four Factors",
        tabName = "fourfactors",
        icon = icon("dice-four")
      ),
      menuItem(
        "Referees and Fouls",
        tabName = "refs",
        icon = icon("users")
      ),
      menuItem(
        "Offense: Shooting Stats",
        tabName = "shots",
        icon = icon("arrow-circle-up")
      ),
      menuItem(
        "Defense: Shooting Stats",
        tabName = "def",
        icon = icon("arrow-circle-down")
      ),
      menuItem(
        "Point Spreads",
        tabName = "spread",
        icon = icon("money-check")
      ),
      menuItem("Rafters",
               tabName = "rafters",
               icon = icon("tshirt")),
      menuItem("FAQs", tabName = "purpose", icon = icon("question"))
    )
  ),
  # Start Dashboard body ---------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "dadgum.css")
    ),
    style = "background-color: #ecf0f5",
    tabItems(
      # BOX SCORES TAB ----------------------------------------------------------------------------
      tabItem(
        tabName = "boxscores",
        # HTML for factors tab -------------------------------------------------------------------------
        fluidRow(width = 12,
                 div(
                   class = "center",
                   titlePanel("Box Scores Game-By-Game"),
                   h3("Filter by season, result, opponent, or overtime"),
                   p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
                 )),
        # Filters for box scores tab ----------------------------------------------------------------
        box(
          width = 4,
          height = 125,
          selectInput("season",
                      label = "Season:",
                      c("All",
                        unique(
                          as.character(gumdad$season)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          selectInput("opponent",
                      label = "Opponent:",
                      c("All",
                        unique(
                          as.character(gumdad$opponent)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          checkboxGroupInput("result",
                             
                             label = "Result:",
                             c(unique(
                               as.character(gumdad$result)
                             )))
        ),
        box(width = 12,
            reactableOutput("tableone"))
      ),
      # FACTORS TAB ----------------------------------------------------------------------------------
      tabItem(
        tabName = "fourfactors",
        # HTML for factors tab -------------------------------------------------------------------------
        fluidRow(width = 12,
                 div(
                   class = "center",
                   titlePanel("Four Factors Game-By-Game"),
                   h3("Filter by season, wins, losses, or opponent"),
                   p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
                 )),
        box(
          width = 4,
          height = 125,
          selectInput("seasonfactors",
                      label = "Season:",
                      c("All",
                        unique(
                          as.character(factors$season)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          selectInput("opponentfactors",
                      label = "Opponent:",
                      c("All",
                        unique(
                          as.character(factors$opponent)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          checkboxGroupInput("resultfactors",
                             label = "Result:",
                             c(unique(
                               as.character(factors$result)
                             )))
        ),
        # Reactable for factors tab ----------------------------------------------------------------------
        box(width = 12,
            reactableOutput('tabletwo'))
      ),
      # REFS TAB ----------------------------------------------------------------------------------
      tabItem(
        tabName = "refs",
        # HTML for factors tab -------------------------------------------------------------------------
        fluidRow(width = 12,
                 div(
                   class = "center",
                   titlePanel("Officials Game-By-Game"),
                   h3("Filter by season, wins, losses, or opponent"),
                   p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
                 )),
        box(
          width = 4,
          height = 125,
          selectInput("seasonrefs",
                      label = "Season:",
                      c("All",
                        unique(
                          as.character(refs$season)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          selectInput("opponentrefs",
                      label = "Opponent:",
                      c("All",
                        unique(
                          as.character(refs$opponent)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          checkboxGroupInput("resultrefs",
                             label = "Result:",
                             c(unique(
                               as.character(refs$result)
                             )))
        ),
        # Reacatable for refs tab ----------------------------------------------------------------------
        box(width = 12,
            reactableOutput('tablethree'))
      ),
      # OFFENSIVE SHOTS TAB ----------------------------------------------------------------------------------
      tabItem(
        tabName = "shots",
        # HTML for offensive shots tab -------------------------------------------------------------------------
        fluidRow(
          width = 12,
          div(
            class = "center",
            titlePanel("Offensive Shots Game-By-Game"),
            h3("Filter by season, wins, losses, or opponent"),
            p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
          ),
          # Filters for offensive shots tab ------------------------------------------------------------------
          column(
            width = 12,
            offset = 3,
            class = "center",
            box(
              width = 6,
              sliderInput(
                "efgshots",
                label = "Effective Field Goal Percentage (eFG)",
                min = 0,
                max = 90,
                value = c(0, 80)
              )
            )
          ),
          box(
            width = 4,
            height = 125,
            selectInput("seasonshots",
                        label = "Season:",
                        c("All",
                          unique(
                            as.character(offshots$season)
                          )))
          ),
          box(
            width = 4,
            height = 125,
            selectInput("opponentshots",
                        label = "Opponent:",
                        c("All",
                          unique(
                            as.character(offshots$opponent)
                          )))
          ),
          box(
            width = 4,
            height = 125,
            checkboxGroupInput("resultshots",
                               label = "Result:",
                               c(unique(
                                 as.character(offshots$result)
                               )))
          ),
          # Reactable for offensive shots tab -----------------------------------------------------------------
          box(width = 12,
              reactableOutput('tablefour'))
        )
      ),
      # DEFENSIVE SHOTS TAB --------------------------------------------------------------------------------
      tabItem(
        tabName = "def",
        # HTML for offensive shots tab -------------------------------------------------------------------------
        fluidRow(
          width = 12,
          div(
            class = "center",
            titlePanel("Defensive Shots Game-By-Game"),
            h3("Filter by season, wins, losses, or opponent"),
            p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
          ),
          # Filters for defensive shots tab --------------------------------------------------------------------
          column(
            width = 12,
            offset = 3,
            class = "center",
            box(
              width = 6,
              sliderInput(
                "oefgshots",
                label = "Opponent Effective Field Goal Percentage (eFG)",
                min = 0,
                max = 90,
                value = c(0, 80)
              )
            )
          ),
          box(
            width = 4,
            height = 125,
            selectInput("seasondef",
                        label = "Season:",
                        c("All",
                          unique(
                            as.character(defshots$season)
                          )))
          ),
          box(
            width = 4,
            height = 125,
            selectInput("opponentdef",
                        label = "Opponent:",
                        c("All",
                          unique(
                            as.character(defshots$opponent)
                          )))
          ),
          box(
            width = 4,
            height = 125,
            checkboxGroupInput("resultdef",
                               label = "Result:",
                               c(unique(
                                 as.character(defshots$result)
                               )))
          ),
          # Reactable for defensive shots tab -------------------------------------------------------------------
          box(width = 12,
              reactableOutput('tablefive'))
        )
      ),
      # SPREAD TAB ----------------------------------------------------------------------------------
      tabItem(
        tabName = "spread",
        # HTML for factors tab -------------------------------------------------------------------------
        fluidRow(width = 12,
                 div(
                   class = "center",
                   titlePanel("Point Spreads Game-By-Game"),
                   h3("Filter by season, wins, losses, or opponent"),
                   p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
                 )),
        box(
          width = 4,
          height = 125,
          selectInput("seasonspread",
                      label = "Season:",
                      c("All",
                        unique(
                          as.character(spread$season)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          selectInput("opponentspread",
                      label = "Opponent:",
                      c("All",
                        unique(
                          as.character(spread$opponent)
                        )))
        ),
        box(
          width = 4,
          height = 125,
          checkboxGroupInput("resultspread",
                             label = "Result:",
                             c(unique(
                               as.character(spread$result)
                             )))
        ),
        # Reactable for spread tab ----------------------------------------------------------------------
        box(width = 12,
            reactableOutput('tablesix'))
      ),
      # RAFTERS TAB ----------------------------------------------------------------------------------
      tabItem(
        tabName = "rafters",
        # HTML for factors tab -------------------------------------------------------------------------
        fluidRow(width = 12,
                 div(
                   class = "center",
                   titlePanel("Jerseys in the Rafters"),
                   h3("List includes retired and honored jerseys."),
                   p(class = "d-sm-none", "You're on a mobile screen, flip to landscape or scroll")
                 )),
        # Reactable for spread tab ----------------------------------------------------------------------
        box(width = 12,
            reactableOutput('tableseven'))
      ),
      # PURPOSE TAB -----------------------------------------------------------------------------------------
      tabItem(tabName = "purpose",
              fluidRow(
                column(
                  width = 8,
                  h2("What is this dadgum thing?"),
                  p(
                    "Dadgum box scores is a collection of every UNC men’s basketball box score since the
  2003-04 season."
                  ),
                  
                  p(
                    "I put together this site because I wanted it to be easier to find old UNC box scores.
  I was tired of navigating crappy sites and not finding what I wanted. It shouldn’t be that
  hard to find a box score. It should be fast, easy, and organized."
                  ),
                  
                  tags$br(),
                  h4("What else will you find on this site?"),
                  p(
                    "The focus of this dadgum doodad has expanded to include other data, including:"
                  ),
                  tags$li("officials: a list of all refs for every game since '03-04"),
                  tags$li(
                    "four factors: game-by-game numbers of the building blocks of efficiency"
                  ),
                  tags$li("shooting data: game-by-game shooting stats since '03-04"),
                  p(
                    "This site doesn’t include schedules, player stats, or interviews. I’m not a reporter, and I’m not going
  to break any news."
                  ),
                  tags$br(),
                  
                  h4("Why only data since the 2003-2004 season?"),
                  p(
                    "Couple reasons. First, the difficulty goes way up trying to find box scores prior to this date.
 Even finding box scores from the '03-04 season took some elbow grease."
                  ),
                  p(
                    "Second, the cutoff date makes it a bit more current. This was Roy Williams first season, and it felt
 like a clean break and good starting point."
                  ),
                  tags$br(),
                  
                  h4("Can I use the data on this site?"),
                  p(
                    "Yes. Feel free to use this data or link to it on your own sites or for your own research. If you want
  to refer to dadgumboxscores.com as the source, you deserve a high-five."
                  ),
                  
                  tags$br(),
                  h4("I found an error, what should I do next?"),
                  p(
                    "If you can’t find something you’re looking for or spot an typo or if data looks blatantly wrong,
  please let me know via",
                    tags$a(href = "mailto:cbbstatshelp.com", "email.")
                  ),
                  tags$br(),
                  h4("Anything else?"),
                  p(
                    "This tool is built in R using the Shiny Dashboard, Readr, reactable, and several other packages. All data is
from box scores via goheels.com and spreads via covers.com. If you have suggestions on how to improve it or make it better, I'm all ears."
                  ),
                  tags$br()
                )
              ))
    )
  )
)
# OPEN SERVER ------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  # BOX SCORES SERVER TAB --------------------------------------------------------------------------------
  output$tableone = renderReactable({
    data <- gumdad
    if (input$season != "All") {
      data <- data[data$season == input$season,]
    }
    if (input$opponent != "All") {
      data <- data[data$opponent == input$opponent,]
    }
    if (!is.null(input$result)) {
      data <- data[data$result == input$result,]
    }
    reactable(
      data,
      searchable = TRUE,
      resizable = TRUE,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      bordered = TRUE,
      columns = list(
        season = colDef(name = "Season"),
        opponent = colDef(name = "Opponent"),
        location = colDef(name = "Location"),
        type = colDef(name = "Type"),
        result = colDef(
          name = "Result",
          maxWidth = 55,
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            htmltools::div(class = class, value)
          }
        ),
        unc = colDef(name = "UNC", maxWidth = 75),
        opp = colDef(name = "Opponent", maxWidth = 75),
        box = colDef(
          name = "Box Score",
          cell = function(value) {
            htmltools::tags$a(href = value, target = "_blank", "Box Score")
          }
        )
      )
    )
    
  })
  # FOUR FACTORS SERVER TAB --------------------------------------------------------------------------------
  output$tabletwo = renderReactable({
    datatwo <- factors
    if (input$seasonfactors != "All") {
      datatwo <- datatwo[datatwo$season == input$seasonfactors,]
    }
    if (input$opponentfactors != "All") {
      datatwo <- datatwo[datatwo$opponent == input$opponentfactors,]
    }
    
    if (!is.null(input$resultfactors)) {
      datatwo <- datatwo[datatwo$result == input$resultfactors,]
    }
    
    reactable(
      datatwo,
      searchable = TRUE,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      resizable = TRUE,
      columns = list(
        season = colDef(name = "Season", maxWidth = 70),
        opponent = colDef(name = "Opponent", maxWidth = 150),
        result = colDef(
          name = "Result",
          maxWidth = 55,
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            htmltools::div(class = class, value)
          }
        ),
        pace = colDef(name = "Pace", maxWidth = 50),
        ppp = colDef(
          name = "UNC PPP",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        oppp = colDef(
          name = "OPP PPP",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        efg = colDef(
          name = "eFG",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        oefg = colDef(
          name = "D eFG",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        to = colDef(
          name = "TO",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        oto = colDef(
          name = "D TO",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        or = colDef(
          name = "OR",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        oor = colDef(
          name = "DR",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        ft = colDef(
          name = "FT",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        oft = colDef(
          name = "D FT",
          maxWidth = 75,
          format = colFormat(digits = 2)
        )
      )
    )
  })
  # REFS SERVER TAB --------------------------------------------------------------------------------
  output$tablethree = renderReactable({
    datathree <- refs
    if (input$seasonrefs != "All") {
      datathree <- datathree[datathree$season == input$seasonrefs,]
    }
    if (input$opponentrefs != "All") {
      datathree <- datathree[datathree$opponent == input$opponentrefs,]
    }
    
    if (!is.null(input$resultrefs)) {
      datathree <- datathree[datathree$result == input$resultrefs,]
    }
    reactable(
      datathree,
      searchable = TRUE,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      columns = list(
        season = colDef(name = "Season", maxWidth = 75),
        opponent = colDef(name = "Opponent", maxWidth = 150),
        result = colDef(
          name = "Result",
          maxWidth = 75,
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            htmltools::div(class = class, value)
          }
        ),
        unc = colDef(name = "UNC"),
        opp = colDef(name = "Opp"),
        off1 = colDef(name = "Ref 1"),
        off2 = colDef(name = "Ref 2"),
        off3 = colDef(name = "Ref 3"),
        uncfouls = colDef(name = "UNC Fouls", maxWidth = 50),
        oppfouls = colDef(name = "Opp Fouls", maxWidth = 50),
        sumfouls = colDef(name = "Total Fouls", maxWidth = 50)
      )
    )
  })
  
  # OFFENSIVE SHOTS SERVER TAB -----------------------------------------------------------------------------
  
  output$tablefour = renderReactable({
    datafour <- offshots
    if (!is.null(input$efgshots)) {
      datafour <-
        datafour[datafour$efg >= input$efgshots[1] &
                   datafour$efg <= input$efgshots[2],]
    }
    if (input$seasonshots != "All") {
      datafour <- datafour[datafour$season == input$seasonshots,]
    }
    if (input$opponentshots != "All") {
      datafour <- datafour[datafour$opponent == input$opponentshots,]
    }
    
    if (!is.null(input$resultshots)) {
      datafour <- datafour[datafour$result == input$resultshots,]
    }
    
    reactable(
      datafour,
      searchable = TRUE,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      columns = list(
        season = colDef(name = "Season", maxWidth = 75),
        opponent = colDef(name = "Opponent", maxWidth = 150),
        result = colDef(
          name = "Result",
          maxWidth = 75,
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            htmltools::div(class = class, value)
          }
        ),
        efg = colDef(
          name = "eFG",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        pm = colDef(name = "2PTM", maxWidth = 75),
        pa = colDef(name = "2PTA", maxWidth = 75),
        pmp = colDef(
          name = "2PT%",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        tpm = colDef(name = "3PTM", maxWidth = 75),
        tpa = colDef(name = "3PTA", maxWidth = 75),
        tmp = colDef(
          name = "3PT%",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        tmpa = colDef(
          name = "3PTRate",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        fpa = colDef(name = "FTM", maxWidth = 75),
        fpt = colDef(name = "FTM", maxWidth = 75),
        tft = colDef(
          name = "FTA",
          maxWidth = 75,
          format = colFormat(digits = 2)
        )
      )
    )
  })
  
  # DEFENSIVE SHOTS SERVER TAB -----------------------------------------------------------------------------
  
  output$tablefive = renderReactable({
    datafive <- defshots
    if (!is.null(input$oefgshots)) {
      datafive <-
        datafive[datafive$oefg >= input$oefgshots[1] &
                   datafive$oefg <= input$oefgshots[2],]
    }
    if (input$seasondef != "All") {
      datafive <- datafive[datafive$season == input$seasondef,]
    }
    if (input$opponentdef != "All") {
      datafive <- datafive[datafive$opponent == input$opponentdef,]
    }
    
    if (!is.null(input$resultdef)) {
      datafive <- datafive[datafive$result == input$resultdef,]
    }
    
    reactable(
      datafive,
      searchable = TRUE,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      columns = list(
        season = colDef(name = "Season", maxWidth = 75),
        opponent = colDef(name = "Opponent", maxWidth = 150),
        result = colDef(
          name = "Result",
          maxWidth = 75,
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            htmltools::div(class = class, value)
          }
        ),
        oefg = colDef(
          name = "eFG",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        opm = colDef(name = "2PTM", maxWidth = 75),
        opa = colDef(name = "2PTA", maxWidth = 75),
        opmp = colDef(
          name = "2PT%",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        otpm = colDef(name = "3PTM", maxWidth = 75),
        otpa = colDef(name = "3PTA", maxWidth = 75),
        otmp = colDef(
          name = "3PT%",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        otmpa = colDef(
          name = "3PTRate",
          maxWidth = 75,
          format = colFormat(digits = 2)
        ),
        ofpa = colDef(name = "FTM", maxWidth = 75),
        ofpt = colDef(name = "FTM", maxWidth = 75),
        otft = colDef(
          name = "FTA",
          maxWidth = 75,
          format = colFormat(digits = 2)
        )
      )
    )
  })
  # SPREAD SERVER TAB --------------------------------------------------------------------------------
  output$tablesix = renderReactable({
    datasix <- spread
    if (input$seasonspread != "All") {
      datasix <- datasix[datasix$season == input$seasonspread,]
    }
    if (input$opponentspread != "All") {
      datasix <- datasix[datasix$opponent == input$opponentspread,]
    }
    
    if (!is.null(input$resultspread)) {
      datasix <- datasix[datasix$result == input$resultspread,]
    }
    reactable(
      datasix,
      searchable = TRUE,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      columns = list(
        season = colDef(name = "Season", maxWidth = 75),
        opponent = colDef(name = "Opponent", maxWidth = 150),
        result = colDef(
          name = "Result",
          maxWidth = 75,
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            htmltools::div(class = class, value)
          }
        ),
        unc = colDef(name = "UNC", maxWidth = 75),
        opp = colDef(name = "Opp", maxWidth = 75),
        pointspread = colDef(
          name = "Spread",
          maxWidth = 75,
          cell = function(value) {
            if (value >= 0 && !is.na(value))
              paste0("+", value)
            else
              value
          },
          style = function(value) {
            color <- if (value > 0 && !is.na(value)) {
              "#e00000"
            } else if (value < 0 && !is.na(value)) {
              "#008000"
            }
            list(fontWeight = 600, color = color)
          }
        ),
        refone = colDef(name = "Ref 1"),
        reftwo = colDef(name = "Ref 2"),
        refthree = colDef(name = "Ref 3")
      )
    )
  })
  # RAFTERS SERVER TAB --------------------------------------------------------------------------------
  output$tableseven = renderReactable({
    dataseven <- rafters
    retired <-
      c(
        "Jack Cobb",
        "Lennie Rosenbluth",
        "Phil Ford",
        "George Glamack",
        "Michael Jordan",
        "Antawn Jamison",
        "Tyler Hansbrough",
        "James Worthy"
      )
    reactable(
      dataseven,
      searchable = TRUE,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      columns = list(
        player = colDef(
          name = "Player",
          maxWidth = 250,
          style = function(value, index, name) {
            color <- if (is.character(value) && value %in% retired) {
              list(background = "#ffffa1", fontWeight = 700)
            }
          }
        ),
        games = colDef(name = "Games", maxWidth = 60),
        points = colDef(name = "Points", maxWidth = 75),
        fgm = colDef(name = "FGM", maxWidth = 50),
        fga = colDef(name = "FGA", maxWidth = 50),
        pct = colDef(
          name = "FG%",
          format = colFormat(digits = 1),
          maxWidth = 50
        ),
        tfgm = colDef(name = "3PM", maxWidth = 60),
        tfga = colDef(name = "3PA", maxWidth = 50),
        tfgpct = colDef(
          name = "3PT%",
          format = colFormat(digits = 1),
          maxWidth = 60
        ),
        ftm = colDef(name = "FTM", maxWidth = 50),
        fta = colDef(name = "FTA", maxWidth = 50),
        ftpct = colDef(
          name = "FT%",
          format = colFormat(digits = 1),
          maxWidth = 75
        ),
        reb = colDef(name = "Rebounds", maxWidth = 85),
        pf = colDef(name = "Fouls", maxWidth = 60),
        assists = colDef(name = "Assists", maxWidth = 70),
        tos = colDef(name = "TOs", maxWidth = 60),
        blocks = colDef(name = "Blocks", maxWidth = 70),
        stls = colDef(name = "Steals", maxWidth = 70)
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)