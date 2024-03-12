library("shiny")
library("shinydashboard")
library("plotly")

dashboardPage(
  dashboardHeader(title="MTG-TOP8 DC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tier List", tabName = "tierlist", icon = icon("dashboard")),
      menuItem("Single-card", tabName="scard",icon=icon("map")),
      menuItem("Deck Analysis", tabName = "decks", icon = icon("th")),
      menuItem("Source and data",tabName="source",icon=icon("source"))
    )
  ),
  dashboardBody(fluidPage(
    tabItems(
      tabItem(tabName = "tierlist",
        fluidRow(column(12,
          column(8,
            column(3,dateInput("date1","Start Date", value=NA)),
            column(3,dateInput("date2","End Date", value=NA)),
            column(2,numericInput("players","Tourn. Size",value=12)),
            # column(1,tags$br(),checkboxInput("other","Others",value=FALSE)),
            column(3,sliderInput("minPercent","Percent Min",min=0.1, max=10, step=0.1, value=1))
          ),
          column(4,verbatimTextOutput("dataBaseSetting"))
        )),
        fluidRow(
          column(7,plotOutput("plotTierList")),
          column(5,plotOutput("plotTierList2"))
        ),
        fluidRow(
          column(12,plotOutput("plotTierList3"))
        ),
      ),
      tabItem(tabName="decks",
        fluidRow(column(12,
          column(3,
            column(12,dateInput("date1.2","Start Date", value=NA)),
            column(12,dateInput("date2.2","End Date", value=NA)),
            column(12,sliderInput("minPercent2","Percent min",min=0.1, max=10, step=0.1, value=1)),
            column(12,selectInput("deckColor","Color By",choices = c("Win.rate",
                                                                  "Score",
                                                                  "Tierlist")))
          ),
            column(9,plotOutput("deckClustering",height="500px"))
          )
        )
      ),
      tabItem(tabName="scard",fluidRow(column(12,
        column(4,
          column(12,selectInput("selectCard","Select a card",choices=NULL)),
          column(12,plotlyOutput("scardCamenbert",width = 350, height = 350))
        ),
        column(8,plotlyOutput("scardBarplot"))
        ))
      ),
      tabItem(tabName="source",
        column(6,shinydashboard::box(title="Tier List Data",width=6)),
        column(6,shinydashboard::box(title="Deck Analysis Data",width=6)),
        column(6,shinydashboard::box(title="External Source",
            tags$a(href="https://docs.google.com/document/d/1wrr2BVYXy877eGJnVYu5xIb5QLJwxE2LXz7JYlMKDKw/edit",
            "Anael YAHI (2023)")
          )
        )
      )
    )
  ))
)

# fluidPage(
#   titlePanel("Duel Commander MTG_TOP8 Tier List"),
#   mainPanel(width = 12,
#     fluidRow(column(12,
#       column(8,
#         column(2,dateInput("date1","Start Date", value=NA)),
#         column(2,dateInput("date2","End Date", value=NA)),
#         column(2,numericInput("players","Tourn. Size",value=12)),
#         #column(1,tags$br(),checkboxInput("other","Others",value=FALSE)),
#         column(3,sliderInput("minPercent","Percent Min",min=0.1, max=10, step=0.1, value=1))
#       ),
#       column(4, verbatimTextOutput("dataBaseSetting"))
#     )),
#     fluidRow(
#       column(7,
#         plotOutput("plotTierList")
#       ),
#       column(5,
#         plotOutput("plotTierList2")
#       )
#     ),
#     fluidRow(column(12,
#       plotOutput("plotTierList3")
#     )),
#     fluidRow(column(12,
#       plotOutput("heatmap",height="600px",width="1200px")
#     )),
#     
#     fluidRow(
#       tags$h5("Source"),
#       tags$a(href="https://docs.google.com/document/d/1wrr2BVYXy877eGJnVYu5xIb5QLJwxE2LXz7JYlMKDKw/edit",
#              "Anael YAHI (2023)")
#     )
#   )
# )
