shinyUI(fluidPage(
  
  titlePanel("Visualizing Trends in Strike Activity in China"),
  
  fluidRow(
    column(8,
           p("This app visualizes strike activity in China over time using data collected by",
             a(href = "http://www.clb.org.hk/en/", "China Labour Bulletin"),
             ", a Hong Kong-based NGO. The app automatically pulls current data from CLB's",
             a(href="http://maps.clb.org.hk/strikes/en", "strike map"),
             "and then generates sparkline plots of monthly event counts from January 2011 through
             the most recent complete calendar month by province, by industry, and by strikers' claims."),
           p("This app was built by Jay Ulfelder; it started out as a",
             a(href="http://dartthrowingchimp.wordpress.com/2015/05/31/visualizing-strike-activity-in-china/", "blog post"),
             "on Dart-Throwing Chimp.")
    )
  ),
  
  hr(),
  
  h3("By Province"),
  
  plotOutput("province", width = "750px", height = "1000px"),
  
  hr(),
  
  h3("By Industry"),
  
  plotOutput("industry", width = "750px", height = "312.5px"),
  
  hr(),
  
  h3("By Strikers' Claims"),
  
  plotOutput("claim", width = "750px", height = "125px"),
  
  hr()

))