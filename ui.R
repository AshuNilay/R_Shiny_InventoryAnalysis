ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody())


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

######################################## Header ################################################################################

header <- dashboardHeader(  tags$li(a(href = 'http://172.24.47.175:89/',
                                      img(src = 'Z.png',
                                          title = "TML Manufacturing Digital Enterprise", height ="18px"),
                                      style = "padding-top:1px; padding-bottom:1px;"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://tatamotors.sharepoint.com/Sites/c-app/s-MTMDev/SitePages/myapps.aspx',
                                      img(src = 'Z2.png',
                                          title = "My Tata Motors", height ="18px"),
                                      style = "padding-top:1px; padding-bottom:1px;"),
                                    class = "dropdown"),
                            tags$li(
  class = "dropdown",
  tags$style(
    ".main-header {max-height: 50px; padding: 0px 1px 10px 50 px;
    font-size:24px; 
    font-weight:bold; 
    line-height:24px;}"),
  tags$style(
    ".main-header .logo {height: 60px; padding: 0px 1px;
    font-size:24px; 
    font-weight:bold; 
    line-height:30px;align:}"
  )                                                          
  ),
  title = HTML(
    "<div style = 'background-color: navy ; vertical-align:Top '>
    <img src = 'Capture.png' align = 'left' height = '60px' border-image-outset: 50px>
    <img src = 'T3.png' align = 'middle' height = '30px' border-image-outset: 0px> <br>
    Tata Motors - Inventory Analysis
    </div>"),
  titleWidth = "85%"
  )
  
                   
############################################################# SideBar ####################################################################

sidebar <- dashboardSidebar(width = "190px",
  collapsed = FALSE,tags$style(
    ".main-sidebar {float:top; margin-top:0px; padding-left:1px; padding-right:1px}" 
  ),
  h4("By: AME | Data Analytics",align = 'middle',style="color:Oragne;background-color:navy"),
  sidebarUserPanel("Nilay Adhwaryu",subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
  image = 'Screenshot_2019.jpeg'
  ),
  sidebarMenu(id = "tabs",
    tags$strong(tags$u(tags$h4('FY 2018-19 Dashboard',align='middle',style ="color:white"))),
    menuItem("ALL CV & PV ", tabName = "dash0", icon = icon("th"),                    #convertMenuItem(
    menuSubItem("Plants Wise", tabName = "dash1", icon = icon("angle-double-right"))),        
    conditionalPanel("input.tabs === 'dash1'",selectInput(inputId = "y5",
                                                          label = "Select Plant",
                                                          choices = c("CV-JSR" = "JSR","CV-PUNE" = "PUNE","CV-LKW" ="LKW","CV-DWD" ="DWD","CV-UTK" = "UTK","PV-Pune"= "KB","PV-SND"= "SND"), 
                                                          selected = "CV-JSR")),
    #tags$strong(tags$u(tags$h4('Pune CV Dashboard',align='left',style ="color:white"))),
  
    menuItem("Pune CV", tabName = "dash", icon = icon("dashboard"),
    #menuSubItem("Inventory Days", tabName = "dash2", icon = icon("angle-double-right")),
    menuSubItem("Monthly Trends", tabName = "dash3", icon = icon("angle-double-right")),         #convertMenuItem(
    menuSubItem("Pune CV Charts", icon = icon("fas fa-angle-double-down"), tabName = "dash4"),         
    conditionalPanel("input.tabs === 'dash4'",selectInput(inputId = "y1",
                label = "Select Month",
                choices = c("Mar2019" = "Mar","Febuaray2019" = "Feb","Januaray2019" ="Jan","December2018" ="Dec","November2018"= "Nov","October2018"= "Oct",
                            "September2018"= "Sep","August2018"= "Aug","July2018" = "Jul","June2018" = "Jun","May2018" = "May","April2018" = "Apr"), 
                selected = "Mar2019")),
       menuSubItem("Summary", icon = icon("fas fa-angle-double-down"), tabName = "summary"),
       conditionalPanel("input.tabs === 'summary'",selectInput(inputId = "y2",
                                                          label = "Select Month",
                                                          choices = c("Mar2019" = "Mar","Febuaray2019" = "Feb","Januaray2019" ="Jan","December2018" ="Dec","November2018"= "Nov","October2018"= "Oct",
                                                                      "September2018"= "Sep","August2018"= "Aug","July2018" = "Jul","June2018" = "Jun","May2018" = "May","April2018" = "Apr"), 
                                                          selected = "Mar2019")),
       conditionalPanel("input.tabs === 'summary'",selectInput(inputId = "y3",
                label = "Value classification",
                choices = c("110-Brought Out" = 110,"120-Imports" = 120,"230-WIP" = 230,"240-MASOP" = 240,"250-InTransit" = 250,"All Classes"), 
                selected = "All Classes")),
      conditionalPanel("input.tabs==='summary'", downloadButton("D_Data", "Download Table",align = 'right',style="color:green")),
    
                            # badgeLabel = "II", badgeColor = "green"),tabName = "summary"),
    #convertMenuItem(menuItem("Highlights", icon = icon("fas fa-bullhorn"), tabName = "Highlights",
                           #  badgeLabel = "IV", badgeColor = "yellow"),tabName = "Highlights"),
    menuSubItem("Data Accuracy", icon = icon("fas fa-bullhorn"), tabName = "DA"),
                             # badgeLabel = "III", badgeColor = "maroon"),tabName = "DA"),tabName = "dash4"),
    conditionalPanel("input.tabs === 'DA'",selectInput(inputId = "y4",
                                                            label = "Select Month",
                                                            choices = c("Mar2019" = "Mar","Febuaray2019" = "Feb","Januaray2019" ="Jan","December2018" ="Dec","November2018"= "Nov","October2018"= "Oct",
                                                                        "September2018"= "Sep","August2018"= "Aug","July2018" = "Jul","June2018" = "Jun","May2018" = "May","April2018" = "Apr"), 
                                                            selected = "Mar2019"))
)))      #,"dash4")
#red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

########################################## Dashboard _First One ######################################################################
Da1_1 <- fluidRow(box(title = "All Plants Direct Material Inventory Status (Excluding Finished Goods) ",width = 12
                       ,status = "success"
                       ,background = "navy"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE,
                       img(src='S7.png', align = "middle",width = "100%",height ="400px",alt="This is alternate text",style = "padding-top:2px; padding-right:2px;")))

Da1_2 <- fluidRow( box(title = "Chart - All Plants Direct Material Inventory Status (Excluding Finished Goods) ",width = 12
                       ,status = "success"
                       ,background = "navy"
                       ,solidHeader = TRUE 
                       ,collapsible = TRUE,
                       img(src='S4.png', align = "middle",width = "100%",height ="420px",style = "padding-top:10px; padding-right:10px;")))

###################################################################################


dash1_1 <-
  fluidRow( 
    box(
      title = "Pune CV - Monthly Trends - Inventory Valuation (All Values are in Cr.)",width = 12
      ,status = "success"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput(outputId ="plot_dash1_1", height = "300px")
    )
  )

dash1_2 <- fluidRow(
       box(title = "Pune CV - Monthly Trends - Inventory Valuation Class wise (All Values are in Cr.)",width = 12
           ,status = "success"
           ,background = "navy"
           ,solidHeader = TRUE 
           ,collapsible = TRUE 
           ,plotOutput(outputId ="plot_dash1_2", height = "300px")
         
       )
)
dash1_3 <- fluidRow(
  box(title = "Pune CV - Monthly Trends - Total Stock Units",width = 12
      ,status = "success"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput(outputId ="plot_dash1_3", height = "300px")
      
  )
)
dash1_4 <- fluidRow(
  box(title = "Pune CV - Monthly Trends - Total Unique Parts",width = 12
      ,status = "success"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput(outputId ="plot_dash1_4", height = "300px")
      
  )
)

################################################################################################


dash2_1 <-fluidRow( box(title = "Pune CV - Inventory Values and Inventory Days Trends",width = 12
                        ,status = "success"
                        ,background = "navy"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE,
                        img(src='G6.png', align = "middle",height ="400px",width ="100%",style = "padding-top:10px; padding-right:10px;")))


dash2_2 <-fluidRow( box(title = "JSR CV - Inventory Values and Inventory Days Trends",width = 12
                        ,status = "success"
                        ,background = "navy"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE,
                        img(src='G5.png', align = "middle",height ="400px",width ="100%",style = "padding-top:10px; padding-right:10px;")))




######################################
dash4_0 <- fluidRow( box(status = "success"
                     ,background = "navy", width = 12,height ='40',align = "left",style = "padding-top:0px",
                     h4("Pune CV - Total Inventory Details (Including Finished Goods)- All Values are in INR Cr.")))

dash4_1 <- 
  fluidRow(
    valueBoxOutput("Box1",width = 3),
    valueBoxOutput("Box2",width = 3),
    valueBoxOutput("Box3",width = 3),
    valueBoxOutput("Box4",width = 3)
  )

dash4_2<- 
  fluidRow(
    valueBoxOutput("Box5",width = 3),
    valueBoxOutput("Box6",width = 3),
    valueBoxOutput("Box7",width = 3),
    valueBoxOutput("Box8",width = 3)
  )


dash4_3<- 
  fluidRow( 
    box(
      title = "Inventory Valuation Class Wise"
      ,status = "primary"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput(outputId ="plot_dash4_3_1",height = "300px")
    )
    ,box(
      title = "Inventory Proportion Valuation Class Wise"
      ,status = "success"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput(outputId ="plot_dash4_3_2", height = "300px")
    ) 
  )
dash4_4 <- 
  fluidRow( 
    box(
      title = "Inventory Aging Class Wise"
      ,status = "success"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotOutput(outputId ="plot_dash4_4_1", height = "300px")
    )
    ,box(
      title = "Inventory Proportion Age wise"
      ,status = "danger"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput(outputId ="plot_dash4_4_2", height = "300px")
    ) 
  )
##################################Top Concerns ########################
TC_1 <- fluidRow(
  box(title = "Top 10 parts with high rate", status = "warning",width = 6,height ='400',solidHeader = TRUE 
        ,collapsible = TRUE,
        div(style = "overflow-x: scroll;overflow-y: scroll;font-size:70%;width: 100%;background color:yellow",
            DT::dataTableOutput(outputId ="TC_1",width = "100%", height = '30%'))),
  box(title = "Top 10 parts with high rate", status = "warning",width = 6,height ='400',solidHeader = TRUE 
      ,collapsible = TRUE,
      div(style = "overflow-x: scroll;overflow-y: scroll;font-size:70%;width: 100%;background color:yellow",
          DT::dataTableOutput(outputId ="TC_2",width = "100%", height = '30%'))
    
  )) #options = list(pageLength = 5, dom = 'tip'





###################################### Summary ############################
s_1 <- fluidRow(box(title = textOutput("selected_var"),status = "success", width = 12,collapsible = TRUE,solidHeader = TRUE,
         div(style = "overflow-x: scroll",DT::dataTableOutput(outputId ="table1",width = "100%", height = "100%"))))

#HTML(paste0(h4("Table of Inventory aging more then 2 years and inwarded in selected month")))),

s_2 <- fluidRow(
  valueBoxOutput("Box10",width = 3),
  valueBoxOutput("Box11",width = 3),
  valueBoxOutput("Box12",width = 3),
  valueBoxOutput("Box13",width = 3)
)
#s_2 <- fluidRow(
  
  #box(title = "Controls",width = 4,sliderInput("slider", "Number of observations:", 1, 100, 50)),
  #box(
  #  width = 3,selectInput(inputId = "t",
   #                       label = "Value classification",
   #                       choices = c("110-Brought Out" = 110,"120-Imports" = 120 ,"230-WIP" = 230 ,"240-MASOP" = 240,"250-InTransit" = 250), 
  #                        selected = "110-Brought Out")
  #),
  #box( width = 3, selectInput(inputId = "r",
   #                           label = "Age Group",
    #                          choices = c(">2 Years","2<Year>1","181<>365","90<>180","60<>90","30<>60"), 
    #                          selected = ">2 Years")
  #))



##################################HighLights #################################333333333
h_1 <-
  fluidRow( 
    box(
      title = "Top Highlights"
      ,status = "success"
      ,background = "navy"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,htmlOutput(outputId ="html1", height = "300px")
    )
)

h_2 <- fluidRow( box(
  title = "Maximum Rate", status = "warning",width = 4,
  "Box content here", br(), "More box content",
  tableOutput(outputId = "table4")
))
####################################### Data Accuracy ###############################################
D_1 <- fluidRow(box(title = "Parts with Zero Rate and Total Value > 0", status = "warning",width = 12,height ='100',solidHeader = TRUE 
                    ,collapsible = FALSE,uiOutput(outputId = "n",height = "30px" )))
                            

D_2 <- fluidRow(
  box(  title = "List of Part Numbers", status = "warning",width = 12,height ='500',solidHeader = TRUE 
        ,collapsible = TRUE,
        div(style = "overflow-x: scroll;overflow-y: scroll;font-size:70%;width: 100%;background color:yellow",
            DT::dataTableOutput(outputId ="table_3",width = "100%", height = '30%')))) #options = list(pageLength = 5, dom = 'tip'



#############################################################################################
D_3 <- fluidRow(box(title = "Parts with discrepancies in Total Value", status = "warning",width = 12,height ='100',solidHeader = TRUE 
                    ,collapsible = FALSE,uiOutput(outputId = "n2",height = "30px" )
                    
))

D_4 <- fluidRow(
  box(  title = "Records with discrepancies in Total Value > 1 INR", status = "warning",width = 12,height ='700',solidHeader = TRUE 
        ,collapsible = TRUE,
        div(style = "overflow-x: scroll;overflow-y: scroll;font-size:84%;width: 100%;background color:yellow",
            DT::dataTableOutput(outputId ="table_5",width = "100%", height = '40%')))) 

######################################################################################
body <- dashboardBody(tags$head(tags$style(
  HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
)),
  tabItems(
    #tabitem(tabName = "dash0",Da0_1,Da0_2)
    tabItem(tabName = "dash1",Da1_1,Da1_2),
    tabItem(tabName = "dash2",dash2_1),  #dash2_2
    tabItem(tabName = "dash3",dash1_1,dash1_2,dash1_3,dash1_4),
    tabItem(tabName = "dash4",dash4_0,dash4_1,dash4_2,dash4_3,dash4_4),
    tabItem(tabName = "TC",TC_1),
    tabItem(tabName = "summary",s_1),
    tabItem(tabName = "Highlights", h_1, h_2),
    tabItem(tabName = "DA",D_1,D_2,D_4)
))

ui <- dashboardPage(title = 'Inventory Analysis - 2018', header, sidebar, body, skin='red')
