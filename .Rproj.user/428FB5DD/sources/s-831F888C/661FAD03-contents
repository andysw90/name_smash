library(shiny)
library(shinyWidgets)
library(shinythemes)
library(elo)

boys <- readLines("data/top1000_boys.txt")
boys <- boys[1:10]
button_colour <- "primary"

elo_df <- data.frame(names = boys,
                     elo = 1500,
                     wins = 0,
                     losses = 0,stringsAsFactors = FALSE)
competitions <- data.frame(t(combn(boys,m = 2)),stringsAsFactors = FALSE)
competitions <- competitions[sample(x = 1:nrow(competitions),size = nrow(competitions),replace = FALSE),]
colnames(competitions) <- c("name1","name2")

write.table(competitions,"competitions_df.txt")
write.table(elo_df,"elo_df.txt")

update_elo <- function(elo_df,this_result){
  
  name1_ind <- which(elo_df$names==this_result$name1)
  name2_ind <- which(elo_df$names==this_result$name2)
  
  # Pre-match ratings
  name1_elo <- elo_df$elo[name1_ind]
  name2_elo <- elo_df$elo[name2_ind]
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = this_result$outcome,
                      elo.A = name1_elo,
                      elo.B = name2_elo,
                      k = 50)
  elo_df$elo[name1_ind] <- new_elo$elo.A
  elo_df$elo[name2_ind] <- new_elo$elo.B
  
  if(this_result$outcome==1){
    elo_df$wins[name1_ind] <- elo_df$wins[name1_ind] + 1  
    elo_df$losses[name2_ind] <- elo_df$losses[name2_ind] + 1
  }else{
    elo_df$wins[name2_ind] <- elo_df$wins[name2_ind] + 1
    elo_df$losses[name1_ind] <- elo_df$losses[name1_ind] + 1
  }
  
  return(elo_df)
}
ui <- 
  fluidPage(theme = shinytheme("journal"),
            titlePanel(
              title =
                tags$link(rel = "icon", type = "image/png", href = "https://www.kindpng.com/picc/m/205-2051178_wild-thornberrys-nigel-meme-face-hd-png-download.png"),
              "NameSmash"
            ),
            column(4,offset = 4,align = "center",
                   hr(),
                   h2("NameSmash"),
                   hr()),
            
            
            fluidRow(
              
              column(3,offset = 3,align = "center",
                     hr(),
                     actionBttn("name1",
                                style = "minimal", 
                                color = button_colour,
                                label = competitions[1,1])),
              column(3,align = "center",
                     hr(),
                     actionBttn("name2",
                                style = "minimal", 
                                color = button_colour,
                                label = competitions[1,2])),
              
              
            ),
            fluidRow(
              column(4,offset = 4,align = "center",
                     tableOutput("elo_top"))
            ),
            textOutput("counter")
  )

server <- function(input, output, session) {
  current_ind <- reactive({input$name1 + input$name2 + 1})
  
  output$counter <- renderText(current_ind())
  observeEvent(input$name1,{
    elo_df <- read.table("elo_df.txt",stringsAsFactors = FALSE)
    this_result <- data.frame(name1 = competitions$name1[current_ind()-1],
                              name2 = competitions$name2[current_ind()-1],
                              outcome = 1,stringsAsFactors = FALSE)
    new_elo <- update_elo(elo_df = elo_df,this_result)
    write.table(new_elo,"elo_df.txt")
    updateActionButton(session,inputId = "name1",label = competitions$name1[current_ind()])
    updateActionButton(session,inputId = "name2",label = competitions$name2[current_ind()])
  })
  observeEvent(input$name2,{
    elo_df <- read.table("elo_df.txt",stringsAsFactors = FALSE)
    this_result <- data.frame(name1 = competitions$name1[current_ind()-1],
                              name2 = competitions$name2[current_ind()-1],
                              outcome = 0,stringsAsFactors = FALSE)
    new_elo <- update_elo(elo_df = elo_df,this_result)
    write.table(new_elo,"elo_df.txt")
    updateActionButton(session,inputId = "name1",label = competitions$name1[current_ind()])
    updateActionButton(session,inputId = "name2",label = competitions$name2[current_ind()])
  })
  get_top <- eventReactive(c(input$name1,input$name2),{
    elo_df <- read.table("elo_df.txt")
    elo_df <- elo_df[sort.int(elo_df$elo,decreasing = TRUE,index.return = TRUE)$ix,]
    elo_df$elo <- round(elo_df$elo)
    class(elo_df$elo) <- "integer"
    colnames(elo_df) <- c("Name","Score","Wins","Losses")
    elo_df
  })
  output$elo_top <- renderTable(get_top())
  
  
}

shinyApp(ui, server)