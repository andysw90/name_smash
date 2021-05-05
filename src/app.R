library(shiny)
library(elo)
boys <- readLines("data/top1000_boys.txt")
boys <- boys[1:20]

elo_df <- data.frame(names = unique(c(result$name1,
                                      result$name2)),
                     elo = 1500,stringsAsFactors = FALSE)
competitions <- data.frame(t(combn(boys,m = 2)))
colnames(competitions) <- c("name1","name2")
competitions$result <- NA

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
                      k = 100)
  elo_df$elo[name1_ind] <- new_elo$elo.A
  elo_df$elo[name2_ind] <- new_elo$elo.B
  
  return(elo_df)
}
ui <- fluidPage(
  fluidRow(
    column(6,
           actionButton("name1",label = competitions[1,1])),
    column(6,
           actionButton("name2",label = competitions[1,2]))
  )
)

server <- function(input, output, session) {
  current_ind <- reactive({input$name1 + input$name2})
  eventReactive(input$name1,{
    elo_df <- read.table("elo_df.txt")
    this_result <- data.frame(name1 = competitions$name1[current_ind()],
                              name2 = competitions$name2[current_ind()],
                              outcome = 0,stringsAsFactors = FALSE)
    new_elo <- update_elo(elo_df = elo_df,this_result)
    write.table(new_elo,"elo_df.txt")
    updateSelectInput(session,inputId = "name1",label = competitions$name1[current_ind()+1])
    updateSelectInput(session,inputId = "name2",label = competitions$name2[current_ind()+1])
  })
  eventReactive(input$name2,{
    elo_df <- read.table("elo_df.txt")
    this_result <- data.frame(name1 = competitions$name1[current_ind()],
                              name2 = competitions$name2[current_ind()],
                              outcome = 1,stringsAsFactors = FALSE)
    new_elo <- update_elo(elo_df = elo_df,this_result)
    write.table(new_elo,"elo_df.txt")
    updateSelectInput(session,inputId = "name1",label = competitions$name1[current_ind()+1])
    updateSelectInput(session,inputId = "name2",label = competitions$name2[current_ind()+1])
  })
}

shinyApp(ui, server)