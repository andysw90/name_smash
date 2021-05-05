library(elo)
boys <- readLines("data/top1000_boys.txt")
boys <- boys[1:200]
vote_sim <- function(names_in,n_names,n_sample){
  sampled_names <- sample(names_in,size = n_names,replace = FALSE)
  names1 <- sample(sampled_names,size = n_sample,replace = TRUE)
  names2 <- sample(sampled_names,size = n_sample,replace = TRUE)
  outcome <- round(runif(n_sample))
  result <- data.frame(name1 = names1,
                       name2 = names2,
                       outcome = outcome,stringsAsFactors = FALSE)
  self <- which(result$name1==result$name2)
  if(length(self)>0){
    result <- result[-self,]
  }
  return(result)
}


result <- vote_sim(boys,100,5000)

elo_df <- data.frame(names = unique(c(result$name1,
                                      result$name2)),
                     elo = 1500,stringsAsFactors = FALSE)
for (i in seq_len(nrow(result))) {
  this_result <- result[i, ]
  
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
 
}
elo_df[sort.int(elo_df$elo,index.return = TRUE,decreasing = TRUE)$ix[1:10],]

       