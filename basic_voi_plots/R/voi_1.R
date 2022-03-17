#' voi - value of information
#'
#' This function determines the value of information by contrasting an uncertain scenario with a perfect information scenario.
#' @param data data set on which the calculations will be made (must present a management choice and five alternative scenarios - the number of scenarios can be modified).
#' @param management_choice management choice under evaluation (e.g., stocking density).
#' @param profits_E1  profits based on first alternative scenario.
#' @param profits_E2 profits based on second alternative scenario.
#' @param profits_E3 profits based on third alternative scenario.
#' @param profits_E4 profits based on fourth alternative scenario.
#' @param profits_E5 profits based on fifth alternative scenario. 
#' @param p1 probability of occurrence of the first scenario.
#' @param p2 probability of occurrence of the second scenario.
#' @param p3 probability of occurrence of the third scenario.
#' @param p4 probability of occurrence of the fourth scenario.
#' @param p5 probability of occurrence of the fifth scenario.
#' @return a figure composed of three graphs, a table, and text describing the value of the information.
#' @author Seeking Shelfish Team


voi_1 = function(data, 
               management_choice, 
               profits_E1, profits_E2, profits_E3, profits_E4, profits_E5, 
               p1 = 0.2, p2 = 0.2, p3 = 0.2, p4 = 0.2, p5 = 0.2
) {
  #Error checking
  if (p1+p2+p3+p4+p5 != 1) 
    return("The sum of probabilities must equal 1")
  
  if (c(p1,p2,p3,p4,p5) < 0) 
    return("Probabilities must be positive")
  
  
  
  #Calculations  
  df <- data
  
  df <- rename(df, choice = management_choice) 
  df <- rename(df, prof_E1 = profits_E1) 
  df <- rename(df, prof_E2 = profits_E2) 
  df <- rename(df, prof_E3 = profits_E3) 
  df <- rename(df, prof_E4 = profits_E4) 
  df <- rename(df, prof_E5 = profits_E5) 
  
  df$P1_p1 = df$prof_E1*p1 
  df$P2_p2 = df$prof_E2*p2
  df$P3_p3 = df$prof_E3*p3
  df$P4_p4 = df$prof_E4*p4
  df$P5_p5 = df$prof_E5*p5 
  df$total_P <- rowSums(df[ , c(c(ncol(data)+1):c(ncol(df)))], na.rm=TRUE)
  
  
  c_x <- which(colnames(df)== "choice") #column #: management_choice
  
  
  P1 <- which.max(df$prof_E1) # max profits
  P2 <- which.max(df$prof_E2)
  P3 <- which.max(df$prof_E3)
  P4 <- which.max(df$prof_E4)
  P5 <- which.max(df$prof_E5)
  P_total <- which.max(df$total_P)
  
  
  x_1 <- as.numeric(df[P1, c_x]) #best management choice
  x_2 <- as.numeric(df[P2, c_x])
  x_3 <- as.numeric(df[P3, c_x])
  x_4 <- as.numeric(df[P4, c_x])
  x_5 <- as.numeric(df[P5, c_x])
  x_star <- as.numeric(df[P_total, c_x])
  
  
  x_star_row <- which(grepl(x_star, df$choice)) #row with x_star management choice
  
  
  P_1_x_star <-as.numeric(df[x_star_row, which(colnames(df)== "prof_E1")]) #profits for diff environmental conditions choosing x_star
  P_2_x_star <- as.numeric(df[x_star_row, which(colnames(df)== "prof_E2")])
  P_3_x_star <- as.numeric(df[x_star_row, which(colnames(df)== "prof_E3")])
  P_4_x_star <- as.numeric(df[x_star_row, which(colnames(df)== "prof_E4")])
  P_5_x_star <- as.numeric(df[x_star_row, which(colnames(df)== "prof_E5")])
  
  value_no_info <- P_1_x_star*p1 + P_2_x_star*p2 +P_3_x_star*p3 +P_4_x_star*p4 +P_5_x_star*p5 
  
  value_info <- max(df$prof_E1, na.rm=TRUE)*p1 + max(df$prof_E2, na.rm=TRUE)*p2 +max(df$prof_E3, na.rm=TRUE)*p3 + max(df$prof_E4, na.rm=TRUE)*p4 + max(df$prof_E5, na.rm=TRUE)*p5
  
  voi = value_info - value_no_info
  
  
  #Graph 1
  g1 <- ggplot(data = df) +
    geom_line(aes(x = choice, y = prof_E1))+
    geom_line(aes(x = choice, y = prof_E2))+
    geom_line(aes(x = choice, y = prof_E3))+
    geom_line(aes(x = choice, y = prof_E4))+
    geom_line(aes(x = choice, y = prof_E5))+
    geom_point(aes(x=x_star, y=P_1_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_2_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_3_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_4_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_5_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_1_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_1_x_star, xend = x_star, yend = P_1_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_2_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_2_x_star, xend = x_star, yend = P_2_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_3_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_3_x_star, xend = x_star, yend = P_3_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_4_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_4_x_star, xend = x_star, yend = P_4_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_5_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_5_x_star, xend = x_star, yend = P_5_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    theme_minimal()+
    theme_minimal()+
    geom_text(aes(x = 95,y = 105, label= paste("VoI: $",round(voi,0), sep = ""), hjust = 0), size = 4, color = "black")+
    labs(x = "Stocking Density (ind/m^3)",
         y = "Expected Profits ($)") + 
    xlim(0, 125)+
    ylim(0, 105)+
    theme(axis.title = element_text(size = 8)) +
    theme(plot.title = element_text(size = 8))
  
  
  #Graph 2 
  g2 <- ggplot(data = df) +
    geom_line(aes(x = choice, y = prof_E1))+
    geom_line(aes(x = choice, y = prof_E2))+
    geom_line(aes(x = choice, y = prof_E3))+
    geom_line(aes(x = choice, y = prof_E4))+
    geom_line(aes(x = choice, y = prof_E5))+
    scale_y_continuous(labels = scales::label_number_si(accuracy = 0.1))+
    geom_point(aes(x=x_star, y=P_1_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_2_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_3_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_4_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_point(aes(x=x_star, y=P_5_x_star), color="#841F27", alpha = 0.05, shape = 15)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_1_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_1_x_star, xend = x_star, yend = P_1_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_2_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_2_x_star, xend = x_star, yend = P_2_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_3_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_3_x_star, xend = x_star, yend = P_3_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_4_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_4_x_star, xend = x_star, yend = P_4_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = x_star, y = 0, xend = x_star, yend = P_5_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    geom_segment(aes(x = 0, y = P_5_x_star, xend = x_star, yend = P_5_x_star), color = "#841F27", linetype="dashed", alpha = 0.01)+
    theme_minimal()+
    labs(title = "Uncertain information",
         x = "Stocking Density (ind/m^3)",
         y = "Expected Profits ($)")+ 
    theme(axis.title = element_text(size = 8)) +
    theme(plot.title = element_text(size = 8))
  
  
  
  #Graph 3
  t<- tribble(~env_var, ~no_info, ~perf_info, #dataframe for graph 3
              "1", P_1_x_star, max(df$prof_E1, na.rm=TRUE),
              "2", P_2_x_star, max(df$prof_E2, na.rm=TRUE),
              "3", P_3_x_star, max(df$prof_E3, na.rm=TRUE),
              "4", P_4_x_star, max(df$prof_E4, na.rm=TRUE),
              "5", P_5_x_star, max(df$prof_E5, na.rm=TRUE))
  g3 <- t %>% 
    pivot_longer(-env_var, names_to = "Information", values_to = "perf_info") %>% 
    ggplot(aes(x = env_var, y = perf_info, fill = Information)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    scale_y_continuous(labels = scales::label_number_si(accuracy = 0.1))+
    scale_fill_manual(values=c("#841F27", "#66b2b2"), 
                      name="",
                      breaks=c("no_info", "perf_info"),
                      labels=c("Uncertain information", "Perfect information"))+
    theme_minimal()+
    labs(x = "Scenario",
         y = "Expected Profits ($)")+
    theme(legend.text = element_text(size=7))+
    theme(legend.title = element_text(size=7))+
    theme(legend.key.size = unit(0.3, 'cm'))+
    theme(legend.position="top") + 
    theme(axis.title = element_text(size = 8)) +
    theme(plot.title = element_text(size = 8))
  
  
  
  #Table
  t1 <- tribble(~"Scenario", ~"Uncertain information", ~"Perfect information", ~"Probability",
                "1", P_1_x_star, max(df$prof_E1, na.rm=TRUE), p1,
                "2", P_2_x_star, max(df$prof_E2, na.rm=TRUE), p2,
                "3", P_3_x_star, max(df$prof_E3, na.rm=TRUE), p3,
                "4", P_4_x_star, max(df$prof_E4, na.rm=TRUE), p4,
                "5", P_5_x_star, max(df$prof_E5, na.rm=TRUE), p5,
                "Total Value", value_no_info, value_info, 1) 
  
  #Table format
  table_theme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.55), bg_params = list(fill = 'white', col=NA)),
    colhead = list(fg_params=list(cex = 0.55)),
    rowhead = list(fg_params=list(cex = 0.55)))
  
  
  #Text
  text <- paste("Value of information: $",round(voi,2), sep = "")
  text.p <- ggparagraph(text = text, face = "bold", size = 12, color = "black")
  
  
  #Adding graphs, table, and text together
  f1 <- ((g3 + theme(plot.margin = unit(c(10,30,0,0), "pt"))) +  gridExtra::tableGrob(t1, rows = NULL, theme=table_theme) + plot_layout(widths = c(1, 3)))
  
  #Final figure design
  layout <- '
AAA
AAA
BBB
BBB
CCC
'
  f2 <- (g1|g2) / (f1) /  (text.p + theme(plot.margin = unit(c(30,0,0,0), "pt"))) + plot_layout(design = layout)
  
  
  
  return(g1)
  
}
