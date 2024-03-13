# MTE dissertation
# Jesus Lopez Perez
# Final Panel Summary Statistics

# Setup ---------------------------

rm(list = ls())

path <- "D:/Personal/MTE/Tesis/Propuestas_Temas/credit-markets/"
path2 <- paste("D:/Personal/MTE/Tesis/Propuestas_Temas/",
               "credit-markets/WP/", sep = "")


source(paste(path, "scripts/",
             "utility_functions.R", sep = ""))

cpm <- function(x, q = 1){
  index <- matrix(0, length(x) - q, 2)
  index[,1] <- 1:nrow(index)
  index[,2] <- (q+1):(nrow(index)+q)
  
  xm <- rep(0, nrow(index))
  for(i in 1 : length(xm))
    xm[i] <- x[index[i,2]]/x[index[i,1]]*100-100
  
  return(xm)
}

library(sandwich)
library(lmtest)
library(tidymodels)
library(AER)
library(stargazer)
library(estimatr)
library(summarytools)
library(patchwork)
library(scales)


G7 <- TRUE
if(G7 == TRUE){
  banks <- c("040002", "040012", "040014", "040021",
             "040072", "040044", "040036")
  which_db <- "G7"
}else{
  banks = NULL
  which_db <- "ALL"
}

# sig level
alpha <- 0.05

# Load Panel data ---------------------------
panel_data_r <- read_csv(paste(path, "/proc/panel_firms_G7small.csv", sep = ""),
                         locale = readr::locale(encoding = "UTF-8"))

panel_data_r <- panel_data_r %>% 
  mutate(institution_code = str_pad(institution_code,width = 6, pad = "0", side = "left")
         ) %>% 
  mutate(period = period %>% lubridate::ymd()) 


# # Load IV data ---------------------
# iv_data <- read_csv(paste(path,"/proc/iv_",
#                          which_db,"_2T_16_4T_21.csv", sep = ""), 
#                     locale = readr::locale(encoding = "UTF-8"))

# create function for summary statistics table ----------
my_summary <- function(data, var){
summarise(data, 
          N = sum(!is.na(.data[[var]])),
          Mean = mean(.data[[var]], na.rm = TRUE),
          "Std Dev" = sd(.data[[var]],na.rm = TRUE),
          "P10" = quantile(.data[[var]], .1, na.rm = TRUE),
          "P25" = quantile(.data[[var]], .25, na.rm = TRUE),
          "P50" = quantile(.data[[var]], .5, na.rm = TRUE),
          "P75" = quantile(.data[[var]], .75, na.rm = TRUE),
          "P90" = quantile(.data[[var]], .9, na.rm = TRUE))
}

panel_data_r <- panel_data_r %>% 
  mutate(mean_disposition_th = mean_disposition/1000)


# Summary table  ----------------------
data_summary <- panel_data_r 


# first pane
vars_ <- c("mean_disposition_th", 
           "wa_interest_rate_d",
           "wa_term_d", 
           "inflation") 
vars_public <- c("Loan amount (Th. pesos)", "Interest rate", 
                 "Term","Inflation")
panel_data_table <- data_summary[,vars_]

table1_A <- map(vars_, 
                function(x) my_summary(panel_data_table,x)) %>% 
  do.call(rbind, .) %>% 
  mutate("Variable" = vars_public) %>% 
  select(Variable, everything())


knitr::kable(
  rbind(table1_A), 
  digits = 1,
  format.args = list(big.mark = ",",   scientific = FALSE),
  format = "latex")


# plot interest rate by sector
g1 <- panel_data_r %>% 
  ggplot(aes(x = wa_interest_rate_d, y = scian_code)) +
  geom_boxplot(aes(group = scian_code)) +
  labs(y = "NAICS code",
       x = "Interest rate (%)") +
  ggtitle("Interest rate by sector") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot interest rate and inflation
g2 <- panel_data_r %>% group_by(period) %>% 
  summarise(mean_i = mean(wa_interest_rate_d, na.rm = T),
            mean_infl = mean(inflation, na.rm = T)) %>% 
  ggplot(aes(x = period, y = mean_i)) +
  geom_line(aes(color = "Interest rate")) + 
  geom_line(aes(x = period, y = mean_infl, color = "Inflation")) +
  xlab("") + 
  labs(x = "Period",
       y = "(%)",
       color = "")  +
  ggtitle("Interest rate to SME") +
  theme(legend.position="top") 
  


g1+g2
ggsave(paste(path2, "Interest_rates.png", sep = ""), device = "png",
       width = 17, height = 9, units = "cm",dpi = 1500)


##  plot demand


delta <- seq(-20,50, by = 1)
i_sc1 <- mean(panel_data_r$wa_interest_rate_d) * (1-delta/100); i_sc1
elas <- -1.435
disp_sc1 <- mean(panel_data_r$mean_disposition_th) *(1-elas*delta/100); disp_sc1

counterfactual <- c(
  mean(panel_data_r$mean_disposition_th) * (1-elas*35/100),
  mean(panel_data_r$wa_interest_rate_d)  * (1-35/100) 
  )

mean_values <- c(
  mean(panel_data_r$mean_disposition_th),
  mean(panel_data_r$wa_interest_rate_d)  
)

table_points <- data.frame(t(cbind(counterfactual, mean_values,
                                   min = c(min(disp_sc1),min(i_sc1)),
                                   max = c(max(disp_sc1),max(i_sc1))
                                   )))
colnames(table_points) <- c("loan_amount", "interest_rate")

data_ggplot <- data.frame(cbind(interest_rate = i_sc1,
                     loan_amount = disp_sc1))
plot1 <-
 ggplot() + 
  geom_line(data = data_ggplot, 
            aes(x = loan_amount, y = interest_rate)) + 
  geom_point(aes(x = table_points$loan_amount[1:2], 
                 y = table_points$interest_rate[1:2]), 
             colour = c("red","blue") ) +
  geom_segment(aes(x = table_points["min","loan_amount"], # add horiz line
                    xend = table_points["counterfactual","loan_amount"],
                   y = table_points["counterfactual","interest_rate"], 
                   yend = table_points["counterfactual","interest_rate"]),
               colour = "red", alpha = 0.5, linetype = 2) + 
  geom_segment(aes(x = table_points["counterfactual","loan_amount"], # add vert line
                   xend = table_points["counterfactual","loan_amount"],
                   y = table_points["min","interest_rate"], 
                   yend = table_points["counterfactual","interest_rate"]),
               colour = "red", alpha = 0.5, linetype = 2) +
  geom_segment(aes(x = table_points["min","loan_amount"], # add horiz line
                   xend = table_points["mean_values","loan_amount"],
                   y = table_points["mean_values","interest_rate"], 
                   yend = table_points["mean_values","interest_rate"]),
               colour = "blue", alpha = 0.5, linetype = 2) + 
  geom_segment(aes(x = table_points["mean_values","loan_amount"], # add vert line
                   xend = table_points["mean_values","loan_amount"],
                   y = table_points["min","interest_rate"], 
                   yend = table_points["mean_values","interest_rate"]),
               colour = "blue", alpha = 0.5, linetype = 2) +
  annotate("text", x=table_points["mean_values","loan_amount"]+100, 
                   y=table_points["mean_values","interest_rate"]+1, 
           label= paste("Mean values"), colour = "blue") +
annotate("text", x=table_points["counterfactual","loan_amount"]+100, 
         y=table_points["counterfactual","interest_rate"]+1, 
         label= paste("Bill scenario"), colour = "red") +
  scale_y_continuous(
    name = "Interest rate (%)",
  ) + 
  scale_x_continuous(
    name = "Loan Amount (MXN)", labels = comma
  ) +
  theme(text = element_text(size = 16))

ggsave(paste(path2, "Felipe Brugues/","Demand.png", sep = ""),
       device = "png", width = 12, height = 9, units = "cm", dpi = 500)  


  








