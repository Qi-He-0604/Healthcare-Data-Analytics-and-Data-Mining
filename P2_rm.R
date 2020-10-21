# Load packages
library("ggplot2")
library("tidyverse")
library("stringr")
library("readxl")
library("lubridate")

# Set working directory
setwd("~/Desktop/Fall 2019 Academics/HS 256_Healthcare data analytics and Data Mining/HW2")

# Import HEDIS file: E0170 file only
df_hedis <- read_excel("HEDIS2018.xlsx", sheet = "EOC170")

df_hedis_2 <- df_hedis %>% 
  transmute(contract_number = `Contract Number`,
            plan_name = `Plan Name`,
            reported_rate = as.numeric(`EOC170-0010`)) %>% 
  select(contract_number,reported_rate )

# Market share
df_enr_share <- read_excel("enrollment_share.xlsx")
df_enr_share_2 <- df_enr_share %>% 
  filter(!my_organization %in% c("Total", "HHI")) %>% 
  pivot_longer(names_to = "state", values_to = "market_share", SD:PA) %>% 
  mutate(market_share = round(market_share,3)) %>% 
  group_by(state) %>% 
  arrange(desc(market_share)) %>% 
  slice(1:10) %>% 
  ungroup() 

# All insurance companies: data prepared by Henry
df_enr_all <- read_excel("enrollment_use_fill.xlsx")

df_enr_all_2 <- df_enr_all %>% 
  mutate(contract_number = `Contract Number`,
            state = State,
            my_organization = my_organization,
            total_enr = Enrollment,
            county = County) %>% 
  group_by(contract_number,state,my_organization) %>% 
  summarise(total_enr= sum(total_enr))

 
# Remove used datasets
rm(df_enr_all, df_enr_share, df_hedis)

# merge data sets
df_enr_hedis <- df_enr_all_2 %>% 
  inner_join(df_hedis_2, by =c("contract_number")) %>% 
  inner_join(df_enr_share_2, by = c("my_organization"= "my_organization",
                                   "state" ="state")) %>% 
  group_by(state,my_organization) %>% 
  arrange(desc(market_share)) 


# Weighted avg
df_enr_hedis_weighted_avg <- df_enr_hedis %>% 
  group_by(state, my_organization) %>% 
  summarise(weighted_avg_uod = sum(reported_rate*total_enr))

  
# state level
  df_tot_enr_state <- df_enr_hedis %>% 
    group_by(state,my_organization) %>% 
    summarise(tot_enr =sum(total_enr))
 
# Weighted avg by state and organization 
df_weighted_avg <- df_enr_hedis_weighted_avg %>% 
  left_join(df_tot_enr_state, by = c("my_organization"= "my_organization",
                                   "state" ="state")) %>% 
  mutate(weighted_avg = round(weighted_avg_uod/tot_enr,2 )) %>% 
  select(state, my_organization, weighted_avg ) %>% 
  arrange(state, desc(weighted_avg)) %>% 
  left_join(df_enr_share_2, by = c("my_organization"= "my_organization",
                                     "state" ="state"))
# Export to csv
write.csv(df_weighted_avg, "df_weighted_avg.csv")

# Concentrated states charts
df_conc_states <- df_weighted_avg %>% 
  filter(state %in% c ("SD", "SC", "NJ", "MS"))

# Plot 1  
plot_1 <- ggplot(data = df_conc_states, aes(x = reorder(my_organization, weighted_avg), 
                                              y = weighted_avg, fill = weighted_avg
                                            )) + 
  geom_col() + 
  scale_fill_gradient(low = "dark green", high = "red") +
  facet_wrap(~state) +
  coord_flip() +
  ggtitle("Use of Opioids at High Dosage: Top 4 states with highest HHI") +
  ylab("Weighted Average UOD Contract rate per 1000 adults") +
  xlab("Company") +
  geom_text(aes(label = round(weighted_avg,0), hjust=0, vjust=0)) +
  theme(
    # Remove legend
    legend.position = "none",
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) 

