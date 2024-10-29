library(ggplot2)
library(tidyverse)
library(openxlsx)
library(scales)

# Path to coding sheet
path <- "C:/Users/hwieland/Seafile/Meine Bibliothek/CE-Review/Kopie von 2024_ARER_CE_macroGHG_review_Oct182024_ANALYSIS - Kopie HPW2.xlsx"

# Source quantitative evidence
tmp <- read.xlsx( path, sheet = "2_LitRelevant" )[4:116,206:285] |> as.data.frame()

# Make a long list containing all scenarios
raw_list <- list(tmp[,1:10],
                 tmp[,11:20],
                 tmp[,21:30],
                 tmp[,31:40],
                 tmp[,41:50],
                 tmp[,51:60],
                 tmp[,61:70],
                 tmp[,71:80])

# Set column names
name <- c("Measure_Group",
          "Measure_Detail",
          "GHG_YesNo",
          "Material_YesNo",
          "Energy_YesNo",
          "Sector",
          "Time_Frame",
          "GHG_pot",
          "Material_pot",
          "Energy_pot")

# Make clean list removing entries containing NA's
raw <- lapply(raw_list,
              function(x)
                { 
                  colnames(x) <- name
                  return(x)
                }) %>% 
  bind_rows() %>% 
  filter(!(is.na(GHG_pot) & is.na(Material_pot) & is.na(Energy_pot) | is.na(Measure_Group))) %>% 
  mutate_all(~ na_if(., "n.a."))

# Get unique list of all entries
unique_list <- lapply( apply(raw, c(2), unique),
                       function(x)
                       {
                         x <- data.frame("original_entry" = x,
                                         "new_grouping" = NA) %>% 
                           filter(!is.na(original_entry))
                         return(x)
                       }
                       )

# Write unique list to file and add labes to inform how to aggregate
# write.xlsx(unique_list,"./unique_list.xlsx")

# Read file containing groupings of entries
FileName_NewGroups <- "./unique_list_Adapted.xlsx"
FileName_SectorOrder <- "./sector_order.xlsx"

group <- list( "sec" = read.xlsx(FileName_NewGroups, sheet = "Sector"),
               "measure" = read.xlsx(FileName_NewGroups, sheet = "Measure_Group"),
               "time" = read.xlsx(FileName_NewGroups, sheet = "Time_Frame"))

order <- list("sec" = list("Group_1" = read.xlsx(FileName_SectorOrder, sheet = "new_grouping"),
                           "Group_2" = read.xlsx(FileName_SectorOrder, sheet = "new_grouping_aggregate")
                           )
              )

# Make clean data set with new labels referring to adapted groupings
clean <- raw %>% 
  select(-Measure_Detail, -GHG_YesNo, -Material_YesNo, -Energy_YesNo) %>% 
  pivot_longer(cols = c("GHG_pot", "Material_pot", "Energy_pot"), names_to = "stressor") %>% 
  left_join(group$measure, by = c("Measure_Group" = "original_entry")) %>% 
  left_join(group$sec, by = c("Sector" = "original_entry")) %>% 
  left_join(group$time, by = c("Time_Frame" = "original_entry")) %>% 
  rename("Measure_Group_Category" = "new_grouping.x",
         "Sector_Group_1" = "new_grouping.y",
         "Sector_Group_2" = "new_grouping_aggregate",
         "Time_Frame_Group" = "new_grouping") %>% 
  select(Measure_Group, Measure_Group_Category, Sector, Sector_Group_1,
         Sector_Group_2, Time_Frame, Time_Frame_Group, stressor, value) %>% 
  mutate(stressor = case_when(stressor == "GHG_pot" ~ "GHGs",
                              stressor == "Material_pot" ~ "Materials",
                              stressor == "Energy_pot" ~ "Energy")
         ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(value = as.numeric(value),
         Measure_Group_Category = factor(Measure_Group_Category, levels = c("Narrow", "Slow", "Close", "Auxiliary", "Combined")),
         stressor = factor(stressor, levels = c("Materials", "Energy", "GHGs")),
         Time_Frame_Group = factor(Time_Frame_Group, levels = c("vs base year", "vs BAU", "vs cumulative")),
         Sector_Group_1 = factor(Sector_Group_1, levels = order$sec$Group_1$order),
         Sector_Group_2 = factor(Sector_Group_2, levels = order$sec$Group_2$order))
  

colors <- pal_viridis()(3)



ggplot(clean, aes(y = value, x = Sector_Group_1, fill = Time_Frame_Group)) +
  geom_boxplot(position = "dodge") +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90),
        legend.position = "top",
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5) +
  geom_text(data = clean %>% count(Measure_Group_Category, stressor),
            aes(label = paste("Count:",n), y = Inf, x  = -Inf))





# Create the plot 1
ggplot(clean, aes(y = value, x = Sector_Group_1, fill = Time_Frame_Group)) +
  geom_boxplot(position = "dodge") +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

ggsave("./overiew_1.png",
       plot = last_plot(),  
       width = 8,  
       height = 11,  
       dpi = 1850,
       bg = "white") 

# Create the plot 2
ggplot(clean, aes(y = value, x = Sector_Group_1, color = Time_Frame_Group)) +
  geom_point(size = 3) +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

ggsave("./overiew_2.png",
       plot = last_plot(),  
       width = 8,  
       height = 11,  
       dpi = 1850,
       bg = "white") 


# Create the plot 3
ggplot(clean, aes(y = value, x = Sector_Group_2, fill = Time_Frame_Group)) +
  geom_boxplot(position = "dodge") +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

ggsave("./overiew_3.png",
       plot = last_plot(),  
       width = 8,  
       height = 11,  
       dpi = 1850,
       bg = "white") 

# Create the plot 4
ggplot(clean, aes(y = value, x = Sector_Group_2, color = Time_Frame_Group)) +
  geom_point(size = 3) +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

ggsave("./overiew_4.png",
       plot = last_plot(),  
       width = 8,  
       height = 11,  
       dpi = 1850,
       bg = "white")

df_counts <- clean %>%
  group_by(Measure_Group_Category, stressor, Time_Frame_Group, Sector_Group_2) %>%
  summarise(count = n())

ggplot(df_counts, aes(y = count, x = ))

