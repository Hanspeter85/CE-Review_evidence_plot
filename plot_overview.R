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

group <- list( "sec" = read.xlsx(FileName_NewGroups, sheet = "Sector"),
               "measure" = read.xlsx(FileName_NewGroups, sheet = "Measure_Group"),
               "time" = read.xlsx(FileName_NewGroups, sheet = "Time_Frame"))

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
  mutate(value = as.numeric(value),
         Measure_Group_Category = factor(Measure_Group_Category, levels = c("Narrow", "Slow", "Close", "Auxiliary", "Combined")),
         stressor = factor(stressor, levels = c("Materials", "Energy", "GHGs"))) %>% 
  filter(complete.cases(.))

colors <- pal_viridis()(3)

unique(clean$Sector_Group_1)

ggplot(clean, aes(y = value, x = Sector_Group_1, fill = Time_Frame_Group)) +
  geom_boxplot(position = "dodge") +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90),
        legend.position = "none",
        axis.title = element_blank(),
        strip.text = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent)


