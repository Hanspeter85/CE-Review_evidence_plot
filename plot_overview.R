library(ggplot2)
library(tidyverse)
library(openxlsx)
library(scales)
library(cowplot)

# Path to coding sheet
# path <- "C:/Users/hwieland/Seafile/Meine Bibliothek/CE-Review/Kopie von 2024_ARER_CE_macroGHG_review_Oct182024_ANALYSIS - Kopie HPW2.xlsx"
path <- "N:/H73700/data/!projekt/7515_CircEUlar/CE_review_ARER/2024_ARER_CE_macroGHG_review_Oct182024_ANALYSIS_HPW_JS.xlsx"

## 1. Plot global GHGs bar chart
EDGAR <- read.xlsx( "./EDGAR_2024_GHG_booklet_2024.xlsx", sheet = "GHG_by_sector_and_country" ) %>% 
  select(Sector, Country, "2023") %>% 
  rename("value" = "2023") %>% 
  filter(!is.na(value),
         Country == "GLOBAL TOTAL") %>% 
  group_by(Sector) %>% 
  summarise(value = sum(value)/1000) %>% 
  mutate(Region = "Global GWP100\nemissions")

sec_order <- c("Fuel Exploitation", "Power Industry", "Agriculture", "Industrial Combustion", "Processes", "Transport", "Buildings", "Waste")   
EDGAR$Sector <- factor(EDGAR$Sector, levels = rev(sec_order))

colors <- pal_viridis()(nrow(EDGAR))

emissions_plot <- ggplot(EDGAR, aes(x = Region, y = value, fill = Sector)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  labs(y = "Global GHG emissions by sector in 2023 [Giga tons CO2e/yr]",
       x = element_blank()) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50))

## 2. Plot evidence summary

# Get data
tmp <- read.xlsx( path, sheet = "2_LitRelevant" )[4:116,206:335] |> as.data.frame()

# Make a long list containing all scenarios
raw_list <- list(tmp[,1:10],
                 tmp[,11:20],
                 tmp[,21:30],
                 tmp[,31:40],
                 tmp[,41:50],
                 tmp[,51:60],
                 tmp[,61:70],
                 tmp[,71:80],
                 tmp[,81:90],
                 tmp[,91:100],
                 tmp[,101:110],
                 tmp[,111:120],
                 tmp[,121:130])

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
write.xlsx(unique_list,"./unique_list.xlsx")

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
         Measure_Group_Category = case_when(Measure_Group_Category == "Combined(excl. Auxiliary)" ~ "Combined\n(excl. Auxiliary)",
                                            Measure_Group_Category == "Combined(incl. Auxiliary)" ~ "Combined\n(incl. Auxiliary)",
                                            .default = Measure_Group_Category),
         Measure_Group_Category = factor(Measure_Group_Category, 
                                         levels = c("Narrow", "Slow", "Close", "Auxiliary", "Combined\n(excl. Auxiliary)", "Combined\n(incl. Auxiliary)")),
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
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)


# Create the plot 1
plot <- ggplot(clean, aes(y = value, x = Sector_Group_1, fill = Time_Frame_Group)) +
  geom_boxplot(position = "dodge") +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

plot_grid(plot,
          emissions_plot, 
          rel_heights = c(2.2,0.5),
          nrow = 2)

ggsave("./overiew_1.png",
       plot = last_plot(),  
       width = 8,  
       height = 13,  
       dpi = 1850,
       bg = "white") 

# Create the plot 2
plot <- ggplot(clean, aes(y = value, x = Sector_Group_1, color = Time_Frame_Group)) +
  geom_point(size = 3) +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

plot_grid(plot,
          emissions_plot, 
          rel_heights = c(2.2,0.5),
          nrow = 2)


ggsave("./overiew_2.png",
       plot = last_plot(),  
       width = 8,  
       height = 13,  
       dpi = 1850,
       bg = "white") 


# Create the plot 3
plot <- ggplot(clean, aes(y = value, x = Sector_Group_2, fill = Time_Frame_Group)) +
  geom_boxplot(position = "dodge") +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

plot_grid(plot,
          emissions_plot, 
          rel_heights = c(2.2,0.5),
          nrow = 2)

ggsave("./overiew_3.png",
       plot = last_plot(),  
       width = 8,  
       height = 13,  
       dpi = 1850,
       bg = "white") 

# Create the plot 4
plot <- ggplot(clean, aes(y = value, x = Sector_Group_2, color = Time_Frame_Group)) +
  geom_point(size = 3) +
  facet_grid(Measure_Group_Category ~ stressor) +
  theme(axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.2),
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 13, face = "bold"),
        panel.grid.major.x = element_blank()) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = percent) +
  geom_vline(xintercept = seq(0.5, 15, by = 1), color="gray", size=.5, alpha=.5)

plot_grid(plot,
          emissions_plot, 
          rel_heights = c(2.2,0.5),
          nrow = 2)

ggsave("./overiew_4.png",
       plot = last_plot(),  
       width = 8,  
       height = 13,  
       dpi = 1850,
       bg = "white") 

df_counts <- clean %>%
  group_by(Measure_Group_Category, stressor, Time_Frame_Group, Sector_Group_1) %>%
  summarise(count = n())

ggplot(df_counts, aes(y = count, x = Measure_Group_Category, fill = Sector_Group_1)) +
  geom_col()



