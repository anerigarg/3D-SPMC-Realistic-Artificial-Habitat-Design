## Frontiers Fig 5 code

# packages and palletes ---------------------------------------------------


library(tidyverse)
library(ggplot2)
library(openxlsx)

`%notin%` = negate(`%in%`)


# data frame joining - from original data ---------------------------------
library(readr)
library(tidyverse)

Art_Recruit_fish_data <- read_csv("data/data frames to join/Art Recruit fish data.csv")
head(Art_Recruit_fish_data)

ARD <- Art_Recruit_fish_data
test <- ARD
#ARD <- test

###---- join ARD to species list ----###

ArtR_species_list <- read_csv("data/data frames to join/ArtR species list.csv")
#change col name CommonName to common_name (to join to ARD)
ARD_spp_list <- ArtR_species_list %>% 
  rename(common_name = CommonName)
head(ARD_spp_list)
#change col name species_ID to common_name 
ARD <- ARD %>% 
  rename(common_name = species_ID)
head(ARD)

#delete unecessary collumns BehaviouralGroup, MedianPopPblTime, L-W parameter source, and L-W region
ARD_spp_list[,c('BehaviouralGroup','MedianPopDblTime', 'L-W parameter source', 'L-W region')] <- list(NULL)

#using the package data.table I will now join ARD and ARD_spp_list and name the new df ARD2. ARD2 should have the same number of observations as ARD with more variables 
library(data.table)
setDT(ARD)
setDT(ARD_spp_list)
setkey(ARD, common_name)
setkey(ARD_spp_list, common_name)
ARD2 = ARD_spp_list[ARD]

head(ARD2)

###---- join ARD2 to survey info ----###

#import ArtR_survey_info, link by by survey_number
ArtR_survey_info <- read_csv("data/data frames to join/ArtR survey info.csv")

setDT(ARD2)
setDT(ArtR_survey_info)
setkey(ARD2, survey_number)
setkey(ArtR_survey_info, survey_number)
ARD3 = ArtR_survey_info[ARD2]

head(ARD3)

###---- join ARD3 to plot info ----###

#import ArtR_plot_info, link by plot
ArtR_plot_info <- read_csv("data/data frames to join/ArtR plot info.csv")

setDT(ARD3)
setDT(ArtR_plot_info)
setkey(ARD3, plot)
setkey(ArtR_plot_info, plot)
ARD4 = ArtR_plot_info[ARD3]

head(ARD4)

###---- join ARD4 to transect info ----###

# import ArtR transect info
ArtR_transect_info <- read_csv("data/data frames to join/ArtR transect info.csv")

#need to make a new variable in ARD4 called plot_grid that makes a unique identifier by combining plot and grid_number ( do the same in ArtR_transect_info)

ARD4$plot_grid <- paste(ARD4$plot, "-", ARD4$grid_number)
ArtR_transect_info$plot_grid <- paste(ArtR_transect_info$plot, "-", ArtR_transect_info$grid_numbers)
#remmeber to delete row grid_numbers in ArtR_transect_info after

setDT(ARD4)
setDT(ArtR_transect_info)
setkey(ARD4, plot_grid)
setkey(ArtR_transect_info, plot_grid)
ARD5 = ArtR_transect_info[ARD4]

ARD5[,c('grid_numbers')] <- list(NULL)

head(ARD5)

#get rid of dupilcate collumns i.plot and i.transect_ID
#rename collumn "yes" to "transit"

ARD5[,c('i.plot','i.transect_ID')] <- list(NULL)

ARD5 <- ARD5 %>% 
  rename('transit' = 'yes')

names(ARD5)

#check how each var is categorized
sapply(ARD5, class)

#change:
ARD5$treatment = as.factor(ARD5$treatment) #treatment to factor
ARD5$complexity = as.factor(ARD5$complexity) #complexity as factor
ARD5$life_phase = as.factor(ARD5$life_phase )#life_phase to factor
ARD5$transit = as.factor(ARD5$transit) #transit to factor
ARD5$genotype = as.factor(ARD5$genotype) #genotype to factor

sapply(ARD5, class)
#check

#ARD5 created Feb 6, 2020 - most recently joined df

# data cleaning -----------------------------------------------------------

#using ARD5, which has all data frames connected
#separate the baseline from recruit data to just use recruit data (after outplanting on)

ARD_baseline <- ARD5 %>% 
  filter(survey_type == "baseline recruit")

ARD_post_outplant <- ARD5 %>% 
  filter(survey_type == "recruit")

#went through raw data and removed schooling/shoaling fish (not closely associated with habitat as recruits)
# rules (over 10 fish in a group)

#removed x50 bhw from survey 227, July 25, HS-13, control --> roaming shoal of fish (not associated with habitat)
#removed x27 bhw and x 16 spf from survey 238, July 28, HS-6, 50/50 --> roaming shoal of herbs
#removed x16bhw and x26 masked gobies from survey 118, June 20, HS-5, control --> roaming shoal of bhw and gobies floating in water col not associated w habitat
#removed x40 bhw from survey 92, June 13, LS, orange --> roaming shoal (transit yes)
#removed x19 bhw from survey 212, Jul 20, LS, orange --> roaming
#removed x45 bhw from survey 183, Jul 5, HN, control --> roaming (transit yes)
#removed x27 bhw from survey 218 Jul 20, HN, Ls-12, green --> roaming
#removed x30bhw from suvey 71, June 9, LS-11, orange --> roaming
#removed x20 bhw from survey 106, June 15, HS-yellow --> roaming
#removed x34 bhw from survey 133, June 25, HN-3, orange --> roaming
#removed duplicates entered of survey 180 (grid entered twice, check whole survey)
#removed x14 clown w from survey 44, June 6, LS-10, green --> shoaling
#removed x14 bhw from survey 58, June 7, HS-6, yellow --> shoaling
#removed x12 bhw from survey 127, June 25, LS-3, yellow --. shoaling
#remoed x8 bhw from survey 153, Jul 2, LS-20, orange - shoaling
#removed x25 bhw from survey 5, Jun 2, LS-11, orange --> shoaling
#removed x20 bhw from survey 19, June 3, HN-1, orange --> shoaling (transit yes)
#removed x20 bhw from survey 73, June 9, HS-1, yellow -> shoaling
#removed x24 bhw from survey 4, June 2, LS-3, yellow --> shoaling
#removed x15 masked goby from survey 55, June 7, LS-2, control --> in water col not associated w habitat
#removed x16 bhw from survey 5, June 2, LS-10, green --> shoaling
#removed x51 bhw from survey 92, Jun 13, LS-16, yellow --> shoaling
#removed x20 bhw from survey 116, LS-14, yellow --> shoaling
#removed x12 bhw from survey 26, LS-14, yellow --> shoaling
#removed x28 bhw from survey 146, HN-16, blue --> shoaling
#removed x18 bhw from survey 63, LN-22, green --> shoaling
#removed x26 bhw from survey 240, HS-19, blue --> shoaling
#removed x19 bhw from survey 132, HS-20, pink or red --> shoaling
#changes made in xsxl file filtering_outliers_x3 (from ARD_final_no2)
#removed x39 bhw from survey 140, LS-16, yellow --> shoaling
#removed x29 bhw from survey 159, HN-18, red or pink --> shoaling
#removed x25 bhw from survey 85, HN-1, orange --> shoaling
#removed x23 bhw from survey 144, HS-19, blue --> shoaling


#survey 183, Jul 5 Taylor, grid 17 should be 45 bhw, data was entered twice -> corrected in Art_Recruit_fish_data df
#it's also a school in transit so change to 'yes" in transit -> converted
#survey 183, Jul 5 Taylor, grid 18 , survey also entered in twice ->  checked the entire survey in Art_Recruit_fish_data df
#convert bhw to yes in transit -> converted

#survey 227, Jul 25 Taylor, grid 13, he wrote down 50 bhw, entered correctly bu im skeptical
#convert bhw to yes in transit-> converted

#survey 238, Jul 28 Noelle, grid 6, looks fine
#convert bhw to yes in transit -> converted

#survey 92, Jun 13 Kelsey, grid 16 looks fine 
#convert bhw to yes in transit -> converted


#the cleanest version of data now: 
ARD_post_out_plant_new <- read_csv("data/clean data frames/ARD_post_out_plant_new.csv")
unique(ARD_post_out_plant_new$plot)
unique(ARD_post_out_plant_new$treatment)



# get 0 values - 0-3cm all fish ------------------------------------------------------

ARD_post_out_plant_new <- read_csv("data/clean data frames (after outlier removal)/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new %>% 
  filter(TL <= 6)

unique(ARD$date)

# make new col called size class, bin by TL (0-3) and (4-6)
is.numeric(ARD$TL)
ARD$size.class <- "0-3"
ARD$size.class[ARD$TL >= 4 & ARD$TL <= 6] <- "4-6"

ARD_3 <- ARD %>% 
  filter(size.class == "0-3")

# group by date instead, that way you can do the cut-off to get rid of the first day and get rid of the day where the boat was driving over us
ARD_3_abun2 <- ARD_3 %>% 
  dplyr:: group_by(plot, grid_number, date) %>%  #add size.class and species?
  dplyr:: summarise(abundance = n()) %>% 
  dplyr:: ungroup() %>% 
  complete(nesting(plot, grid_number), date, fill = list(abundance = 0))

# combine plot_grid: 

ARD_3_abun2$plot_grid <- paste(ARD_3_abun2$plot, "-", ARD_3_abun2$grid_number)
ARD_lookup <- read_csv("data/ARD_lookup.csv") # add treatment and complexity:
ARD_3_abun3 <- full_join(ARD_3_abun2, ARD_lookup)

# add days_since_outplanting: (starts all from day 1 on June 5)

ArtR_days_since_lookup <- read_csv("data/add days since outplanting/ArtR_days_since_lookup.csv")

#1) make a col in each df that is common (date-plot)

ARD_3_abun3$plot_date <- paste(ARD_3_abun3$plot, "-", ARD_3_abun3$date)
ArtR_days_since_lookup$plot_date <- paste(ArtR_days_since_lookup$plot, "-", ArtR_days_since_lookup$date)

#2) delete date and plot cols from the lookup table

ArtR_days_since_lookup <- ArtR_days_since_lookup %>% 
  select( -date) %>% 
  select(-plot)

#3) join them by plot-date

ARD_3_abun4 <- inner_join(ARD_3_abun3, ArtR_days_since_lookup)
hist(ARD_3_abun4$abundance) # well that's a poisson/neg binom <- consider making a cut off at 20 to reduce outliers
range(ARD_3_abun4$abundance)

# remember that day 4 was when that boat was driving over us all day --> exclude from analysis
ARD_3_abun5 <- ARD_3_abun4 %>% 
  filter(days_since_outplanting != 4)

### USE THIS DF FROM NOW ON ###
write.csv(ARD_3_abun5, "ARD_3.csv")

unique(ARD_3_abun5$plot)
hist(ARD_3_abun5$abundance)
unique(ARD_3_abun5$days_since_outplanting)

#quick visuals for fun

ARD_3_sum <- ARD_3_abun5 %>% 
  na.omit() %>% 
  group_by(treatment, complexity) %>% 
  summarize(mean.ab = mean(abundance))

library(ggplot2)

ggplot(data = ARD_3_sum) +
  geom_point(aes(x = treatment,
                 y = mean.ab)) +
  facet_grid(.~complexity)



# calculate recruitment rate ####
#1 ) import ARD_3 (the one with 0 values) 
ARD_3 <- read_csv("data/filter 0 values/ARD_3.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

unique(ARD_3$days_since_outplanting)
unique(ARD_3$treatment)

ARD_dr <- ARD_3 %>%
  # filter(days_since_outplanting != 1) %>% 
  dplyr::select(days_since_outplanting, plot_grid,  abundance) %>% 
  dplyr::arrange(plot_grid, days_since_outplanting) #kind of like sort in excel

ARD_dr_1 <- ARD_3 %>% #may not even need this tbh since the rate on day 1 for all is gong to be 0 
  filter(days_since_outplanting == 1) %>% 
  dplyr::select(days_since_outplanting, plot_grid,  abundance) %>% 
  dplyr::arrange(plot_grid, days_since_outplanting) %>%  #kind of like sort in excel
  mutate(rate = 0)

#export ARD_dr to calculate rate 

write.xlsx(ARD_dr, "ARD_dr.xlsx")
# =ABS(0) in first cell
# formula: =(C3-C2)/(A3-A2)

# Import the new sheet with daily rate of recruitment data (rate)
ARD_rate <- read_csv("data/rate calculations/n/ARD_rate.csv")

#join ARD_d_convert to ARD_lookup to get treatment and complexity information 
# (ARD_lookup has info on which treatment/complexity pairs with each plot_grid)
ARD_lookup <- read_csv("data/ARD_lookup.csv")

ARD_3_rate <- full_join(ARD_rate, ARD_lookup) %>% 
  mutate(days_since_outplanting = as.numeric(days_since_outplanting)) %>% 
  mutate(plot_grid1 = plot_grid) %>% 
  separate(plot_grid1, c("plot", "grid")) %>% 
  dplyr::select(-grid)

write.csv(ARD_3_rate,"ARD_3_rate.csv")
#1536 entries, checks out (24*4*16)

# visualizations ----------------------------------------------------------



# plot of mean density per treatment and plot of mean density over time per treatment


# Adding the time plot -> revisions for frontiers Jan 27, 2022
# made the working directory: U of A --> habitat seleciton paper --> Habitat Selection Study --> data

ARD_3_rate <- read_csv("rate calculations/ARD_3_rate.csv")

ARD_dr_rate_3 <- ARD_3_rate %>% 
  filter(treatment %in% c("0%", "50%", "100%"))

ARD_dr_rate_sum_col_3 <- ARD_dr_rate_3 %>% 
  na.omit() %>% 
  mutate(treatment = factor(treatment, levels = c("0%","50%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"),
                             labels = c("Low Complexity", "High Complexity"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(rate), density.sd = sd(rate)) %>%
  mutate(density.se = density.sd/sqrt(768))

#column plot it

ggplot(data = ARD_dr_rate_sum_col_3) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("firebrick", "darkorchid4", "royalblue3"), name = "% living coral") +
  labs(x = expression(Percent~Living~Coral),
       y = expression(Recruitment~rate~(fish~m^{2}~day^{1}))) +
  # ggtitle("Mean Daily Recruitment Rate 3cm")+
  # ylim(-0.75,1.25) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    # axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )


# visualize over time (first 10 visits recruitment curves)# density over time: (0-3) (all visits)

ARD_dr_rate_3 <- ARD_3_rate %>% 
  filter(treatment %in% c("0%", "50%", "100%"))

ARD_dr_rate_3_sum <- ARD_dr_rate_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%","50%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"),
                             labels = c("Low Complexity", "High Complexity"))) %>% 
  group_by(treatment, complexity, days_since_outplanting) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(768))

ggplot() +
  # geom_point(data = ARD_dr_rate_3,
  #            aes(x = days_since_outplanting,
  #                y = rate,
  #                colour = treatment),
  # alpha = 0.3) +
  geom_point(data = ARD_dr_rate_3_sum,
             aes(x = days_since_outplanting,
                 y = rate.mean,
                 group = treatment,
                 colour = treatment),
             size = 2.5,
             alpha = 0.5) +
  geom_smooth(data = ARD_dr_rate_3_sum,
              aes(x = days_since_outplanting,
                  y = rate.mean,
                  group = treatment,
                  colour = treatment),
              method = loess, 
              se = FALSE,
              size = 2) +
  facet_grid(.~complexity) +
  geom_line(data = ARD_dr_rate_3_sum,
            aes(x = days_since_outplanting,
                y = rate.mean,
                group = treatment,
                colour = treatment),
            size = 1,
            alpha = 0.5)+
  geom_errorbar(data = ARD_dr_rate_3_sum,
                aes(x = days_since_outplanting,
                    ymin = rate.mean-rate.se,
                    ymax = rate.mean-rate.se,
                    colour = treatment),
                width = 0.15) +
  scale_colour_manual(values = c("firebrick", "darkorchid4", "royalblue3"), name = "% living coral") +
  labs(x = expression(Days~since~outplanting),
       y = expression(Recruitment~rate~(~fish~m^{2}~d^{1}))) +
  theme_classic()+
  theme(axis.text =element_text(size = 15),
        axis.title = element_text(size = 18),
        strip.text= element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))
# xlim(0,10)
# ylim(-5,5)
