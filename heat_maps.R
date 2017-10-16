# Heat Maps
# Evan Kramer
# 10/13/2017

library(tidyverse)
library(lubridate)
library(haven)
library(readxl)
library(stringr)

setwd("K:/ORP_accountability/")
date = str_replace_all(as.character(today()), "-", "")
cur_yr = year(today())
lag_yr = cur_yr - 1

# Master crosswalks
a = read_csv("projects/2017_district_accountability/final files/min_performance_goal_mb.csv")

b = data.frame(system = rep(c(unique(a$system)), 5 * 8), 
               subject = rep(c(rep("3-5 ELA", length(unique(a$system))), rep("3-5 Math", length(unique(a$system))),
                               rep("6-8 ELA", length(unique(a$system))), rep("6-8 Math", length(unique(a$system))),
                               rep("HS ELA", length(unique(a$system))), rep("HS Math", length(unique(a$system))),
                               rep("ACT", length(unique(a$system))), rep("Graduation Rate", length(unique(a$system)))), 5),
               subgroup = rep(c(rep("All Students", length(unique(a$system))), rep("Black/Hispanic/Native American", length(unique(a$system))),
                                rep("Economically Disadvantaged", length(unique(a$system))), rep("English Language Learners with T1/T2", length(unique(a$system))), 
                            rep("Students with Disabilities", length(unique(a$system)))), 8)) %>% 
    arrange(system, subject, subgroup)

c = b %>% 
    group_by(system, subject) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    select(-n)

# Participation Rates
part_rates = read_csv("projects/2017_district_accountability/final files/min_performance_goal_mb.csv") %>% 
    filter(!is.na(subgroup)) %>% 
    mutate(subject = ifelse(subject == "Math" & grade == "3rd through 5th", "3-5 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "3rd through 5th", "3-5 ELA", subject),
           subject = ifelse(subject == "Math" & grade == "6th through 8th", "6-8 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "6th through 8th", "6-8 ELA", subject),
           sort_priority = ifelse(subject == "3-5 ELA", 1, Inf),
           sort_priority = ifelse(subject == "3-5 Math", 2, sort_priority),
           sort_priority = ifelse(subject == "6-8 ELA", 3, sort_priority),
           sort_priority = ifelse(subject == "6-8 Math", 4, sort_priority),
           sort_priority = ifelse(subject == "HS ELA", 5, sort_priority),
           sort_priority = ifelse(subject == "HS Math", 6, sort_priority),
           sort_priority = ifelse(subject == "ACT", 7, sort_priority),
           sort_priority = ifelse(subject == "Graduation Rate", 8, sort_priority)) %>% 
    select(system, subject, subgroup, participation_pathway, sort_priority) %>% 
    full_join(b, by = c("system", "subject", "subgroup")) %>% 
    arrange(system, subgroup, sort_priority)

write_csv(part_rates, "projects/2017_district_accountability/part_rates_EK.csv", na = "")

### MAKE SURE SUBJECTS, GRADES, AND SUBGROUPS ARE CONSISTENT WHEN MERGING

# Minimum Performance Goal
min_goal = read_csv("projects/2017_district_accountability/final files/min_performance_goal_mb.csv") %>% 
    filter(is.na(subgroup)) %>% 
    mutate(subject = ifelse(subject == "High School English", "HS ELA", subject),
           subject = ifelse(subject == "High School Math", "HS Math", subject),
           subject = ifelse(subject == "Math" & grade == "3rd through 5th", "3-5 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "3rd through 5th", "3-5 ELA", subject),
           subject = ifelse(subject == "Math" & grade == "6th through 8th", "6-8 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "6th through 8th", "6-8 ELA", subject)) %>% 
    full_join(c, by = c("system", "subject")) %>% 
    mutate(sort_priority = ifelse(subject == "3-5 ELA", 1, Inf),
           sort_priority = ifelse(subject == "3-5 Math", 2, sort_priority),
           sort_priority = ifelse(subject == "6-8 ELA", 3, sort_priority),
           sort_priority = ifelse(subject == "6-8 Math", 4, sort_priority),
           sort_priority = ifelse(subject == "HS ELA", 5, sort_priority),
           sort_priority = ifelse(subject == "HS Math", 6, sort_priority),
           sort_priority = ifelse(subject == "ACT", 7, sort_priority),
           sort_priority = ifelse(subject == "Graduation Rate", 8, sort_priority)) %>% 
    arrange(system, sort_priority)

write_csv(min_goal, "projects/2017_district_accountability/mpg_EK.csv", na = "")

# Achievement
ach = read_csv("projects/2017_district_accountability/final files/ach_status_mb.csv") %>% 
    mutate(subject = ifelse(subject == "Math" & grade == "3rd through 5th", "3-5 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "3rd through 5th", "3-5 ELA", subject),
           subject = ifelse(subject == "Math" & grade == "6th through 8th", "6-8 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "6th through 8th", "6-8 ELA", subject),
           sort_priority = ifelse(subject == "3-5 ELA", 1, Inf),
           sort_priority = ifelse(subject == "3-5 Math", 2, sort_priority),
           sort_priority = ifelse(subject == "6-8 ELA", 3, sort_priority),
           sort_priority = ifelse(subject == "6-8 Math", 4, sort_priority),
           sort_priority = ifelse(subject == "HS ELA", 5, sort_priority),
           sort_priority = ifelse(subject == "HS Math", 6, sort_priority),
           sort_priority = ifelse(subject == "ACT", 7, sort_priority),
           sort_priority = ifelse(subject == "Graduation Rate", 8, sort_priority)) %>% 
    full_join(c, by = c("system", "subject")) %>% 
    arrange(system, sort_priority)

write_csv(ach, "projects/2017_district_accountability/ach_EK.csv", na = "")
    
# Subgroup
gap = read_csv("projects/2017_district_accountability/final files/gap_status_mb.csv") %>% 
    mutate(subject = ifelse(subject == "Math" & grade == "3rd through 5th", "3-5 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "3rd through 5th", "3-5 ELA", subject),
           subject = ifelse(subject == "Math" & grade == "6th through 8th", "6-8 Math", subject),
           subject = ifelse(subject == "ELA" & grade == "6th through 8th", "6-8 ELA", subject),
           sort_priority = ifelse(subject == "3-5 ELA", 1, Inf),
           sort_priority = ifelse(subject == "3-5 Math", 2, sort_priority),
           sort_priority = ifelse(subject == "6-8 ELA", 3, sort_priority),
           sort_priority = ifelse(subject == "6-8 Math", 4, sort_priority),
           sort_priority = ifelse(subject == "HS ELA", 5, sort_priority),
           sort_priority = ifelse(subject == "HS Math", 6, sort_priority),
           sort_priority = ifelse(subject == "ACT", 7, sort_priority),
           sort_priority = ifelse(subject == "Graduation Rate", 8, sort_priority)) %>% 
    full_join(filter(b, subgroup != "All Students"), by = c("system", "subject", "subgroup")) %>% 
    arrange(system, subgroup, sort_priority)

write_csv(gap, "projects/2017_district_accountability/gap_EK.csv", na = "")

