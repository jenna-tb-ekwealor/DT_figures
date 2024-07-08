library(rgbif)
library(rotl)

# the next three lines find the script location and set that as the working directory. copy these lines into all scripts for this project. 
library(rstudioapi)
wd_script_location <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd_script_location)

#--- Load DT taxa data ---#
# Marks_et.al_Appendix_S1 2021
marks <- read.csv("../data/Marks_et.al_Appendix_S1.csv", header = T)
# fix colnames
colnames(marks) <- as.character(unlist(marks[1,]))
marks = marks[-1, ]

resolved_names <- rotl::tnrs_match_names(unique(marks$Family))

in_tree <- rotl::is_in_tree(rotl::ott_id(resolved_names))
family_tree <- tol_induced_subtree(ott_id(resolved_names)[in_tree])

# how many families total?
length(unique(marks$Family))
# [1] 99

# how many in tree?
length(resolved_names$search_string[in_tree])
# [1] 68

# join original df to rotl df since tip names are ott_id
full_df <- dplyr::left_join(resolved_names, marks, c("unique_name" = "Family")) # %>% na.omit()
