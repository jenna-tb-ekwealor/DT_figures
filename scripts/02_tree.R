library(rgbif)
library(rotl)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("ggtree")
# BiocManager::install("ggtreeExtra")
library(ggtree)
library(ggtreeExtra)

# the next three lines find the script location and set that as the working directory. copy these lines into all scripts for this project. 
library(rstudioapi)
wd_script_location <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd_script_location)


#--- Download all plant names ---#
# Latest Static Version:	World Flora Online Taxonomic Backbone	108MB	v.2023.12	Dec. 22, 2023	(DwCA) Taxonomic classification.	10.5281/zenodo.10425161
# World Flora Online The Plant List https://www.worldfloraonline.org/downloadData
allplants <- read.csv("../data/classification.csv", sep = "\t", na.strings=c("","NA"))
allplants %>% dplyr::select(family, genus) %>% unique() -> allplants
# omit row if NA in family
allplants[!is.na(allplants$family),] -> allplants


#--- Load DT taxa data ---#
# Marks_et.al_Appendix_S1 2021
marks <- read.csv("../data/Marks_et.al_Appendix_S1.csv", header = T)
# fix colnames
colnames(marks) <- as.character(unlist(marks[1,]))
marks = marks[-1, ]
marks$Kingdom <- rep("Plantae")

# count genera per family
marks_counts <- marks %>% dplyr::group_by(Family, Kingdom) %>% dplyr::summarize(count = n())  %>% arrange(desc(count)) %>% distinct() 
# # remove "idae" non-family names
# family_df <- dplyr::filter(family_df, !grepl("idae",family))

# combine marks with all plants





resolved_names <- rotl::tnrs_match_names(unique(allplants_DT$Family))

in_tree <- rotl::is_in_tree(rotl::ott_id(resolved_names))
family_tree <- tol_induced_subtree(ott_id(resolved_names)[in_tree])

# how many families total?
length(unique(allplants_DT$Family))
# [1] 99

# how many in tree?
length(resolved_names$search_string[in_tree])
# [1] 68

# join original df to rotl df since tip names are ott_id
full_plant_df <- dplyr::left_join(resolved_names, (allplants_DT %>% dplyr::select(Family, Kingdom) %>% unique()), c("unique_name" = "Family")) # %>% na.omit()
full_plant_df$Family <- full_plant_df$unique_name
# custom fill in blanks
full_plant_df %>% dplyr::mutate(Kingdom = case_when(Family == "Cactaceae"~ "Plantae",
                                                    Family == "Hookeriaceae"~ "Plantae",
                                                    Family == "Tetraphidaceae"~ "Plantae",
                                                    Family == "Plagiochilaceae" ~ "Plantae", 
                                                    .default = Kingdom)) -> full_plant_df

# fix tip labels for plotting data
otl_tips <- strip_ott_ids(family_tree$tip.label, remove_underscores = TRUE)
taxon_map <- structure(full_plant_df$unique_name, names = full_plant_df$unique_name) # can use this map to change the names further if need to
family_tree$tip.label <- taxon_map[ otl_tips ]


# plot tree
ggtree(family_tree, layout = "fan", open.angle = 1, size = 0.15) +
  ggtreeExtra::geom_fruit(data = marks_counts, geom = geom_bar,
                          mapping=aes(y = Family, x = log10(count+1), fill = Kingdom), # add 1 to count before log so that 0 still shows as a bar
                          pwidth=0.38,
                          orientation="y",
                          stat="identity",
  ) +
scale_fill_manual(name = "Kingdom", 
                    values = c(Animalia = "#FB4D3D", Fungi = "#345995", Plantae = "#03CEA4", Bacteria = "#E5D4ED", Chromista = "#EAC435")) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> family_tree_plot

family_tree_plot 


pdf(file = "../output/tree.pdf", height = 4, width = 4.3)
family_tree_plot
dev.off()






