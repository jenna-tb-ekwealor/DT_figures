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
colnames(allplants) <- c("Family", "Genus")
allplants$Kingdom <- rep("Plantae")

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
allplants_DT <- marks %>% dplyr::select(Family, Genus, Kingdom) %>% rbind(., allplants)


resolved_names <- rotl::tnrs_match_names(unique(allplants_DT$Family))
# Warning messages:
# 1: Some names were duplicated: ‘ricciaceae’, ‘hookeriaceae’. 
# 2: pseudoleskeaceae, antitrichiaceae, jocheniaceae, aongstroemiaceae, ruficaulaceae, dicranellopsidaceae, rhizofabroniaceae, obtusifoliaceae, pseudomoerckiaceae are not matched 

in_tree <- rotl::is_in_tree(rotl::ott_id(resolved_names))
family_tree <- tol_induced_subtree(ott_id(resolved_names)[in_tree])

# how many families total?
length(unique(allplants_DT$Family))
# [1] 726

# how many in tree?
length(resolved_names$search_string[in_tree])
# [1] 622

# join original df to rotl df since tip names are ott_id
full_plant_df <- dplyr::left_join(resolved_names, (allplants_DT %>% dplyr::select(Family, Kingdom) %>% unique()), c("unique_name" = "Family")) # %>% na.omit()
full_plant_df$Family <- full_plant_df$unique_name
# custom fill in blanks
# THESE FAMILIES NEED TO BE CHECKED, I DON'T THINK THEY'RE ALL PLANTS. IF THEYRE NOT, THEY NEED TO BE REMOVED
full_plant_df %>% dplyr::mutate(Kingdom = case_when(Family == "Cactaceae"~ "Plantae",
                                                    Family == "Hookeriaceae"~ "Plantae",
                                                    Family == "Tetraphidaceae"~ "Plantae",
                                                    Family == "Plagiochilaceae" ~ "Plantae", 
                                                    Family == "Adoxaceae"~ "Plantae",
                                                    Family == "Amphidiniaceae"~ "Plantae",
                                                    Family == "Chordariaceae" ~ "Plantae",                                                     Family == "Hookeriaceae"~ "Plantae",
                                                    Family == "Chrysoblastella"~ "Plantae",
                                                    Family == "Dicranemaceae" ~ "Plantae",                                                     Family == "Hookeriaceae"~ "Plantae",
                                                    Family == "Ephemeraceae"~ "Plantae",
                                                    Family == "Gigaspermaceae" ~ "Plantae", 
                                                    Family == "Heterocladiaceae"~ "Plantae",
                                                    Family == "Hydnodontaceae"~ "Plantae",
                                                    Family == "Hydropogonaceae" ~ "Plantae",                                                     Family == "Hookeriaceae"~ "Plantae",
                                                    Family == "Hymenomonadaceae"~ "Plantae",
                                                    Family == "Pilotrichaceae" ~ "Plantae",                                                     Family == "Hookeriaceae"~ "Plantae",
                                                    Family == "Pseudoleskeella"~ "Plantae",
                                                    Family == "Sarocladiaceae" ~ "Plantae", 
                                                    .default = Kingdom)) -> full_plant_df

# fix tip labels for plotting data
otl_tips <- strip_ott_ids(family_tree$tip.label, remove_underscores = TRUE)
taxon_map <- structure(full_plant_df$unique_name, names = full_plant_df$unique_name) # can use this map to change the names further if need to
family_tree$tip.label <- taxon_map[ otl_tips ]


# plot tree
# after: manually move label for 0 to shortest bar on y axis
# change values to linear numbers (1, 7, 55) instead of (0, 2, 4)
ggtree(family_tree, layout = "fan", open.angle = 1, size = 0.15, ladderize = T) +
  ggtreeExtra::geom_fruit(data = marks_counts, geom = geom_bar,
                          mapping=aes(y = Family, x = log(count + 1), fill = Kingdom),
                          pwidth=0.38,
                          orientation="y",
                          stat="identity",
                          axis.params=list(
                            axis="x", # add axis text of the layer.
                            text.size = 2,
                            text.angle=0, # the text size of axis.
                            hjust=0,# adjust the horizontal position of text of axis.
                            nbreak = 2
                          ),
                          grid.params=list() # add the grid line of the external bar plot.
  ) + 
scale_fill_manual(name = "Kingdom", 
                    values = c(Animalia = "#FB4D3D", Fungi = "#345995", Plantae = "#03CEA4", Bacteria = "#E5D4ED", Chromista = "#EAC435")) +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm")) -> family_tree_plot

family_tree_plot 


pdf(file = "../output/tree.pdf", height = 4, width = 4.3)
family_tree_plot
dev.off()






