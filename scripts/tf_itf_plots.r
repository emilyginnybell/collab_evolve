#   tf_itf_plots.r
#===========================================================================

#	Pre-processes text
#	Generates constructs, calculates Cronbach's alpha
#	Runs ti-itf
#	Plots DTM of tf-itf
#	Plots percent change by region over time
#	Plots descriptive of # BMPs by region over time
#	Plots appendix graphics of random vs. chosen constructs

#===========================================================================

library(dplyr)
library(tidyverse)
library(cleanNLP) # R wrapper for Python package
library(SnowballC)
library(reshape)
library(ggplot2)
library(data.table)
library(reticulate)
library(spacyr)
library(tidyr)
library(ggpubr)
library(viridis)
library(gridExtra)
library(grid)
library(gtable)
library(lemon)

#===========================================================================
#	Call on NLP package, specify function for cleaning text data
#===========================================================================

# library(rJava) # Don't need this for Macs

# Before this, must run the following on command line in terminal in Mac/Linux
# 1. sudo pip install --ignore-installed spacy
# 2. sudo python -m spacy download en

#   In Windows, you'll need to install the C++ requirements for spacy. 
#   See https://spacy.io/usage#source-windows

# Need to open a command prompt (cmd) as Administrator and enter
# 1. pip install --ignore-installed spacy
# 2. python -m spacy download en

# Need to run the next line to install spaCy in a conda environment
# This will install the latest version of spaCy and will download 
# the English language model.

# spacy_install()

# Next, you want to initialize spaCy in R:

# spacy_initialize()


cnlp_init_spacy("en_core_web_sm")


tidify <- function(text) {

# Remove punctuation

  text <- gsub("[[:punct:]]+", " ", text)
  
# Remove sequences of digits (including years)
# Removes '2009' but not '10th'

  text <- gsub("(?<![[:alnum:]])\\d+(?![[:alnum:]])", "", text, ignore.case=TRUE, perl=TRUE)
  
# Remove very long "words"

  text <- gsub("\\S{19,}", "", text, ignore.case=TRUE, perl=TRUE)
  
# Fix typos and remove irrelevant text

  text <- gsub("pl\\s+anning", "planning", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("planni\\s+ng", "planning", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\banning", "planning", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bth\\s+e\\b", "the", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bwa\\s+ter\\b", "the", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bplanb", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\binternet\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bsettingstemporary\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bflintochlockonee\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\boctober\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bkelly\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bchairman\\s+richardson\\b", "chairman", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bjoe\\s+maltese\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bchair\\s+windom\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bchairman\\s+bennett\\b", "chairman", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\brobert\\s+watkins\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\btim\\s+cash\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bfebruary\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bapril\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bmanagement\\s+practice\\b", "management practices", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bwater\\s+planning\\b", "water plan", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bdirector\\s+barnes\\b", "", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bc\\s+hair\\b", "chair", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bdon\\t\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bone\\s+group\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bmake\\s+sure\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bcdocuments\\b", "documents", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bdoc\\b", "documents", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bwater\\s+ning\\b", "water", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bpc\\s+yes\\b", "water", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bplan\\s+ning\\b", "water", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bdoc\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bdoc\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bsummarydoc\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bchairman\\s+said\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bpc\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bcm\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bplanning\\s+contractor\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bw\\s+ater\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bed\\s+jefford\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bashley\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bdavid\\s+ashburn\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bms\\s+champion\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bjeff\\s+lukken\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bbecky\\s+champion\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bbob\\s+bomar\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text <- gsub("\\bwest\\s+point\\b", " ", text, ignore.case=TRUE, perl=TRUE)
  text
}

#===========================================================================
# PLOT AIMS BY REGION
#===========================================================================

#	Read in the data

allFiles <- read.csv("test2.csv", header = TRUE) %>% as_tibble

#	Let's pare down the variables to only what we want:

#	Because the actor type was creating addition rows for the same BMPs,
#	we exclude that here.

allFiles <- dplyr::select(allFiles, 
	c("BMP", "Council", "Period", "Description", "Short.Term.Actions", "Long.Term.Actions")) %>% distinct() %>% rowid_to_column("doc_id")

#	And a few more:

#	Make a single vector from these three vectors, which will be the BMP 
#	text we analyze. This has the brief description of the BMP, as well
#	as the short- and long-term actions entailed in implementation.

allFiles <- allFiles %>% unite("combo", "Description", "Short.Term.Actions", "Long.Term.Actions", sep=" ")

#	This removes the duplicated instances of BMP implementation actions
#	for each region.

allFiles <- allFiles[!duplicated(allFiles[,c('Council','combo')]),]

#	We'll double check the BMP entries against the manual entries and then
#	write out the ones that are not there (or that were duplicates).

allFiles <- allFiles[!allFiles$doc_id == 580,]
allFiles <- allFiles[!allFiles$doc_id == 581,]
allFiles <- allFiles[!allFiles$doc_id == 19,]
allFiles <- allFiles[!allFiles$doc_id == 141,]
allFiles <- allFiles[!allFiles$doc_id == 24,]
allFiles <- allFiles[!allFiles$doc_id == 25,]
allFiles <- allFiles[!allFiles$doc_id == 26,]
allFiles <- allFiles[!allFiles$doc_id == 28,]
allFiles <- allFiles[!allFiles$doc_id == 22,]
allFiles <- allFiles[!allFiles$doc_id == 196,]
allFiles <- allFiles[!allFiles$doc_id == 33,]
allFiles <- allFiles[!allFiles$doc_id == 415,]
allFiles <- allFiles[!allFiles$doc_id == 504,]
allFiles <- allFiles[!allFiles$doc_id == 505,]
allFiles <- allFiles[!allFiles$doc_id == 813,]
allFiles <- allFiles[!allFiles$doc_id == 318,]
allFiles <- allFiles[!allFiles$doc_id == 128,]
allFiles <- allFiles[!allFiles$doc_id == 319,]
allFiles <- allFiles[!allFiles$doc_id == 320,]
allFiles <- allFiles[!allFiles$doc_id == 321,]
allFiles <- allFiles[!allFiles$doc_id == 322,]
allFiles <- allFiles[!allFiles$doc_id == 323,]
allFiles <- allFiles[!allFiles$doc_id == 474,]
allFiles <- allFiles[!allFiles$doc_id == 502,]
allFiles <- allFiles[!allFiles$doc_id == 520,]
allFiles <- allFiles[!allFiles$doc_id == 552,]
allFiles <- allFiles[!allFiles$doc_id == 521,]
allFiles <- allFiles[!allFiles$doc_id == 522,]
allFiles <- allFiles[!allFiles$doc_id == 554,]
allFiles <- allFiles[!allFiles$doc_id == 523,]
allFiles <- allFiles[!allFiles$doc_id == 524,]
allFiles <- allFiles[!allFiles$doc_id == 555,]
allFiles <- allFiles[!allFiles$doc_id == 556,]
allFiles <- allFiles[!allFiles$doc_id == 525,]
allFiles <- allFiles[!allFiles$doc_id == 526,]
allFiles <- allFiles[!allFiles$doc_id == 743,]
allFiles <- allFiles[!allFiles$doc_id == 744,]
allFiles <- allFiles[!allFiles$doc_id == 324,]
allFiles <- allFiles[!allFiles$doc_id == 325,]
allFiles <- allFiles[!allFiles$doc_id == 727,]
allFiles <- allFiles[!allFiles$doc_id == 583,]
allFiles <- allFiles[!allFiles$doc_id == 584,]
allFiles <- allFiles[!allFiles$doc_id == 585,]
allFiles <- allFiles[!allFiles$doc_id == 586,]
allFiles <- allFiles[!allFiles$doc_id == 587,]
allFiles <- allFiles[!allFiles$doc_id == 494,]
allFiles <- allFiles[!allFiles$doc_id == 457,]

#	Run the tidify function to remove punctuation, digits, and very long
#	"words".

text <- tidify(allFiles$combo)

#	Make tokens

anno <- cnlp_annotate(allFiles, doc_name="doc_id", text_name="combo")$token

#	Stem the tokens

anno$stem <- wordStem(tolower(anno$token))

#===========================================================================
# Construct development
#===========================================================================

#	Joining the metadata of allFiles to the stemmed tokens.

bmps <- left_join(anno, allFiles, by = "doc_id")

#	Removing rows where Council or Period is NA

bmps <- bmps[!(is.na(bmps$Council)) | !(is.na(bmps$Period)),]

#	Counting the instances of each type of stemmed token for the data set "df"

df <- bmps %>% group_by(Council) %>% count(stem)
df <- cast(df, stem~Council, value="n", fill=0) %>% as.data.frame

#	Make the row names of df the stemmed tokens

row.names(df) <- df$stem

#	Drop the first column

df <- df[, -c(1)]

#	Making the construct for innovation

changeconstruct <- c(
  "adopt",
  "amend",
  "begin",
  "creat",
  "develop",
  "establish",
  "improv",
  "modifi",
  "reduc",
  "refin",
  "replac",
  "retrofit",
  "updat",
  "upgrad",
  "revis"
  )

change <- df[row.names(df) %in% changeconstruct,]

#	Getting the Cronbach's alpha of the innovation construct

tchange <-  data.frame(t(change))
library(ltm)
cronbach.alpha(tchange)

#	Making the construct for monitoring

monitconstruct <- c(
  "assess",
  "evalu",
  "gaug",
  "inspect",
  "investig",
  "monitor",
  "review",
  "survei",
  "track"
  )

monit <- df[row.names(df) %in% monitconstruct,]

#	Getting the Cronbach's alpha of the monitoring construct

tmonit <-  data.frame(t(monit))
library(ltm)
cronbach.alpha(tmonit)

#	Making the construct for collaboration

supportconstruct <- c(
  "assist", # original
  "facilit",
  "provid",
  "coordin", 
  "particip",
  "partner"
)

support <- df[row.names(df) %in% supportconstruct,]

#	Getting the Cronbach's alpha of the collaboration construct

tsupport <-  data.frame(t(support))
library(ltm)
cronbach.alpha(tsupport)


#===========================================================================
# Filter data frame to get surrounding sentences
#===========================================================================

# Develop a comprehensive list of tokens from the constructs we have generated.

constructs <- 
      c(changeconstruct, monitconstruct, supportconstruct)


relevant_sentences1 <- bmps %>% 
  filter(stem %in% constructs) %>% 
  dplyr::select(doc_id, sid) %>% 
  unite(doc_sent)

bmps1_sentences <- bmps %>% 
  unite(doc_sent, doc_id, sid, remove=F) %>% 
  filter(doc_sent %in% relevant_sentences1$doc_sent) %>%
  group_by(doc_sent) %>%
  summarise(sentence = paste(token, collapse=" "))

#===========================================================================
#	tf-itf DTM
#===========================================================================

#	Just to make sure we have continuity, we'll read in the data and process
#	this in the same fashion as we did prior to construct development.

#	Read in the data

allFiles <- read.csv("bmps_11_17.csv", header = TRUE) %>% as_tibble

#	Let's pare down the variables to only what we want:

#	Because the actor type was creating addition rows for the same BMPs,
#	we exclude that here.

allFiles <- dplyr::select(allFiles, 
	c("BMP", "Council", "Period", "Description", "Short.Term.Actions", "Long.Term.Actions")) %>% distinct() %>% rowid_to_column("doc_id")

#	And a few more:

#	Make a single vector from these three vectors, which will be the BMP 
#	text we analyze. This has the brief description of the BMP, as well
#	as the short- and long-term actions entailed in implementation.

allFiles <- allFiles %>% unite("combo", "Description", "Short.Term.Actions", "Long.Term.Actions", sep=" ")

#	This removes the duplicated instances of BMP implementation actions
#	for each region.

allFiles <- allFiles[!duplicated(allFiles[,c('Council','combo')]),]

#	We'll double check the BMP entries against the manual entries and then
#	write out the ones that are not there or that are part of repeated entries.

allFiles <- allFiles[!allFiles$doc_id == 580,]
allFiles <- allFiles[!allFiles$doc_id == 581,]
allFiles <- allFiles[!allFiles$doc_id == 19,]
allFiles <- allFiles[!allFiles$doc_id == 141,]
allFiles <- allFiles[!allFiles$doc_id == 24,]
allFiles <- allFiles[!allFiles$doc_id == 25,]
allFiles <- allFiles[!allFiles$doc_id == 26,]
allFiles <- allFiles[!allFiles$doc_id == 28,]
allFiles <- allFiles[!allFiles$doc_id == 22,]
allFiles <- allFiles[!allFiles$doc_id == 196,]
allFiles <- allFiles[!allFiles$doc_id == 33,]
allFiles <- allFiles[!allFiles$doc_id == 415,]
allFiles <- allFiles[!allFiles$doc_id == 504,]
allFiles <- allFiles[!allFiles$doc_id == 505,]
allFiles <- allFiles[!allFiles$doc_id == 813,]
allFiles <- allFiles[!allFiles$doc_id == 318,]
allFiles <- allFiles[!allFiles$doc_id == 128,]
allFiles <- allFiles[!allFiles$doc_id == 319,]
allFiles <- allFiles[!allFiles$doc_id == 320,]
allFiles <- allFiles[!allFiles$doc_id == 321,]
allFiles <- allFiles[!allFiles$doc_id == 322,]
allFiles <- allFiles[!allFiles$doc_id == 323,]
allFiles <- allFiles[!allFiles$doc_id == 474,]
allFiles <- allFiles[!allFiles$doc_id == 502,]
allFiles <- allFiles[!allFiles$doc_id == 520,]
allFiles <- allFiles[!allFiles$doc_id == 552,]
allFiles <- allFiles[!allFiles$doc_id == 521,]
allFiles <- allFiles[!allFiles$doc_id == 522,]
allFiles <- allFiles[!allFiles$doc_id == 554,]
allFiles <- allFiles[!allFiles$doc_id == 523,]
allFiles <- allFiles[!allFiles$doc_id == 524,]
allFiles <- allFiles[!allFiles$doc_id == 555,]
allFiles <- allFiles[!allFiles$doc_id == 556,]
allFiles <- allFiles[!allFiles$doc_id == 525,]
allFiles <- allFiles[!allFiles$doc_id == 526,]
allFiles <- allFiles[!allFiles$doc_id == 743,]
allFiles <- allFiles[!allFiles$doc_id == 744,]
allFiles <- allFiles[!allFiles$doc_id == 324,]
allFiles <- allFiles[!allFiles$doc_id == 325,]
allFiles <- allFiles[!allFiles$doc_id == 727,]
allFiles <- allFiles[!allFiles$doc_id == 583,]
allFiles <- allFiles[!allFiles$doc_id == 584,]
allFiles <- allFiles[!allFiles$doc_id == 585,]
allFiles <- allFiles[!allFiles$doc_id == 586,]
allFiles <- allFiles[!allFiles$doc_id == 587,]
allFiles <- allFiles[!allFiles$doc_id == 494,]
allFiles <- allFiles[!allFiles$doc_id == 457,]

#	Run the tidify function to remove punctuation, digits, and very long
#	"words".

text <- tidify(allFiles$combo)

#	Tokenize the text

anno <- cnlp_annotate(allFiles, doc_name="doc_id", text_name="combo")$token

#	Stem the tokens

anno$stem <- wordStem(tolower(anno$token))

#===========================================================================

bmps <- left_join(anno, allFiles, by = "doc_id")

bmps <- bmps[!(is.na(bmps$Council)) | !(is.na(bmps$Period)),]

bmps11 <- bmps[bmps$Period == 1,]

x11 <- as.matrix(bmps11$token)
x11 <- tolower(x11)
x11 <- wordStem(x11)

counts11 <- table(x11)

#	Change construct

construct1 <- c(
  "adopt",
  "amend",
  "begin",
  "creat",
  "develop",
  "establish",
  "improv",
  "modifi",
  "reduc",
  "refin",
  "replac",
  "retrofit",
  "updat",
  "upgrad",
  "revis"
  )

# Monitoring construct

construct2 <- c(
  "assess",
  "evalu",
  "gaug",
  "inspect",
  "investig",
  "monitor",
  "review",
  "survei",
  "track"
  )

# Coordination construct

construct4 <- c(
  "assist", # original
  "facilit",
  "provid",
  "coordin", 
  "particip",
  "partner"
)
bmps17 <- bmps[bmps$Period == 2,]

x17 <- as.matrix(bmps17$token)
x17 <- tolower(x17)
x17 <- wordStem(x17)

counts17 <- table(x17)


tf11 <- cnlp_utils_tf(bmps11, token_var = "stem", vocabulary = unique(c(construct1, construct2, construct4)))
tf17 <- cnlp_utils_tf(bmps17, token_var = "stem", vocabulary = unique(c(construct1, construct2, construct4)))

# term frequency inverse term count, i.e. proportion of tokens that match this token

# This creates a proportionality matrix of the individuals terms
# that were used to create the constructs. This allows us to see each
# selected term in the document in proportion to all of the selected 
# documents.

doc_length11 <- bmps11 %>% group_by(doc_id) %>% tally()
tfit11 <- data.frame(as.matrix(tf11) / doc_length11$n)

doc_length17 <- bmps17 %>% group_by(doc_id) %>% tally()
tfit17 <- data.frame(as.matrix(tf17) / doc_length17$n)

#	This creates a proprtionality matrix of the terms to number of terms in 
#	the document, where each "term" is a construct we have generated.

cfit11 <- data.frame(
	construct1 = unlist(rowSums(tfit11[, construct1])),
	construct2 = unlist(rowSums(tfit11[, construct2])),
	construct4 = unlist(rowSums(tfit11[, construct4]))
)

cfit11$doc_id <- as.numeric(row.names(cfit11))
cfit11 <- cfit11 %>% 
  left_join(allFiles %>% dplyr::select(doc_id, Council), by=c("doc_id")) %>%
  # left_join(allFiles %>% dplyr::select(doc_id, Council, parties_typology), by=c("doc_id")) %>%
  dplyr::select(-c(doc_id)) %>%
  plyr::rename(replace=c("Council"= "id"))
# cfit11 <- melt(cfit11, id.vars=c("id", "parties_typology")) # NEED TO FIX 12/1-/21
cfit11 <- reshape::melt(cfit11, id.vars="id")
cfit11$year <- 2011

# This creates a proprtionality matrix of the terms to number of terms in 
# the document, where each "term" is a construct we have generated.

cfit17 <- data.frame(
  construct1 = unlist(rowSums(tfit17[, construct1])),
  construct2 = unlist(rowSums(tfit17[, construct2])),
  construct4 = unlist(rowSums(tfit17[, construct4]))
)

cfit17$doc_id <- as.numeric(row.names(cfit17))
cfit17 <- cfit17 %>% 
  left_join(allFiles %>% dplyr::select(doc_id, Council), by=c("doc_id")) %>%
  # left_join(allFiles %>% dplyr::select(doc_id, Council, parties_typology), by=c("doc_id")) %>%
  dplyr::select(-c(doc_id)) %>%
  plyr::rename(replace=c("Council"= "id"))
# cfit17 <- dplyr::melt(cfit17, id.vars=c("id", "parties_typology")) #NEED TO FIX
cfit17 <- reshape::melt(cfit17, id.vars=c("id"))
cfit17$year <- 2017

cfit <- rbind(cfit11, cfit17)

#	Get some heatmaps of cfit and tfit.

library(ggplot2)


cfit$variable <- recode(cfit$variable,
									construct1 = "Innovation",
									construct2 = "Monitoring",
									construct4 = "Collaboration",
									)

# cfit$variable <- factor(cfit$variable, levels = c("Change", "Collaboration", "Monitoring"))

cfit$value <- round(cfit$value, digits = 3)


cfit$variable <- as.factor(cfit$variable)

cfit$variable <- factor(cfit$variable, c("Innovation", "Collaboration", "Monitoring"))

cfit$id <- recode(cfit$id,
                        "ALT" = "Altamaha",
                        "CGA" = "Coastal Georgia",
                        "CNG" = "Coosa North \nGeorgia",
                        "LFO" = "Lower Flint-\nOchlockonee",
                        "MCH" = "Middle \nChattahoochee",
                        "MOC" = "Middle Ocmulgee",
                        "SSA" = "Suwanee Satilla",
                        "SUO" = "Savannah-Upper \nOgeechee",
                        "UFL" = "Upper Flint",
                        "UOC" = "Upper Oconee")

#	To plot the averages for regional proportions

a <- ggplot(cfit %>% group_by(id, variable, year) %>% summarise(value = mean(value)), aes(id, year, fill = value)) +
geom_tile(color = "white") +
scale_y_reverse(breaks = unique(cfit$year)) + #continuous(trans = "reverse", breaks = unique(cfit$year)) + 
scale_x_discrete() +
scale_fill_gradientn(colors = c("lightblue","mediumblue"), 
	name="Proportion \nof \nWords") +
theme_minimal() + # minimal theme
geom_text(aes(label = sprintf("%0.2f%%", value * 100)), color = "black", size = 3.5) +
facet_wrap(.~variable, ncol=1) +
theme(legend.position = "none",
	axis.title.x = element_text(size = 0),
	axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
	axis.text.y = element_text(hjust = 1, vjust = .5, size = 10),
	axis.title.y = element_text(size = 0),
	panel.grid.major = element_blank(), 
	panel.grid.minor = element_blank(),
	strip.text = element_text(size=12))

time_plottable <- cfit %>% 
  group_by(id, variable, year) %>% 
  summarise(value = mean(value)) %>%
  ungroup() %>%
  group_by(variable, year) %>%
  summarise(value = mean(value)) %>%
  mutate(panel = "Average", id = "Average")

#   Plot the averages for the regional proportions:

time_plottable$x <- "Average Percentage"

time_plottable$variable <- as.factor(time_plottable$variable )

time_plottable$variable  <- factor(time_plottable$variable , c("Innovation", "Collaboration", "Monitoring"))

b <- ggplot(time_plottable, aes(x, year, fill = value)) +
geom_tile(color = "white") +
scale_y_reverse(breaks = unique(time_plottable$year)) + #continuous(trans = "reverse", breaks = unique(cfit$year)) + 
scale_x_discrete() +
scale_fill_gradientn(colors=c("lightblue","mediumblue"), 
  name="Proportion \nof \nWords") +
theme_minimal() + # minimal theme
# geom_text(aes(label = scales::percent(avg)), color = "black", size = 4) +
geom_text(aes(label = sprintf("%0.2f%%", value * 100)), color = "black", size = 3.5) +
facet_wrap(.~variable, ncol=1) +
theme(legend.position = "none",
  axis.title.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  strip.text = element_text(size = 12, colour = "white")
  )

# Read in a library so that we can bring all this stuff together.

library(gridExtra)

# Bring all of the regions and the average together to make a figure for 
# proportions of constructions within each RWP.

gridplot <- grid.arrange(arrangeGrob(a + theme(legend.position = 'hidden'), 
  b + theme(legend.position = 'hidden'), ncol=2, nrow=1, widths=c(8, 1.15)))

ggsave("tf_itf_avgs.png", gridplot, width = 5.5, heigh = 6, units = "in")

#===========================================================================
# To plot the distribution of parties reported responsible for BMPs
#===========================================================================

#	Now we're going to do the same process to plot the 

cfit <- cfit %>% drop_na(parties_typology)

summarized <- aggregate(cfit$value, by=list(Parties=cfit$parties_typology, Construct=cfit$variable, Region=cfit$id, Year=cfit$year), FUN=sum)

summarized$Region <- recode(summarized$Region,
                        "Altamaha" = "ALT",
                        "Coastal Georgia" = "CGA",
                        "Coosa North \nGeorgia" = "CNG",
                        "Lower Flint-\nOchlockonee" = "LFO",
                        "Middle \nChattahoochee" = "MCH",
                        "Middle Ocmulgee" = "MOC",
                        "Suwanee Satilla" = "SSA",
                        "Savannah-Upper \nOgeechee" = "SUO",
                        "Upper Flint" = "UFL",
                        "Upper Oconee" = "UOC"
                  )

summarized$Parties <- recode(summarized$Parties,
                      "local government" = "Local Govt.",
                      "research_academia" = "Research",
                      "state government" = "State Govt.", 
                      "users" = "Users", 
                      "utilities" = "Utilities", 
                      "interest groups" = "Interest Groups", 
                      "regional council" = "Regional Council",
                      "federal government" = "Federal Govt.", 
                      "npo" = "Non-profit", 
                      "private" = "Private"
                      )

#===========================================================================
#	PLOT ACTOR BREAKDOWN
#===========================================================================

#	Just to be safe, we'll do the data prep over again one more time before
#	plotting.

allFiles <- read.csv("bmps_11_17.csv", header = TRUE) %>% 
                  as_tibble %>% rowid_to_column("doc_id")

allFiles <- allFiles %>% 
unite("combo", "Description", "Short.Term.Actions", "Long.Term.Actions", sep=" ")


allFiles <- allFiles[!duplicated(allFiles[,c('Council','combo')]),]
allFiles$doc_id <- seq(1, nrow(allFiles))

text <- tidify(allFiles$combo)

anno <- cnlp_annotate(allFiles, doc_name="doc_id", text_name="combo")$token

anno$stem <- wordStem(tolower(anno$token))

#===========================================================================

bmps <- anno %>% left_join(allFiles %>% dplyr::select(doc_id, Council, Period, parties_typology))

bmps <- bmps[!(is.na(bmps$Council)) | !(is.na(bmps$Period)),]

bmps11 <- bmps[bmps$Period == 1,]

x11 <- as.matrix(bmps11$token)
x11 <- tolower(x11)
x11 <- wordStem(x11)

counts11 <- table(x11)

#	Change construct

construct1 <- c(
  "adopt",
  "amend",
  "begin",
  "creat",
  "develop",
  "establish",
  "improv",
  "modifi",
  "reduc",
  "refin",
  "replac",
  "retrofit",
  "updat",
  "upgrad",
  "revis"
  )

# Monitoring construct

construct2 <- c(
  "assess",
  "evalu",
  "gaug",
  "inspect",
  "investig",
  "monitor",
  "review",
  "survei",
  "track"
  )

# Coordination

construct4 <- c(
  "assist", # original
  "facilit",
  "provid",
  "coordin", 
  "particip",
  "partner"
)
bmps17 <- bmps[bmps$Period == 2,]

x17 <- as.matrix(bmps17$token)
x17 <- tolower(x17)
x17 <- wordStem(x17)

counts17 <- table(x17)


tf11 <- cnlp_utils_tf(bmps11, token_var = "stem", vocabulary = unique(c(construct1, construct2, construct4)))
tf17 <- cnlp_utils_tf(bmps17, token_var = "stem", vocabulary = unique(c(construct1, construct2, construct4)))

# Term frequency inverse term count, i.e. proportion of tokens that match this token

# This creates a proportionality matrix of the individuals terms
# that were used to create the constructs. This allows us to see each
# selected term in the document in proportion to all of the selected 
# documents.

doc_length11 <- bmps11 %>% group_by(doc_id) %>% tally()
tfit11 <- data.frame(as.matrix(tf11) / doc_length11$n)

doc_length17 <- bmps17 %>% group_by(doc_id) %>% tally()
tfit17 <- data.frame(as.matrix(tf17) / doc_length17$n)

#	This creates a proprtionality matrix of the terms to number of terms in 
#	the document, where each "term" is a construct we have generated.

cfit11 <- data.frame(
	construct1 = unlist(rowSums(tfit11[, construct1])),
	construct2 = unlist(rowSums(tfit11[, construct2])),
	construct4 = unlist(rowSums(tfit11[, construct4]))
)

cfit11$doc_id <- as.numeric(row.names(cfit11))
cfit11 <- cfit11 %>% 
  left_join(allFiles %>% dplyr::select(doc_id, Council, parties_typology), by=c("doc_id")) %>%
  dplyr::select(-c(doc_id)) %>%
  plyr::rename(replace=c("Council"= "id"))
cfit11 <- reshape::melt(cfit11, id.vars=c("id", "parties_typology"))
cfit11$year <- 2011

# This creates a proprtionality matrix of the terms to number of terms in 
# the document, where each "term" is a construct we have generated.

cfit17 <- data.frame(
  construct1 = unlist(rowSums(tfit17[, construct1])),
  construct2 = unlist(rowSums(tfit17[, construct2])),
  construct4 = unlist(rowSums(tfit17[, construct4]))
)

cfit17$doc_id <- as.numeric(row.names(cfit17))
cfit17 <- cfit17 %>% 
  left_join(allFiles %>% dplyr::select(doc_id, Council, parties_typology), by=c("doc_id")) %>%
  dplyr::select(-c(doc_id)) %>%
  plyr::rename(replace=c("Council"= "id"))
cfit17 <- reshape::melt(cfit17, id.vars=c("id", "parties_typology"))
cfit17$year <- 2017

cfit <- rbind(cfit11, cfit17)

#	Get some heatmaps of cfit and tfit.

library(ggplot2)

cfit$variable <- recode(cfit$variable,
									construct1 = "Innovation",
									construct2 = "Monitoring",
									construct4 = "Collaboration",
									)

cfit$value <- round(cfit$value, digits = 3)

#===========================================================================
# Quick descriptives so we have a count of actor types by region, time
#===========================================================================

#   We're going to convert the parties to a factor real quick for some counts.
#   We'll convert this back to character in a minute to continue on our way.

cfit$parties_typology <- as.factor(cfit$parties_typology)

cfit <- cfit[cfit$value > 0,]


cact <- cfit %>% group_by(id, parties_typology, variable, year) %>% tally()

cact$parties_typology <- as.character(cact$parties_typology)

cact$parties_typology[cact$parties_typology == "local government"] <- "Local Govt."
cact$parties_typology[cact$parties_typology == "state government"] <- "State Govt."
cact$parties_typology[cact$parties_typology == "users"] <- "Users"
cact$parties_typology[cact$parties_typology == "utilities"] <- "Utilities"
cact$parties_typology[cact$parties_typology == "interest groups"] <- "Interest Groups"
cact$parties_typology[cact$parties_typology == "regional council"] <- "Regional Council"
cact$parties_typology[cact$parties_typology == "research_academia"] <- "Academic"
cact$parties_typology[cact$parties_typology == "private"] <- "Private"
cact$parties_typology[cact$parties_typology == "federal government"] <- "Federal Govt."

cact11 <- cact[cact$year == 2011,]
cact17 <- cact[cact$year == 2017,]

cors <- c("#e1be6a", "#40b0a6", "#009e73", "#d55e00", "#cc79a7", "#f0e442", "#0072b2")

library(ggpubr)

cact$n <- as.numeric(cact$n)
cact$variable <- as.character(cact$variable)
cact <- cact %>% drop_na(parties_typology)
cact$id <- as.factor(cact$id)

cact$id  <- factor(cact$id, levels = rev(sort(unique(cact$id))))

cact$variable <- factor(cact$variable,      # Reordering group factor levels
                         levels = c("Innovation", "Collaboration", "Monitoring"))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

dfl <- pivot_longer(cact, names_to = "Type", values_to = "Count", 1:2) 
plot <- ggplot(cact,  aes(y = n, x = parties_typology, col = parties_typology)) + 
        geom_jitter(position = position_jitter(height = 0, width=0.25), size = 3, alpha = .8) +
        facet_grid(variable ~ year, margins = FALSE) +
        scale_color_manual(values = cbPalette) +
		theme_light() +
		theme(strip.background = element_rect(color="darkgray", fill="white", size=1, linetype="solid")) +
		theme(strip.text = element_text(colour = 'black')) +
		theme(legend.position = "bottom") +
		ylab("Count") +
		xlab("") +
		theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
		labs(col = "Stakeholder Type")

ggsave("actor_breakdown.jpeg", plot, width = 7, height = 7, units = "in")

#========================================================================
# DESCRIPTIVE BMP PLOTS
#========================================================================

#	This is just to show how many articles there are between the two periods.

allFiles <- read.csv("descriptives_plot.csv", header = TRUE) %>%  as_tibble %>% rowid_to_column("doc_id")

dta <- read.csv("bmp_breakdown.csv", header = TRUE, fileEncoding = "UTF-8-BOM")

dta$Year = as.factor(dta$Year)

bmplot <- ggplot(dta, aes(x=Region, y=num_bmp, fill=Year)) + 
  scale_fill_manual(values = c("Gray", "#E1AD01")) +
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  theme_classic() +
  ylab("Number of BMPs")

ggsave("num_bmps.jpeg", bmplot, width = 7, height = 5, units = "in")

#===========================================================================
#	tf-itf DTM
#===========================================================================

#	This is for the final check against the random constructs.

#	Read in the data

allFiles <- read.csv("bmps_11_17.csv", header = TRUE) %>% as_tibble

#	Let's pare down the variables to only what we want:

#	Because the actor type was creating addition rows for the same BMPs,
#	we exclude that here.

allFiles <- dplyr::select(allFiles, 
	c("BMP", "Council", "Period", "Description", "Short.Term.Actions", "Long.Term.Actions")) %>% distinct() %>% rowid_to_column("doc_id")

#	And a few more:

#	Make a single vector from these three vectors, which will be the BMP 
#	text we analyze. This has the brief description of the BMP, as well
#	as the short- and long-term actions entailed in implementation.

allFiles <- allFiles %>% unite("combo", "Description", "Short.Term.Actions", "Long.Term.Actions", sep=" ")

#	This removes the duplicated instances of BMP implementation actions
#	for each region.

allFiles <- allFiles[!duplicated(allFiles[,c('Council','combo')]),]

#	We'll double check the BMP entries against the manual entries and then
#	write out the ones that are not there.

allFiles <- allFiles[!allFiles$doc_id == 580,]
allFiles <- allFiles[!allFiles$doc_id == 581,]
allFiles <- allFiles[!allFiles$doc_id == 19,]
allFiles <- allFiles[!allFiles$doc_id == 141,]
allFiles <- allFiles[!allFiles$doc_id == 24,]
allFiles <- allFiles[!allFiles$doc_id == 25,]
allFiles <- allFiles[!allFiles$doc_id == 26,]
allFiles <- allFiles[!allFiles$doc_id == 28,]
allFiles <- allFiles[!allFiles$doc_id == 22,]
allFiles <- allFiles[!allFiles$doc_id == 196,]
allFiles <- allFiles[!allFiles$doc_id == 33,]
allFiles <- allFiles[!allFiles$doc_id == 415,]
allFiles <- allFiles[!allFiles$doc_id == 504,]
allFiles <- allFiles[!allFiles$doc_id == 505,]
allFiles <- allFiles[!allFiles$doc_id == 813,]
allFiles <- allFiles[!allFiles$doc_id == 318,]
allFiles <- allFiles[!allFiles$doc_id == 128,]
allFiles <- allFiles[!allFiles$doc_id == 319,]
allFiles <- allFiles[!allFiles$doc_id == 320,]
allFiles <- allFiles[!allFiles$doc_id == 321,]
allFiles <- allFiles[!allFiles$doc_id == 322,]
allFiles <- allFiles[!allFiles$doc_id == 323,]
allFiles <- allFiles[!allFiles$doc_id == 474,]
allFiles <- allFiles[!allFiles$doc_id == 502,]
allFiles <- allFiles[!allFiles$doc_id == 520,]
allFiles <- allFiles[!allFiles$doc_id == 552,]
allFiles <- allFiles[!allFiles$doc_id == 521,]
allFiles <- allFiles[!allFiles$doc_id == 522,]
allFiles <- allFiles[!allFiles$doc_id == 554,]
allFiles <- allFiles[!allFiles$doc_id == 523,]
allFiles <- allFiles[!allFiles$doc_id == 524,]
allFiles <- allFiles[!allFiles$doc_id == 555,]
allFiles <- allFiles[!allFiles$doc_id == 556,]
allFiles <- allFiles[!allFiles$doc_id == 525,]
allFiles <- allFiles[!allFiles$doc_id == 526,]
allFiles <- allFiles[!allFiles$doc_id == 743,]
allFiles <- allFiles[!allFiles$doc_id == 744,]
allFiles <- allFiles[!allFiles$doc_id == 324,]
allFiles <- allFiles[!allFiles$doc_id == 325,]
allFiles <- allFiles[!allFiles$doc_id == 727,]
allFiles <- allFiles[!allFiles$doc_id == 583,]
allFiles <- allFiles[!allFiles$doc_id == 584,]
allFiles <- allFiles[!allFiles$doc_id == 585,]
allFiles <- allFiles[!allFiles$doc_id == 586,]
allFiles <- allFiles[!allFiles$doc_id == 587,]
allFiles <- allFiles[!allFiles$doc_id == 494,]
allFiles <- allFiles[!allFiles$doc_id == 457,]

text <- tidify(allFiles$combo)

anno <- cnlp_annotate(allFiles, doc_name="doc_id", text_name="combo")$token

anno$stem <- wordStem(tolower(anno$token))

#===========================================================================

bmps <- left_join(anno, allFiles, by = "doc_id")

bmps <- bmps[!(is.na(bmps$Council)) | !(is.na(bmps$Period)),]

bmps11 <- bmps[bmps$Period == 1,]

x11 <- as.matrix(bmps11$token)
x11 <- tolower(x11)
x11 <- wordStem(x11)

counts11 <- table(x11)

random_a <- c(
                "aquif",
                "classif",
                "cistern",
                "custom",
                "reach",
                "yield",
                "zone",
                "tributari",
                "center"
                )

random_b <- c(
                "buffer",
                "wildlif",
                "weather",
                "trigger",
                "grass",
                "factor",
                "excess",
                "ambient",
                "commiss",
                "convert"
                )

random_c <- c(
                "certif",
                "trend",
                "green",
                "fiscal",
                "accur",
                "amount",
                "calibr",
                "differ",
                "ecolog",
                "feet"
                  )


changeconstruct <- c(
  "adopt",
  "amend",
  "begin",
  "creat",
  "develop",
  "establish",
  "improv",
  "modifi",
  "reduc",
  "refin",
  "replac",
  "retrofit",
  "updat",
  "upgrad",
  "revis"
  )

monitconstruct <- c(
  "assess",
  "evalu",
  "gaug",
  "inspect",
  "investig",
  "monitor",
  "review",
  "survei",
  "track"
  )


collabconstruct <- c(
  "assist", # original
  "facilit",
  "provid",
  "coordin", 
  "particip",
  "partner",
  "messag",
  "share"
)
  
bmps17 <- bmps[bmps$Period == 2,]


x17 <- as.matrix(bmps17$token)
x17 <- tolower(x17)
x17 <- wordStem(x17)

counts17 <- table(x17)

tf11 <- cnlp_utils_tf(bmps11, token_var = "stem", vocabulary = unique(c(random_a, random_b, random_c, changeconstruct, monitconstruct, collabconstruct)))
tf17 <- cnlp_utils_tf(bmps17, token_var = "stem", vocabulary = unique(c(random_a, random_b, random_c, changeconstruct, monitconstruct, collabconstruct)))

# Term frequency inverse term count, i.e. proportion of tokens that match this token

# This creates a proportionality matrix of the individuals terms
# that were used to create the constructs. This allows us to see each
# selected term in the document in proportion to all of the selected 
# documents.

doc_length11 <- bmps11 %>% group_by(doc_id) %>% tally()
tfit11 <- data.frame(as.matrix(tf11) / doc_length11$n)

doc_length17 <- bmps17 %>% group_by(doc_id) %>% tally()
tfit17 <- data.frame(as.matrix(tf17) / doc_length17$n)

#	This creates a proprtionality matrix of the terms to number of terms in 
#	the document, where each "term" is a construct we have generated.

cfit11 <- data.frame(
  random_a = unlist(rowSums(tfit11[, random_a])),
  random_b = unlist(rowSums(tfit11[, random_b])),
  random_c = unlist(rowSums(tfit11[, random_c])),
  changeconstruct = unlist(rowSums(tfit11[, changeconstruct])),
  monitconstruct = unlist(rowSums(tfit11[, monitconstruct])),
  collabconstruct = unlist(rowSums(tfit11[, collabconstruct]))
                    )

cfit11$doc_id <- as.numeric(row.names(cfit11))
cfit11 <- cfit11 %>% 
  left_join(allFiles %>% dplyr::select(doc_id, Council), by=c("doc_id")) %>%
dplyr::select(-c(doc_id)) %>%
plyr::rename(replace=c("Council"= "id"))
cfit11 <- reshape::melt(cfit11, id.vars=c("id"))
cfit11$year <- 2011

# This creates a proprtionality matrix of the terms to number of terms in 
# the document, where each "term" is a construct we have generated.

cfit17 <- data.frame(
  random_a = unlist(rowSums(tfit17[, random_a])),
  random_b = unlist(rowSums(tfit17[, random_b])),
  random_c = unlist(rowSums(tfit17[, random_c])),
  changeconstruct = unlist(rowSums(tfit17[, changeconstruct])),
  monitconstruct = unlist(rowSums(tfit17[, monitconstruct])),
  collabconstruct = unlist(rowSums(tfit17[, collabconstruct]))
)

cfit17$doc_id <- as.numeric(row.names(cfit17))
cfit17 <- cfit17 %>% 
  left_join(allFiles %>% dplyr::select(doc_id, Council,), by=c("doc_id")) %>%
  dplyr::select(-c(doc_id)) %>%
  plyr::rename(replace=c("Council"= "id"))
cfit17 <- reshape::melt(cfit17, id.vars=c("id"))
cfit17$year <- 2017

cfit <- rbind(cfit11, cfit17)
cfit$construct_group <- "Chosen"
cfit$construct_group[startsWith(as.character(cfit$variable), "r")] <- "Random"

#	Get some heatmaps of cfit and tfit.

library(ggplot2)


cfit$id <- recode(cfit$id,
					ALT = "Altamaha",
					CGA = "Coastal Georgia",
					CNG = "Coosa North \nGeorgia",
					LFO = "Lower Flint-\nOchlockonee",
					MCH = "Middle \nChattahoochee",
					MOC = "Middle Ocmulgee",
					SSA = "Suwanee Satilla",
					SUO = "Savannah-Upper \nOgeechee",
					UFL = "Upper Flint",
					UOC = "Upper Oconee"
				)

cfit$variable <- recode(cfit$variable,
							random_a = "Random A",
							random_b = "Random B",
							random_c = "Random C",
							changeconstruct = "Innovation",
							monitconstruct = "Monitoring",
		  					collabconstruct = "Collaboration"
							)


cfit <- dplyr::select(cfit, c("id", "variable", "value", "year", "construct_group"))

cfit2 <- reshape2::dcast(cfit, id + variable + construct_group ~ year, fun.aggregate=mean)

cfit2 <- cfit2[!cfit2$variable == "year",]


df <- cfit2

# Plot for the appendix: random constructs vs. chosen for innovation, monitoring, and collab

p <- ggplot(df) + geom_segment(aes(x=1, xend=2, y=`2011`, yend=`2017`, col=variable), size=.75, show.legend=F) + 
                  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
                  geom_vline(xintercept=2, linetype="dashed", size=.1) +
                  # scale_color_manual(labels = c("Up", "Down"), 
                  #                    values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
                  labs(x="", y="Proportion of Construct Frequency") +  # Axis labels
                  xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`2011`, df$`2017`)))) +
                  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                       "#D55E00", "#0072B2")) +
                  theme_classic()+
                  facet_wrap(. ~ variable, labeller = label_value)  # X and Y axis limits

p <- p + geom_text(label="2011", x=1, y=1.1*(max(df$`2011`, df$`2017`)), hjust=1.2, size=3)  # title
p <- p + geom_text(label="2017", x=2, y=1.1*(max(df$`2011`, df$`2017`)), hjust=-0.1, size=3)  # title

# Minify theme

p + theme(panel.background = element_blank(), 
           panel.grid = element_blank(),
           axis.ticks = element_blank(),
           axis.text.x = element_blank(),
           panel.border = element_blank(),
           plot.margin = unit(c(1,2,1,2), "cm"))

ggsave("sanity_check.jpeg", p, width = 20, height = 15, units = "cm")

