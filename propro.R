########## ESCC-pre


# Directories

setwd("m://users//stefange//OneDrive - NTNU//2-data//crisis") # This is where your textfiles are stored.
# setwd("d://users//stefange//OneDrive - NTNU//2-data//crisis")
# setwd("c://users//stefange//OneDrive - NTNU//2-data//crisis")

# Load

# load(file="__crisis.RData")

# Libraries

library(data.table)				# For data representation.
library(stringr)				# For fast and efficient text processing and modification.
library(tidyverse)				# For data organization and re-organization. Also text analysis.
library(readtext)				# For reading text files into data.
library(quanteda)				# For various types of text analysis, preprocessing and representation.
library(spacyr)					# For natural language processing and named entity recognition in particular.
library(tm)						# For topic models.
library(quanteda.dictionaries)	# For various types of text analysis, preprocessing and representation.
library(topicmodels)	# For topic models.
library(ldatuning)	# For finding the optimal number of topics in topic models.
library(ggplot2)	# For plotting.
library(car)		# For recoding.
library(zoo)		# For time series analysis.
library(stm)		# For topic modeling.
library(stringdist) # functions for calculating the similarity/dissimilarity of text strings. For word similarity recognition and word substitution.
library(babynames)	# a list of all names used in the US in the past. Surnames and first names. Allows identifying person names and prevent them from being overwritten with "real" words.
library(wru)		# Includes a list of all surnames in the 2010 US census ("surnames2010").
library(lexicon)

# Loading text files

filelist1 <- list.files(pattern = "*.txt")
	# filelist gives the names of the raw text files
	setwd("m://users//stefange//OneDrive - NTNU//2-data//crisis//1871-1890//original")
	
filelist2 <- list.files(pattern = "*.txt")
	setwd("m://users//stefange//OneDrive - NTNU//2-data//crisis")
# filelist <- c(filelist1,filelist2)

filelist <- c(filelist1)

textfiles <- readtext::readtext("*.txt") # reads the texts from all .txt files in the current working directory

textfiles$id <- str_extract(textfiles[,1],"[:upper:]{2}[:digit:]{5,15}") # extracts the Gale ID number of the articles for later identification of units.

metadata <- rbind(  read.csv("crisis 1700-1870_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1871-1890_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1891-1900_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1901-1910_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 1911-1915_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1916-1920_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1921-1925_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1926-1930_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1931-1935_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 1936-1940_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1941-1950_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1951-1955_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1956-1960_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1961-1965_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 1966-1970_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1971-1973_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1974-1976_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1977-1980_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1981-1982_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 1983-1985_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1986-1988_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1989-1990_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1991-1992_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1993-1994_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 1995-1997_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 1998-1999_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2000-2001_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2002-2003_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2004-2005_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2006-2007_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 2008_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2009_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2010_metadata.csv",sep=",",row.names=11,header=TRUE),					
					read.csv("crisis 2011-2012_metadata.csv",sep=",",row.names=11,header=TRUE),
					read.csv("crisis 2013-2015_metadata.csv",sep=",",row.names=11,header=TRUE)) # This imports the metadata for all articles.


# Metadata					
				
names(metadata) <- c("headline","type","genre","newspaper","date","publisher","place","author","archive") # Gives meaningful variable names for the metadata.
metadata$id <- str_extract(rownames(metadata),"[:upper:]{2}[:digit:]{5,15}") # Creates a variable for the ID, which was previously only represented as rownumbers.

metadatamatcher <- match(textfiles$id,metadata$id) # A vector of which textfiles match which metadata entries, by Gale ID number as identifier.

textfiles$date <- metadata[metadatamatcher,"date"] # assigns metadata to textfile object.
textfiles$newspaper <- metadata[metadatamatcher,"newspaper"] # assigns metadata to textfile object.
textfiles$headline <- metadata[metadatamatcher,"headline"] # assigns metadata to textfile object.
textfiles$genre <- metadata[metadatamatcher,"genre"] # assigns metadata to textfile object.
textfiles$place <- metadata[metadatamatcher,"place"] # assigns metadata to textfile object.
textfiles$publisher <- metadata[metadatamatcher,"publisher"] # assigns metadata to textfile object.
textfiles$author <- metadata[metadatamatcher,"author"] # assigns metadata to textfile object.
textfiles$archive <- metadata[metadatamatcher,"archive"] # assigns metadata to textfile object.
textfiles$type <- metadata[metadatamatcher,"type"] # assigns metadata to textfile object.

monthnames <- c("January","February","March","April","May","June","July","August","September","October","November","December") 

textfiles$year <- str_extract(textfiles$date,"[:digit:]{4}") # Extracts the year from the date metadata.
textfiles$day <- str_extract(textfiles$date,"[:digit:]{1,2}") # Extracts the day of the month from the date metadata.
textfiles$month <- str_extract(textfiles$date,"[:alpha:]{3,9}") # Extracts the month from the data metadata.
textfiles$Month <- Recode(textfiles$month,"	'January'='01';'February'='02';'March'='03';
											'April'='04';'May'='05';'June'='06';'July'='07';
											'August'='08';'September'='09';'October'='10';
											'November'='11';'December'='12'") # Month numbers are changed to month names. This prevents mixing days and months when switching between dd/mm/yyyy and mm/dd/yyyy formats.

textfiles$time <- with(textfiles,paste0(year,"-",month,"-",day)) # Full representation of date as numbers.
textfiles$Time <- with(textfiles,paste0(year,"-",Month,"-",day)) # Full representation of date, with month names.

theeconomist <- cbind(	year=1843:2015,
					articles=c(390,1038,1540,1933,1738,1559,1811,1760,1644,
								1647,1628,1562,1254,1417,1143,1355,1029,1235,
								1097,1266,1081,1223,1285,1343,1445,1417,1275,
								1671,1579,1529,1476,1428,1554,1643,1653,1586,
								1651,1754,1815,1803,1920,1907,1818,1730,1570,
								1938,2037,1972,1900,1991,1987,1994,1973,2081,
								2261,2125,2148,2258,2320,2296,2427,2401,2255,
								2425,2434,2954,2924,3176,3132,2985,3002,2872,
								2558,2540,2286,2146,2390,2516,2605,2508,2839,
								2841,2971,3033,3069,3060,3392,3324,3226,
								3640,3281,3272,3422,3362,3729,4116,3916,3442,
								3159,3301,3159,3113,3215,3276,3083,3312,3136,3018,3674,
								3165,3380,4088,4124,4036,4455,4477,4207,4891,
								4857,1230,5167,4865,5048,4542,4373,4135,4097,
								3959,3538,3617,3969,3915,3974,3918,4226,4133,
								4115,4511,4437,4312,4367,4348,4439,4366,4667,
								4700,4642,4622,4655,4742,4751,4728,4954,4793,
								4771,4736,4895,4825,4427,4108,4035,4092,4089,
								4011,4095,4087,4141,4081,4049,4037,3980,3915,
								3719)) # This is the total number of articles in the Gale archive in the respective years. This allows calculating the percentage of articles that have any crisis keywords in them.0

theecono <- read.csv("theeconomist.csv",sep=",",header=FALSE) 
names(theecono) <- c("keyword","year","articles")

thetimes <- read.csv("thetimes.csv",sep=",",header=FALSE)
names(thetimes) <- c("keyword","year","articles")

# Creating the corpus and the various representations thereof

# Load an English vocabulary to compare "nonwords" in the texts to.

	endict <- read.csv(file="ENGVOC_lo.txt")
	endict <- as.character(endict[,1]) # ensure that the vocabulary list is represented as character vector.
	sw <- c("oclock","minut","yard","morn","half-past","dont","don't","say","did","didn't","get","want","just",
		"think","cant","cannot","theyr","wasnt","wouldnt","wasn't","wouldn't","havent","haven't","shouldn't","should not",
		"ing","tion","con","com","pro","ã‚â£","ã‚â£m","â£m","â€¢","â–º","â€œthe","britainâ€™","¬","â","â€”")
		# stopwords that are not meaningful for the topic models and should be removed from the Document term matrix for the Topic Models.
		# always remove as late as possible.

	person.names <- as.matrix(babynames[,3])
	person_names <- tolower(unique(person.names))
	surnames <- tolower(surnames2010[,1])

# Create a corpus object
	cp <- corpus(textfiles.1)

# Create tokens
	tok <- quanteda::tokens(str_to_lower(textfiles.1$text),remove_punct=TRUE,remove_symbols=TRUE,remove_numbers=FALSE,split_hyphens=TRUE,remove_separators=TRUE)


	tok2 <- tokens_replace(tok,pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma,valuetype="fixed")
#	tok <- tokens_replace(quanteda::tokens(str_to_lower(textfiles.1$text),remove_punct=TRUE,remove_symbols=TRUE,remove_numbers=FALSE,split_hyphens=TRUE,remove_separators=TRUE),pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma) # Lemmatizes the text. This is superior to stemming in being sensitive to the language's grammar, e.g. irregular forms of conjugation/declination patterns.
	pretok <- textfiles.1$text

	fullwordlist <- c(endict,person_names,surnames)

# Substitute non-words (that are also not first names or surnames) with most similar words
# This will take several days to complete on a normal computer.

		for (j in 1:length(tok2))	# a loop that runs through all documents.
		for (j in 1:100)	# a loop that runs through all documents.
		{
		ntok <- tok2[[j]]	# saves the current text tokens under a new name to manipulate it and ease code.
		goodpos <- which(ntok%in%fullwordlist) # marker which words are recognized either as words (endict) or names (person_names,surnames).
		badpos  <- which(!(ntok%in%fullwordlist))  # marker which words are not recognized as words (endict) or names (person_names,surnames).
		good <- ntok[goodpos] # list of all words that are unproblematic. Will be used later to compile a full list of all words again.
		bad <- ntok[badpos] # list of all words unrecognized that should be substituted if possible. 
		wole <- nchar(bad) # Word length of the words that are unrecognized. This is important to calculate the maximum permissible number of deviations for substitution.
		thresh <- ceiling(wole/3) # Word-specific threshold for deciding whether to substitute it for the best-fitting word in the dictionary or not.

		for (i in 1:length(bad))	# a loop that runs through all "bad" words in a document.
		{
		if (length(bad)>0) {		# condition to either proceed if there are any bad words, or jump to the next document if there are no bad words.
		stdi.raw <- stringdist(endict,ntok[badpos[i]]) # Calculates the deviation between the bad word to be substituted and all words in the dictionary of English words.
		stdi.pos <- stringdist(endict,paste0(ntok[badpos[i]],ntok[badpos[i]+1])) # The same calculation as in stdi.raw, but adds the next word to the current bad word. If there is a linebreak, a word may have been broken down into two broken words if the hyphen was not recognized.
		stdi.pre <- stringdist(endict,paste0(ntok[badpos[i]-1],ntok[badpos[i]])) # The same calculation as in stdi.raw, but adds the immediately preceding word to the current bad word. For the same logic, for the word ending up at the start rather than the end of the line.
		min.stdi.raw <- min(stdi.raw,na.rm=T) # Gives the lowest observed raw deviation.
		min.stdi.pos <- min(stdi.pos,na.rm=T) # Gives the lowest observed deviation when combined with the next word.
		min.stdi.pre <- min(stdi.pre,na.rm=T) # Gives the lowest observed deviation when combined with the previous word.
		stdi <- if(min.stdi.raw<=min.stdi.pos & min.stdi.raw<=min.stdi.pre) {stdi.raw} else 
					if (min.stdi.pos<=min.stdi.pre) {stdi.pos} else 
					{stdi.pre} # choose the stdi list with the lowest deviation
		
		# stdi <- cbind(stdi.raw,stdi.pos,stdi.pre)[,which(c(min.stdi.raw,min.stdi.pos,min.stdi.pre)==min(c(min.stdi.raw,min.stdi.pos,min.stdi.pre)))[1]]
		loc <- which(stdi==min(stdi)) # Position of the lowest values in stdi (maybe several)
		locsim <- loc[which(nchar(endict[loc])==wole[i])] # Position of the lowest values in stdi that have the same word length as the target word (maybe several)
		if(length(locsim)>0){ # is there any location with similar word length as original word?
			if(stdi[locsim][1]<=thresh[i]){ # lower than threshold?
			ntok[badpos[i]] <- sample(x=endict[locsim],size=1)}} else # draw a sample of one from the set of words that have the same length and fit optimally (best fitters of same length)
		if(length(loc)>0){ # is there any location?
			if(stdi[loc][1]<=thresh[i]){ # lower than threshold?
			ntok[badpos[i]] <- sample(x=endict[loc],size=1)}} # draw a sample of one from the set of words that fit just as good (best fitters)
		} # note that the result from loc will be overwritten with the result from locsim if locsim >0
		}
		pretok[j] <- paste(ntok,collapse=" ")
		print(j)
		flush.console()
		}

tok3 <- quanteda::tokens(pretok)

textfiles.2 <- textfiles

textfiles.2$text <- pretok

# Create a corpus object
	# cp <- corpus(textfiles)
	cp <- corpus(textfiles.2)

# Create a Document-Term-Matrix / Document-Feature-Matrix
	# dtm <- dfm(cp,tolower=T,stem=T,remove_punct=T,remove=stopwords("english"))
	dtm <- dfm(cp,tolower=T,stem=F,remove_punct=T,remove=c(stopwords("english"),sw))

save(dtm,file="wDTM.RData")
