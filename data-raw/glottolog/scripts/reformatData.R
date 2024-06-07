# ===== Glottolog data

# read downloaded data
g <- read.csv("sources/glottolog/languoid.csv", as.is = T)
g <- g[,c("pk","father_pk","family_pk","name","id","hid","level","longitude","latitude")]

# remove bookkeeping
g <- g[-which(g$family_pk == 107384),]
g <- g[-which(g$pk == 107384),]

# remove untattested
g <- g[-which(g$family_pk == 107392),]
g <- g[-which(g$pk == 107392),]

# remove unclassifiable
g <- g[-which(g$family_pk == 107393),]
g <- g[-which(g$pk == 107393),]

# apostrophe is leading to errors in some libraries later on
g$name <- gsub("'","’",g$name)

# make names unique
dupl <- ( duplicated(g$name) | rev(duplicated(rev(g$name))) ) & g$level != "language"
g$name[dupl] <- paste0(g$name[dupl], " [", g$id[dupl], "]")

# remove "NOCODE" iso codes
g$hid <- gsub("NOCODE_.+$", "", g$hid)

# substitute ids
tmp <- g$name
names(tmp) <- g$pk
g$father_pk <- tmp[as.character(g$father_pk)]
g$family_pk <- tmp[as.character(g$family_pk)]

# add world
g$father_pk[is.na(g$father_pk)] <- "World"

# add NA for not-available iso codes
g$hid[g$hid == ""] <- NA

# fill out family column for easier separation into families
g$family_pk[is.na(g$family_pk)] <- g$name[is.na(g$family_pk)]

# clean structure
rownames(g) <- g$id
g <- g[,c(4,2,3,5,6,7,8,9)]
colnames(g) <- c("name", "father", "stock", "glottocode","iso", "level", "longitude", "latitude")

# replace round brackets, because they interfere with newick format
g$name <- gsub("\\(","[",g$name)
g$name <- gsub("\\)","]",g$name)
g$father <- gsub("\\(","[",g$father)
g$father <- gsub("\\)","]",g$father)
g$stock <- gsub("\\(","[",g$stock)
g$stock <- gsub("\\)","]",g$stock)

# remove Artificial and Sign languages, Speech Register

g <- g[g$stock != "Artificial Language",]
g <- g[g$stock != "Sign Language",]
g <- g[g$stock != "Speech Register",]

# ===== add Areas

fams <- unique(g$stock)
coords <- t(sapply(fams, function(x){
	colMeans(g[g$stock == x, 7:8], na.rm=T)
}))
area <- rep("Eurasia", length = length(fams))
area[coords[,1] > 115 & coords[,2] < 12] <- "Sahul"
area[coords[,1] < 115 & coords[,1] > 60 & coords[,2] < 35] <- "Southeast Asia"
area[coords[,1] < 60 & coords[,1] > -25 & coords[,2] < 25] <- "Africa"
area[coords[,1] < -25 & coords[,2] < 12] <- "South America"
area[coords[,1] < -25 & coords[,2] > 12] <- "North America"
area[fams == "Austronesian"] <- "Southeast Asia"
area[fams == "Mixed Language"] <- "World"

for (i in seq_along(fams)) {
	g[g$name == fams[i], "father"] <- area[i]
}

add <- cbind(unique(area)[-7],"World", NA, NA, NA, "area", NA, NA)
colnames(add) <- colnames(g)
g <- rbind(g, add)
rownames(g)[(nrow(g)-5):nrow(g)] <- paste0("area000",1:6)

# ===== add WALS codes

w <- read.csv("sources/wals/language.csv", as.is = T)

# note that 20 WALS codes are missing links to iso
# only a few are manually added here
w[w$wals_code=="aab","glottocode"] <- "abua1245"
w[w$wals_code=="ayo","glottocode"] <- "jira1235"
w[w$wals_code=="bfg","glottocode"] <- "tazn1238"
w[w$wals_code=="joh","glottocode"] <- "kuma1273"

# duplicate glottocode are reduced to entity with most information in WALS
# this is not perfect, but workeable
# note that many cases seem solveable with more in-depth comparison of the sources
dupl <- unique(w$glottocode[which(duplicated(w$glottocode) & w$glottocode != "")])
id_remove <- c()
for (i in dupl) {
	missing <- apply(w[w$glottocode==i,],1,function(x){sum(x=="")})
	worst <- rownames(w[w$glottocode==i,])[-which(missing == min(missing))]
	id_remove <- c(id_remove, worst)
}
w <- w[!(rownames(w) %in% id_remove),]

match <- w$wals_code
names(match) <- w$glottocode
g <- cbind(g, wals = match[rownames(g)])
g <- g[,c(1:5,9,6:8)]

# ===== Speaker numbers

# adding population statistics from the 13th Edition Ethnologue
# note that iso codes have changed, so linking is not perfect
e <- read.delim("sources/ethnologue/ethnologue.txt")

# remove duplicates
e <- e[!duplicated(e[,c(2,5)]),]
# merge (note that the R:merge function gives different sort order)
match <- e$POPULATION
names(match) <- e$"ISO639.3"
g <- cbind(g, population = match[g$iso])

# ===== Macrolanguages

# adding macrolanguages from iso 639-3
macro <- read.delim("sources/sil/macrolanguages.txt", as.is = T)
avail <- macro$ID %in% g$iso
name_avail <- macro[!avail,"NAME"] %in% g$name
names <- macro[!avail,"NAME"][name_avail]
for (i in names) {
	if (is.na(g[g$name==i,"iso"])) {
		g[g$name==i,"iso"] <- macro[macro$NAME==i,"ID"]
	}
}

# a few manual additions that seem easy to map
g[g$name=="Swahili (G.40)","iso"] <- "swa"
g[g$name=="Nuclear Malayic","iso"] <- "msa"
g[g$name=="Indo-Aryan Northern zone-Western Pahari","iso"] <- "doi"
g[g$name=="Konkanic","iso"] <- "kok"
g[g$name=="Fante","iso"] <- "fat"
g[g$name=="Asante","iso"] <- "twi"

# code changes
g[g$name=="Maket","iso"] <- "bjp"
g[g$name=="Anir","iso"] <- "hrc"
g[g$name=="Tanga","iso"] <- "hrw"

# retired codes
g[which(g$iso=="daf"),"iso"] <- "dnj"
g[which(g$iso=="bkb"),"iso"] <- "ebk"

# missing in download?
g$wals <- as.character(g$wals)
norw1259 <- c("Norwegian Bokmål", "Norwegian", "Indo-European", "norw1259", "nob", "nor", "dialect", NA, NA, NA)
norw1262 <- c("Norwegian Nynorsk", "Norwegian", "Indo-European", "norw1262", "nno", NA, "dialect", NA, NA, NA)
g <- rbind(g, norw1259 = norw1259, norw1262 = norw1262)

# ====== recent code change

g["para1306","population"] <- g["para1320","population"]
g["para1320",8:10] <- NA

# ====== classes got mixed up

g$stock <- as.factor(g$stock)
g$wals <- as.character(g$wals)
g$level <- as.factor(g$level)
g$longitude <- as.numeric(g$longitude)
g$latitude <- as.numeric(g$latitude)
g$population <- as.numeric(g$population)

# ====== duplicated coordinates

# slighly nudge duplicated coordinates
dupl <- which(duplicated(g[,8:9]) & !is.na(g[,8:9][,1]))
while (length(dupl > 0)) {
	g[dupl,8:9] <- g[dupl,8:9] + 0.01
	dupl <- which(duplicated(g[,8:9]) & !is.na(g[,8:9][,1]))
}

# ======= Encoding

Encoding(g$name) <- "UTF-8"
Encoding(g$father) <- "UTF-8"
Encoding(levels(g$stock)) <- "UTF-8"


# ======= save result

glottolog <- g
write.csv(glottolog, file = "data/glottolog.csv")
save(glottolog, file = "data/glottolog.Rdata", compress = "xz")
