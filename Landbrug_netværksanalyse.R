# SOCIAL NETVÆRKSANALYSE AF LANDBRUGSSEKTOREN 

# Der gøres i løbet af dette script brug af data indsamlet af Christoph Ellesgaard, Anton Grau Larsen og Sarah Steinitz i 2017.
# Der bruges også kode stillet til rådighed for at analyse dataen. 
# Læs mere om dataens sammensætninger og evt. metodiske udfordringer i metodeafsnittet. 

# Ryder hukommelse. Sikrer at data ikke blandes med evt. tidligere materiale
rm(list = ls()) 

# Installerer de relevante pakker. Kør kun ved første gennemgang. 
install.packages("tidyverse")
install.packages("igraph")
install.packages("rio")
install.packages("readxl")
install.packages("Matrix")
install.packages("knitr")
install.packages("RColorBrewer")
install.packages("ggthemes")
install.packages("scales")
install.packages("ggrepel")

# Kører specifik elite pakke
install.packages("devtools")
library(devtools)
install_github("antongrau/eliter")
data(den)

# Kører de relevante ekstra pakker
library(tidyverse) # Generel pakke som bl.a. indlæser data.
library(igraph) # Underliggende pakke for eliter
library(rio) # Anden import og export af data 
library(readxl) # Læsning af excelfiler.
library(eliter) # Pakke udviklet af Ellesgaard og Grau baseret på igraph til at foretage social netværksanalyse med deres metodiske betingelser.
library(Matrix) # Udvidede tabeller
library(knitr) # Hjælp til overblik af input og output
library(RColorBrewer) # Farver til plots
library(ggthemes) # Plots
library(scales) # Plots
library(ggrepel) # Plots

# Angiver working directory. Mappe hvor data pr. standard hentes fra. Hvis det køres fra anden computer, så sæt til spefikke mappe.
setwd("ingridelisabeththuesen/netværksanalyse") 

# Importerer data fra github
download.file("https://raw.githubusercontent.com/antongrau/eliter/master/raw_data/den17.csv",
              "data/2017data.csv", mode = "wb")

# Indlæser datafilen importeret fra github. Kører med csv2, da filen er adskilt af semikolon.   
data_1 <- read_csv2(file="data/2017data.csv")

# Får et overblik over variablene i data.
names(data_1)

# Kigger på de unikke værdier i TAGS variablen for at finde de relevante tags til at skille landbrugssektoren ud. 
# Vi er meget inklusive i den proces, så der ikke ekskluderes nogen. 
# Helt konkret slår vi også relevante organisationer i klimapartnerskabet op, og inkluderer deres tags samt kigger samtlige tags igennem for relevans.
data_1 %>% select(TAGS) %>% unique()

# Det ses også at nogle observationer er tagget med N/A, og derfor ikke fanges ikke af de andre tags.
# Der laves et datasæt med alle, som er tagget med N/A.
na_1 <- data_1 %>% filter(is.na(TAGS))
export(na_1, "data/na_1.xlsx")

# N/A datasættet med 2040 observationer gennemgås for relevante observationer i excel, og de tagges med "select".
# Det er bl.a. SEGES og enkelte Danish Crown positioner, som ikke er tagget i det oprindelige datasæt. 
# Redigeringen foretages i excel, da datasæt kan redigeres nemmere der end i R. N/A datasættet vedhæftes opgaven.
# Genindlæser N/A datasæt.
na_2 <- read_excel("data/na_1.xlsx")

# Nu kodes de relevante positioner fra N/A datasættet om til "select" i det endelige datasæt. 
na_3 <- na_2 %>% filter(!is.na(TAGS)) %>% pull(POSITION_ID)
data_1[data_1$POSITION_ID %in% na_3,]$TAGS <- "select"

# Anders Bering indegår også i klimapartnerkabet, men hans netværk er ikke tagget med vores udvalgte tags. 
# Derfor tages hans position manuelt. 
manuelle_numre <- c(119367)
data_1[data_1$POSITION_ID %in% manuelle_numre,]$TAGS <- "select"

# Skiller alle uden for landbrugsektoren samt miljøorganisationer fra ved hjælp af de forskellige tags indlejeret i data. 
# Select tag har vi selv tilført til relevante positioner for sektoren, som ikke  fremgik med nogle tags eller relevante tags i datasættet.
data_2 <- data_1 %>% filter(str_detect(TAGS, "Food|Fishing|Forestry|Ministry of Industry|Ministry of Energy|Ministry of the Environment and Food|Animals|Farming|Nature|Environment|Energy|Rural|Utilities and Climate|select")) %>% 
  select(PERSON_ID) 

# Sammensætter med det oprindelige datasæt på person id, så hele netværket for relevante personer er inkluderet.
# Det er nemlig muligt at personer inden for sektoren mødes uden for klassiske landbrugfora. 
data_3 <- data_1 %>% semi_join(data_2)

# Laver forløbeligt netværk over de relevante personer
network_1 <- elite.network(data_3)

# Får de interne forbindelser og vægten af disse mellem relevante aktører
connections <- as.data.frame(get.edgelist(network_1)) %>% 
  cbind(vægt = edge.attributes(network_1)$weight) %>% view

# Sorterer personer med kun et medlemskab fra samt personer hvor det vægtede medlemskab er mindre end 1. 
relevant_persons <- get.vertex.attribute(network_1) %>% as.data.frame() %>%
  filter(memberships>1) %>% 
  filter(weighted.memberships>2) %>% 
  select(name)

# Samler datasættet med det tidligere data, så personer få positioner er sorteret fra.  
data_final <- data_3 %>% 
  semi_join(relevant_persons, by=c("NAME" = "name"))


# Omformer den endelige df til den format, som bruges fremadrettet i scriptet.
den <- data_final
den <- den %>% filter(!str_detect(TAGS, "Events")) #Sorterer events fra, da de forvirrer data, da der ikke tages højde for den interne vægtning af positioner
kill   <- c("DLG - Repr√¶sentantskab (Medlemmer)") #Fjerner store fora med mange medlemmer af samme årsag.
den    <- den %>% filter(!AFFILIATION %in% kill)
den    <- as.den(den)

# df med navn og poster på hhv. rækker og kolonner
incidence <- xtabs(~NAME + AFFILIATION, den, sparse = TRUE)

# Sikrer sig at individer med mindre end 3 poster er væk, nu når df er fuldt. Laver også forslag på kerner i netværket. 
incidence@x[] <- 1
circle.solution <- k.circles(incidence, 3, check.for.nested = FALSE) # Holder ikke op mod oprindelige datasæt, da jeg er sikrer på data kommer fra Ellesgaard og Grau Larsen

# Laver df for organisationer og individer med niveauet af positioner
lev.solution <- level.membership(circle.solution, mode = "two-mode")

# Laver funktion for at summere k scorer for organisationer
sum.of.k.scores <- function(k, drop.last = TRUE){
  seq <- seq_along(k)
  if(identical(drop.last, TRUE)) seq <- seq[-length(seq)]
  
  m <- level.membership(k, "two-mode", levels = seq) %>% filter(type == 1)
  i <- k[[1]] 
  stopifnot(all.equal(rownames(i), m$Name))
  il <- i * (m$Level - 1)
  cs <- colSums(il) - colSums(i)
  cs
}

# Laver funktion for at summere k scorer for individer
sum.of.k.scores.members <- function(k, drop.last = TRUE){
  seq <- seq_along(k)
  if(identical(drop.last, TRUE)) seq <- seq[-length(seq)]
  
  m <- level.membership(k, "two-mode", levels = seq) %>% filter(type == 2)
  i <- k[[1]] 
  
  stopifnot(all.equal(colnames(i), m$Name))
  il <- t(i) * (m$Level - 1)
  il <- t(il)
  rs <- rowSums(il)
  rs
}

# Opsumerer for organisationer
soks.affil  <- sum.of.k.scores(circle.solution) %>% enframe()

# Opsumerer for individer
soks.member <- sum.of.k.scores.members(circle.solution) %>% enframe()

# Tabel for hvor mange indgår på hver level værdi
table(lev.solution$Level, lev.solution$type)

# Laver sektor inddeling
sector.tags <- list()
sector.tags$"Erhvervsliv" <- c("Corporation")
sector.tags$"Erhvervsorganisationer" <- c("Business association", "Employers association")
sector.tags$"Fagbevægelse" <- c( "Unions", "Standsforening", "A-kasse", "Union controlled")
sector.tags$"Politik"      <- c("Politics", "Parliament", "Political party")
sector.tags$"Stat"         <- c("State administration", "Ministry", "State corporation", "Military", 
                                "Public leaders", "Commission", "Politics", "Parliament", "State business")
sector.tags$"Videnskab og uddannelse" <- c("Science", "Education", "Universities")

# Sætter farver på hver sektor
sektor.pal <- c("Erhvervsliv" = "PuBu", "Fagbevægelse" = "Reds", "Videnskab og uddannelse" = "Greens", 
                "Erhvervsorganisationer" = "BuPu" , "Stat" = "RdPu", "Politik" = "Greys") 

# Putter farveskala på. Farveskala med farver fra 1 til 9 til at vise levels
l.colors  <- lapply(sektor.pal, brewer.pal, n = 9)

# Sætter intensitet for farver
intensity <- 5

# Sætter farver på hver sektor
fill.scale <- c("Erhvervsliv" = l.colors$Erhvervsliv[intensity],
                "Fagbevægelse" = l.colors$Fagbevægelse[intensity + 2],
                "Videnskab og uddannelse" = l.colors$`Videnskab og uddannelse`[intensity - 1],
                "Erhvervsorganisationer" = l.colors$Erhvervsorganisationer[intensity - 2],
                "Stat" = l.colors$Stat[intensity],
                "Politik" = l.colors$Politik[intensity + 2])

# Piller uklare farver ud
disc.scale <- brewer.pal(9, "YlGnBu")
dic.scale <- c(disc.scale[1], disc.scale[length(disc.scale)])
disc.scale.3 <- disc.scale[c(1, 5, 9)]

# Putter sektorer på for positioner
affil.sector       <- tags.to.sectors(den, sector.tags = sector.tags, 
                                      sector.membership = TRUE, 
                                      other = "Andet", mutually.exclusive = F, 
                                      silent = F)

# Bestemmer hvilken sektor en organisation tilhører
affil.sector       <- affil.sector %>% bind_rows(.id = "Sektor") %>% 
  as_tibble() %>% select(AFFILIATION, Sektor)  %>% 
  group_by(AFFILIATION) %>% summarise(Sektor = first(Sektor))
d                  <- affil.sector %>% select(Name = AFFILIATION, Sektor)
soks               <- bind_rows(soks.affil, soks.member) %>% rename("K-sum" = value, Name = name)

# Udvælger cirkel 5. Hvis cirkel 1 ønskes med alle 901 personer, så vælg 1 i stedet
ind <- circle.solution[[5]]

# Laver en graf for tidligere tabel og trækker navnene for grafen ud og joiner til ny df
g      <- ind %>% graph_from_incidence_matrix()
d      <- tibble(Name = V(g)$name) %>% left_join(d, by = "Name") %>% left_join(., soks, by = "Name")
d$type <- V(g)$type

# Bestemmer farve og størrelse for graf
fill <- d$Sektor
size <- d$`K-sum`
size[is.na(size)] <- min(size, na.rm = TRUE)/2

# Laver df i netværksformat
ew   <- edge.betweenness.estimate(g, cutoff = 8)
ef     <- tibble(Name = as_edgelist(g)[,2]) %>% left_join(d, by = "Name")
ec     <- ef$Sektor

# Layouts for graf
lay    <- layout_with_fr(g, weights = 1/log(ew))
lay.labels <- tibble(Name = d$Name, X=lay[,1], Y = lay[, 2])
lay.affil <- lay.labels %>% filter(d$type == TRUE) # Organisationer
lay.affil_2 <- lay.labels %>% filter(d$type == FALSE) # Individer

# Datasæt over personer i netværkskerne 
persons_network <- left_join(lay.affil_2, data_endeligt, by = c("Name" = "NAME"))

# Plot af netværk
p      <- graph.plot(g, lay, vertex.shape = V(g)$type, vertex.fill = fill, edge.color = ec,  vertex.size = size, edge.size = 0.2, edge.alpha = ew, edge.order = ew, norm.coords = FALSE, vertex.order = size)
p      <- p + scale_size_continuous(range = c(1, 6), guide = "none") + scale_alpha(range = c(0.3, 1), guide = "none")
p      <- p + scale_fill_manual(values = fill.scale,  aesthetics = c("color", "fill"), drop = FALSE)
p      <- p + scale_shape_manual(values = c(4, 21), guide = "none") 
p      <- p + guides(fill = guide_legend(override.aes = list(shape = 21, size = 2)))
p      <- p + coord_equal() 
p 

# Organisationer og individer
p + geom_text(data = lay.labels, aes(x = X, y = Y, label = Name), size=2, family = "Times New Roman", check_overlap = T)

# Organisationer
p + geom_text(data = lay.affil, aes(x = X, y = Y, label = Name), size=2, family = "Times New Roman", check_overlap = T)

# Individer
p + geom_text(data = lay.affil_2, aes(x = X, y = Y, label = Name), size=2, family = "Times New Roman", check_overlap = T)

# Plot hvor udvalgte individer kommer på
p + geom_text(data = lay.affil_2, aes(x = X, y = Y, label=Name), size=0, check_overlap = T)
