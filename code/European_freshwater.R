
## Database for Tonda
setwd("C:/Users/Propietario/Downloads")
data <- read.csv("European_Red_List_2017_December.csv")
names(data)

## Goal: create a database for all aquatic invasive species in Europe
# possibly with filtering for marine, brackish and freshwater habitats?

hanno <- read_xlsx("GlobalAlienSpeciesFirstRecordDatabase_v3.1_freedata.xlsx", sheet = "FirstRecords")
names(hanno)
unique(hanno$Region)
eu <- c("Aland Islands","Åland Islands","Albania","Andorra","Austria","Azores","Bahrain","Balearic Islands",
        "Belarus","Belgium","Belgium, France, Netherlands, Uk","Bosnia and Herzegovina","Bulgaria","Canary Islands",
        "Crete","Croatia", "Cyprus","Czech Republic","Denmark","Estonia","Falkland Islands","Faroe Islands",
        "Finland","France", "France, Turkey","Germany","Germany and France","Germany and Spain","Gibraltar",
        "Greece","Hungary","Iceland", "Ireland","Italy","Italy and Germany","Italy, Hungary, Spain",
        "Latvia","Liechtenstein","Lithuania","Macedonia","Madeira","Malta","Moldova","Monaco","Montenegro",
        "Netherlands","Norway","Poland","Portugal","Romania","San Marino","Serbia","Sicily","Slovakia","Slovenia",
        "Spain","Spain, France, Hungary","Svalbard and Jan Mayen","Sweden","Switzerland","Turkey",
        "Uk and Netherlands","Ukraine","United Kingdom")

hanno <- hanno %>% filter(Region %in% eu)


hanno <- hanno[,-c(13:17)]
str(hanno)

hanno1 <- hanno  %>%
  filter((!is.na(Habitat_freshwater) & Habitat_freshwater == 1) |
          (!is.na(Habitat_marine) & Habitat_marine == 1))
hanno1 <- hanno1[,-13]

hanno1 <- hanno1[,c(2,3,4,5,9:12,1,6:8,13,14)]


### Red list ####
setwd("C:/Users/Propietario/Downloads")
file_list <- list.files(pattern = "Red_List.*\\.xls$")

data_list <- lapply(file_list, read_excel)

all_columns <- unique(unlist(lapply(data_list, names)))


data_list <- Map(function(df, file_name) {
  missing_cols <- setdiff(all_columns, names(df))
  df[missing_cols] <- NA
  df$SourceFile <- file_name
  return(df)
}, data_list, file_list)

comb_data <- bind_rows(data_list)

unique(comb_data$SourceFile)
names(comb_data)

comb_data$species2 <- paste0(comb_data$Genus, " ", comb_data$Species)

# filter the columns i want
names(comb_data)
comb_data <- comb_data[,-c(1:9,18,19,23:33)]
colnames(comb_data)[13] = "TaxonName"

comb_data2 = hanno1  %>% left_join(comb_data, by= "TaxonName")
names(comb_data2)



#### 100 Worst sp in Europe 

invasive_species <- data.frame(
  Species = c(
    "Branta canadensis",
    "Rattus norvegicus",
    "Procambarus clarkii",
    "Ondatra zibethicus",
    "Varroa destructor",
    "Acacia dealbata",
    "Lantana camara",
    "Cervus nippon",
    "Muntiacus reevesi",
    "Pueraria lobata var. montana",
    "Eichhornia crassipes",
    "Eriocheir sinensis",
    "Robinia pseudoacacia",
    "Procambarus fallax",
    "Acripeza toralis tristis",
    "Sciurus carolinensis",
    "Myocastor coypus",
    "Hymenoscyphus pseudoalbidus",
    "Neovison vison",
    "Carassius auratus",
    "Cortaderia selloana",
    "Heracleum mantegazzianum",
    "Heracleum persicum",
    "Heracleum sosnowskyi",
    "Dreissena polymorpha",
    "Elodea canadensis",
    "Procyon lotor",
    "Phytophthora pluviora",
    "Pheidole megacephala",
    "Crassula helmsii",
    "Opisthoteuthis agassizii",
    "Anoplophora chinensis",
    "Ambrosia artemisiifolia",
    "Axis axis",
    "Corvus splendens",
    "Phytophthora alni",
    "Parthenium hysterophorus",
    "Oreochromis mossambicus",
    "Seridium cardinale",
    "Castor canadensis",
    "Fallopia japonica",
    "Opuntia ficus-indica",
    "Saperda candida",
    "Pomacea canaliculata",
    "Siganus luridus",
    "Linepithema humile",
    "Arundo donax",
    "Potamopyrgus antipodarum",
    "Pacifastacus leniusculus",
    "Hydrocotyle ranunculoides",
    "Ficopomatus enigmaticus",
    "Mnemiopsis leidyi",
    "Phytophthora cinnamomi",
    "Ludwigia grandiflora",
    "Ludwigia peploides",
    "Azolla filiculoides",
    "Lupinus polyphyllus",
    "Rhopilema nomadica",
    "Aethina tumida",
    "Oreochromis niloticus",
    "Cherax quadricarinatus",
    "Eucalyptus globulus",
    "Alternanthera philoxeroides",
    "Caulerpa racemosa",
    "Lithobates catesbeianus",
    "Rapana venosa",
    "Siganus rivulatus",
    "Bursaphelenchus xylophilus",
    "Sicyos angulatus",
    "Paralithodes camtschaticus",
    "Oreochromis aureus",
    "Ammotragus lervia",
    "Threskiornis aethiopicus",
    "Caulerpa taxifolia",
    "Anoplophora glabripennis",
    "Paysandisia archon",
    "Pomacea maculata",
    "Aedes albopictus",
    "Baccharis halimifolia",
    "Harmonia axyridis",
    "Prunus serotina",
    "Pseudorasbora parva",
    "Senecio mikanioides",
    "Solanum elaeagnifolium",
    "Solidago canadensis",
    "Cyclamen persicum",
    "Oncorhynchus mykiss",
    "Micropterus dolomieu",
    "Cherax destructor",
    "Dikerogammarus villosus",
    "Cabomba caroliniana",
    "Callosciurus finlaysonii",
    "Artemisia californica",
    "Balanus improvisus",
    "Ctenopharyngodon idella",
    "Eucalyptus camaldulensis",
    "Odocoileus virginianus",
    "Tradescantia fluminensis",
    "Frankliniella occidentalis",
    "Nasua nasua",
    "Nyctereutes procyonoides",
    "Ambrosia trifida",
    "Eleagnus angustifolia",
    "Pistia stratiotes",
    "Psittacula krameri",
    "Tamias sibiricus",
    "Orconectes virilis",
    "Spartina anglica",
    "Acacia saligna",
    "Pancyhthus citri",
    "Carpobrotus acinaciformis",
    "Cotula coronopifolia",
    "Sparganium erectum",
    "Xenopus laevis",
    "Carpodrotus edulis",
    "Tuta absoluta",
    "Homarus americanus",
    "Marisa cornuarietis",
    "Saurida undosquamis",
    "Crassostrea gigas",
    "Alexandrium catenella",
    "Alexandrium catenella",
    "Ligustrum sinense",
    "Poecilia reticulata",
    "Rosa rugosa",
    "Campylopus introflexus",
    "Hedychium gardnerianum",
    "Ovis orientalis",
    "Acacia longifolia",
    "Buddleja davidii",
    "Gambusia holbrooki",
    "Grapholita molesta",
    "Herpestes javanicus",
    "Liriomyza trifolii",
    "Tilapia zillii",
    "Anguillicola crassus",
    "Aedes aegypti",
   "Corbicula fluminea",
    "Ehrharta calycina",
    "Anthonomus grandis",
    "Eonomus fortunei",
    "Orconectes limosus",
    "Oxyura jamaicensis",
    "Bonnemaisonia hamifera",
    "Globodera pallida",
    "Globodera rostochiensis",
    "Helicoverpa armigera",
    "Meloidogyne chitwoodi",
    "Meloidogyne fallax",
    "Opogona sacchari"
  )
)


comb_data2$Worst_Invasive_EU <- ifelse(comb_data2$TaxonName %in% invasive_species$Species, "Yes", "No")


### List of 88 European concern  ####
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_invasive_alien_species_of_Union_concern"

page <- read_html(url) 

table <- page %>%
  html_table(fill = TRUE) %>%
  .[[1]] 


comb_data2$EU_concern <- ifelse(comb_data2$TaxonName %in% table$`Scientific name`, "Yes", "No")


write_xlsx(comb_data2, "frehswater_invadersv1.0.xlsx")




