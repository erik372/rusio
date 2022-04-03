rm(list=ls())
library("tidyverse")
library("writexl")
library("readxl")
library("patchwork")
library("ggrepel")
library("conflicted")
conflict_prefer("filter","dplyr")
conflict_prefer("lag","dplyr")

options(scipen=999)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
resultatkat <- getwd()
rawkat <- "C:/Users/hegel/Dropbox/_MINA TEXTER _db/OECD ICIO projekt/ICIO raw"

setwd(resultatkat)
industry_labels <- read_xlsx("bransch och kod.xlsx") 
country_labels_df <- read_xlsx("landskoder.xlsx")

################################################################################
# funktion för att korrigera saknade värden osv
################################################################################
fixmatrix <- function(matrixname)
{
  matrixname <- matrixname %>% replace_na(0)
  matrixname[is.na(matrixname)] <- 0 ; matrixname[is.nan(matrixname)] <- 0
  matrixname[matrixname==Inf] <- 0 ; matrixname[matrixname==-Inf] <- 0
  return(matrixname)
}
################################################################################



################################################################################
### Ladda ICIO data
################################################################################
setwd(rawkat)
# VA
load("ICIO2021econVA.Rdata")
# GO
load("ICIO2021econX - GO.Rdata")
# FD
load("ICIO2021econFD.Rdata")
# Z = flowtable
load("ICIO2021econZ.Rdata")
# L = (I-A)^-1 , Leontief inverse
#load("ICIO2021econB - Leontief.Rdata")


################################################################################
comparison_countries <- c("RUS","USA","GBR","DEU","FRA","ITA","CHN","IND","SWE")

comparison_country_labels <- c("Ryssland","USA","Storbritannien","Tyskland","Frankrike","Italien","Kina","Indien","Sverige")

manufacture_industries <- c("10T12", "13T15", "16", "17T18", "19","20", "21", "22", "23", "24", "25","26", "27", "28","29","30","31T33")

service_industries <- c("45T47","49","50","51","52","53","55T56","58T60","61","62T63","64T66","68","69T75","77T82","84","85","86T88","90T93","94T96","97T98")


#excl_countries <- c("CN1","CN2","MX1","MX2") 
excl_countries <- "NULL"
all_countries <- colnames(ICIO2021econVA) %>% 
  as_tibble() %>% 
  mutate(cty = str_sub(value, 1,3)) %>% 
  distinct(cty) %>%
  # filter some
  filter(!cty %in% excl_countries) %>% 
  pull()  # pull = dplyr function similar to $

### sätt dessa länder som en sträng
# excl_countries <- c("CN1","CN2","MX1","MX2") %>% str_c(collapse="|")






################################################################################
## Rysk ekonomi i allmänhet
## jämför följande
the_year <- 2019

cty_label_df <- tibble(iso3c=comparison_countries, comparison_country_labels)
setwd(resultatkat)
################################################################################
library("wbstats")

gdp_ppp_usd <- wb_data(
  # GDP PPP, constant 2017 USD
  c("NY.GDP.MKTP.PP.KD",
  # samma sak men per capita
  "NY.GDP.PCAP.PP.KD"), start_date = 2000, end_date= 2022) %>% 
  rename(gdp_ppp_usd = NY.GDP.MKTP.PP.KD, 
         gdp_capita_ppp_usd = NY.GDP.PCAP.PP.KD) %>% 
  filter(date==the_year, 
         iso3c %in% comparison_countries) %>% 
  left_join(cty_label_df) 
  

gdp_ppp_usd %>% 
  mutate(gdp_ppp_usd = gdp_ppp_usd/100000)
 
library("RColorBrewer")
th <- theme(legend.position = 'none', 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=10)) 
gc <-   geom_col()
my_colors <- brewer.pal( length(comparison_country_labels), "Set3" )
names(my_colors) <- levels(factor(comparison_country_labels))
sfm <- scale_fill_manual(name="land", values=my_colors)


g1 <- gdp_ppp_usd %>% 
    ggplot(aes(x=reorder(comparison_country_labels, gdp_ppp_usd), 
               fill=comparison_country_labels, 
               y=gdp_ppp_usd /1000000000000 )) + 
  labs(x=NULL, 
       y="1000 mdr USD PPP,\n2017 års priser", 
       title="BNP") +
  th + gc + sfm +
  geom_text(data = gdp_ppp_usd %>% filter(!iso3c %in% c("IND","USA","CHN")), 
            aes(label = comparison_country_labels, 
                y = gdp_ppp_usd /1000000000000 + .2), angle=90, hjust=0) +
  geom_text(data = gdp_ppp_usd %>% filter(iso3c %in% c("IND","USA","CHN")), 
            aes(label = comparison_country_labels, 
                y = gdp_ppp_usd /1000000000000 - .3), angle=90, hjust=1)

g2 <- gdp_ppp_usd %>% 
  ggplot(aes(x=reorder(comparison_country_labels, gdp_capita_ppp_usd), 
             fill=comparison_country_labels, 
             y=gdp_capita_ppp_usd /1000 )) +
  labs(x=NULL, y="1000 USD PPP,\n2017 års priser", 
       title="BNP per person") +
  th + gc + sfm +
  geom_text(data = gdp_ppp_usd %>% filter(!iso3c %in% c("IND")), 
            aes(label = comparison_country_labels, 
                y = gdp_capita_ppp_usd /1000 - .5), angle=90, hjust=1) +
  geom_text(data = gdp_ppp_usd %>% filter(iso3c %in% c("IND")), 
            aes(label = comparison_country_labels, 
                y = gdp_capita_ppp_usd /1000 + .5), angle=90, hjust=0)

(g1 + g2)
(g1 + g2) %>% 
  ggsave(filename="g_gdp_comparison.pdf", height=4, width=7)




################################################################################
### Ryssland, per bransch 2018 ICIO
# insatsimport 
# total export per bransch
################################################################################
this_country <- "RUS"
this_year <- 24  # 2018

# insatsimport till Ryssland 
df1 <- ICIO2021econZ[this_year, 
                     # avsändare = andra länder, 
                     rownames(ICIO2021econZ[this_year,,]) %>% str_subset( this_country, negate=TRUE),
                     # mottagare = Ryssland
                     colnames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country)] %>% 
  as_tibble(rownames="countryrow") %>% 
  mutate(industry_code = substr(countryrow, 5,10)) %>% 
  group_by(industry_code) %>% 
  relocate(industry_code) %>% 
  select(-countryrow) %>% 
  
  # sum col för insatsimport per sektor
  mutate( across( starts_with(this_country),  sum )) %>%      
  filter(row_number()==1) %>% ungroup %>% 
  
  # sum col 
  select(-industry_code) %>% 
  colSums(na.rm=TRUE) %>% 
  
  as_tibble(rownames="countryindustry") %>% mutate(industry_code = substr(countryindustry, 5,10)) 




# import df
imp_df <- df1 %>% 
  slice_max(value, n=15) %>% 
  left_join(industry_labels)

g_inputimport_RUS <- imp_df %>% 
  ggplot(aes(y=reorder(industry, 
                       value), 
             x=value/1000,
             fill=value)) + 
  geom_col() + 
  scale_fill_gradient2() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        text=element_text(size=8), 
        legend.position = "none") +
  labs(y=NULL, 
       x="Mdr USD", 
       title="Import av insatsvaror, Ryssland 2018") + 
  geom_text(data= imp_df %>% slice_max(value, n=4), 
            aes(label=industry, x=value/1000 -.2), hjust=1, size=2.7, color="white" ) +
  geom_text(data= imp_df %>% slice_min(value, n=11), 
            aes(label=industry, x=value/1000 +.2), hjust=0, size=2.7, color="black" ) 
  

g_inputimport_RUS
# g_inputimport_RUS %>%   ggsave(filename="g_inputimport_RUS.pdf", width=6, height=3)


### total export från Ryssland
# export för slutkonsumtion 
fdexport_df <- ICIO2021econFD[this_year, 
                   # avsändare = Ryssland
                   rownames(ICIO2021econFD[this_year,,]) %>% str_subset(this_country),
                   # mottagare = andra länder, exkl CN1, CN2...
                   colnames(ICIO2021econFD[this_year,,]) %>% str_subset(this_country, negate=TRUE)] %>% 
  rowSums(na.rm=TRUE)   

# export av insatsvaror
inputexport_df <- ICIO2021econZ[this_year, 
                     # avsändare = Ryssland
                     rownames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country),
                     # mottagare = andra länder, exkl CN1, CN2...
                     colnames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country, negate=TRUE)] %>% 
  rowSums(na.rm=TRUE)

totalexport_df <- (fdexport_df + inputexport_df) %>% 
  as_tibble(rownames="countryrow") %>% 
  mutate(industry_code = substr(countryrow, 5,10)) %>% 
  left_join(industry_labels) %>% 
  
  slice_max(value, n=15)


g_totalexport_RUS <- totalexport_df %>% 

  ggplot(aes(y=reorder(industry, 
                       value), 
             x=value/1000, 
             fill=value)) + 
  geom_col() +
  scale_fill_gradient2() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        text=element_text(size=8), 
        legend.position = "none") +
  labs(y=NULL, 
       x="Mdr USD", 
       title="Export av insatsvaror, Ryssland 2018") +
  geom_text(data= totalexport_df %>% slice_max(value, n=1), 
            aes(label=industry, x=value/1000-1), hjust=1, size=2.7, color="white" ) +
  geom_text(data= totalexport_df %>% slice_min(value, n=14), 
            aes(label=industry, x=value/1000+1), hjust=0, size=2.7, color="black" ) 

  
g_totalexport_RUS

(g_inputimport_RUS + g_totalexport_RUS)
(g_inputimport_RUS + g_totalexport_RUS) %>% 
  ggsave(filename="g_rus_importexport_in_usd.pdf", height=4, width=7)

# g_inputexport_RUS %>% ggsave(filename="g_inputexport_RUS.pdf", width=6, height=3)




################################################################################
### Leontief matriser för 2018, enbart Ryssland
# mål BL och FL för ryska branscher
################################################################################
this_year <- 24   # 2018
this_country <- "RUS"
# flödestabellen
  icioZ <- ICIO2021econZ[this_year,
                       colnames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country),
                       rownames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country)]

# total produktion per bransch
  icioX <- ICIO2021econX[this_year,
                       names(ICIO2021econX[this_year,]) %>% str_subset(this_country)]

# för att beräkna A tar vi 1/total produktion.
  mat_GOinv <- (1/icioX) %>% fixmatrix
  mat_A <- icioZ %*% diag(mat_GOinv) 
# Beräkna matris L
  mat_L <- solve( diag((mat_A %>% dim)[1]) - mat_A)
  rownames(mat_L) <- colnames(mat_L) <- names(icioX)

### backward linkeage = colsum L
  cs_L <- mat_L %>% colSums(na.rm=TRUE)
### forward linkeage = rowsum L
  rs_L <- mat_L %>% rowSums(na.rm=TRUE)

bl_fl_df_russia <- cbind(cs_L, rs_L) %>% 
  as_tibble(rownames="countryrow") %>% 
  mutate(industry_code = substr(countryrow, 5,10)) %>% 
  left_join(industry_labels)

g_bl_rus <- bl_fl_df_russia %>% 
  slice_max(cs_L, n=20) %>% 
  ggplot(aes(y=reorder(industry, cs_L), x=cs_L, fill=cs_L)) + 
  geom_col() +
  scale_fill_gradient2() +
  theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        text=element_text(size=8)) +
  geom_text(aes(x=.1, label=industry), color="white", size=2.5, hjust=0) +
  labs(y=NULL, 
       x="Totalproduktion per \nefterfrågad enhet", 
       subtitle="Produktionsmultiplikator") 

g_fl_rus <- bl_fl_df_russia %>% 
  slice_max(rs_L, n=20) %>% 
  ggplot(aes(y=reorder(industry, rs_L), x=rs_L, fill=rs_L)) + 
  geom_col() +
  scale_fill_gradient2() +
  theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        text=element_text(size=8)) +
  labs(y=NULL, 
       x="Totalproduktion per \nproducerad enhet", 
       subtitle="Insatsmultiplikator") +
  geom_text(data=bl_fl_df_russia %>% slice_max(rs_L, n=20) %>% slice_max(rs_L, n=14) , 
            aes(x=.1, label=industry), color="white", size=2.5, hjust=0) +
  geom_text(data=bl_fl_df_russia %>% slice_max(rs_L, n=20) %>% slice_min(rs_L, n=6) , 
            aes(x=rs_L + .1, label=industry), color="black", size=2.5, hjust=0) 
  
(g_bl_rus + g_fl_rus)
(g_bl_rus + g_fl_rus) %>% 
  ggsave(filename = "g_fl_bl_rus.pdf", width=7, height=4)

################################################################################
### Globala matriser för 2018, Leontief osv
# mål = global VA = GVKI
################################################################################
this_year <- 24   # 2018

# flödestabellen
icioZ <- ICIO2021econZ[this_year,
                       colnames(ICIO2021econZ[this_year,,]) %>% str_subset(excl_countries, negate=TRUE),
                       rownames(ICIO2021econZ[this_year,,]) %>% str_subset(excl_countries, negate=TRUE)]

# total produktion per bransch
icioX <- ICIO2021econX[this_year,
                       names(ICIO2021econX[this_year,]) %>% str_subset(excl_countries, negate=TRUE)]

# för att beräkna A tar vi 1/total produktion.
mat_GOinv <- (1/icioX) %>% fixmatrix

mat_A <- icioZ %*% diag(mat_GOinv) 

# Beräkna matris L
mat_L <- solve( diag((mat_A %>% dim)[1]) - mat_A)
rownames(mat_L) <- colnames(mat_L) <- names(icioX)


### backward linkeage = colsum L
cs_L <- mat_L %>% colSums(na.rm=TRUE)

g_bl_world_rus <- cs_L[names(cs_L) %>% str_subset("RUS")]  %>%
  as_tibble(rownames="countryrow") %>% 
  mutate(industry_code = substr(countryrow, 5,10)) %>% 
  slice_max(value, n=20) %>%
  left_join(industry_labels) %>% 
  
  ggplot(aes(y=reorder(industry, value), 
             x=value, 
             fill=value)) + 
  geom_col(color="white") +
  geom_text(aes(label=industry, x=.1), 
            color="white", 
            hjust=0, 
            size=2.7) +
  scale_fill_gradient2() +
  labs(y=NULL, x="Produktion per \nefterfrågad enhet", 
       #title="Produktionsmultiplikator 2018", 
       subtitle="Produktionsmultiplikator.") +
  geom_vline(xintercept=1, size=.2, linetype="dashed") +
  theme(text=element_text(size=8),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none", 
        plot.title = element_text(hjust = -1)) 


#### rowsum L = forward linkage
rs_L <- mat_L %>% rowSums(na.rm=TRUE)

gdf_rsl <- rs_L[names(rs_L) %>% str_subset("RUS")] %>% 
  as_tibble(rownames="countryrow") %>% 
  mutate(industry_code = substr(countryrow, 5,10)) %>% 
  slice_max(value, n=15) %>%
  left_join(industry_labels) 
  
g_fl_world_rus <- gdf_rsl %>% 
  ggplot(aes(y=reorder(industry, 
                       value), 
             x=value, 
             fill=value)) + 
  geom_col(color="white") +
  geom_text(data= gdf_rsl %>% slice_min(value, n=13),
            aes(label=industry, x= value + .2), hjust=0, size=2.8, color="black") +
  geom_text(data= gdf_rsl %>% slice_max(value, n=2),
            aes(label=industry, x= value -.2), hjust=1, size=2.8, color="white") +
  scale_fill_gradient2() +
  labs(y=NULL,x="Produktion per \nproducerad enhet", 
       #title="Insatsmultiplikator 2018:\n",
       subtitle="Insatsmultiplikator") +
  theme(text=element_text(size=8), 
        legend.position = "none", 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = -1)) 

g_fl_world_rus


(g_bl_world_rus + g_fl_world_rus)
(g_bl_world_rus + g_fl_world_rus) %>% 
  ggsave(filename="g_blfl_world_rus.pdf", width=7, height=5)






################################################################################
### HYPOTHETICAL EXTRACTION
# beräkna världsekonomin med och utan Ryssland
# Skapa globala matriser A och L
################################################################################
rm(list=setdiff(ls(), c("rawkat", "resultatkat", "th", "excl_countries", "fixmatrix",
                        "ICIO2021econVA",
                        "ICIO2021econX",
                        "ICIO2021econZ",
                        "ICIO2021econFD")))

this_year <- 24  # 2018
# 45x45 per land
# rownames(ICIO2021econA[this_year,,]) %>% str_subset("RUS") %>% length
################################################################################
setwd(resultatkat)

### Funktion för att beräkna hypothecial extraction för valfritt land
# Använd landskod på 3 bokstäver som argument 'this_country'
my_function_hypothetical_extraction <- function(this_country) {
  
  mat_Z_with_RUS <- ICIO2021econZ[this_year,
                                  colnames(ICIO2021econZ[this_year,,]) %>% str_subset(excl_countries, negate=TRUE),
                                  rownames(ICIO2021econZ[this_year,,]) %>% str_subset(excl_countries, negate=TRUE)]
  mat_Z_excl_RUS <- ICIO2021econZ[this_year,
                                  colnames(ICIO2021econZ[this_year,,]) %>% str_subset(paste0(this_country,"|",excl_countries), negate=TRUE),
                                  rownames(ICIO2021econZ[this_year,,]) %>% str_subset(paste0(this_country,"|",excl_countries), negate=TRUE)]
  
  # total produktion per bransch
  icioX_with_RUS <- ICIO2021econX[this_year, names(ICIO2021econX[this_year,]) %>% str_subset(excl_countries, negate=TRUE)]
  icioX_excl_RUS <- ICIO2021econX[this_year, names(ICIO2021econX[this_year,]) %>% str_subset(paste0(this_country,"|",excl_countries), negate=TRUE)]
  
  
  # för att beräkna A tar vi 1/total produktion.
  mat_GOinv_with_RUS <- (1/icioX_with_RUS) %>% fixmatrix
  mat_GOinv_excl_RUS <- (1/icioX_excl_RUS) %>% fixmatrix
  
  mat_A_with_RUS <- mat_Z_with_RUS %*% diag(mat_GOinv_with_RUS) 
  mat_A_excl_RUS <- mat_Z_excl_RUS %*% diag(mat_GOinv_excl_RUS)
  
  
  # Beräkna matris L
  mat_L_with_RUS <- solve( diag((mat_A_with_RUS %>% dim)[1] ) - mat_A_with_RUS)
  mat_L_excl_RUS <- solve( diag((mat_A_excl_RUS %>% dim)[1] ) - mat_A_excl_RUS)
  
  # Hämta FD
  diag_FD_with_RUS <- ICIO2021econFD[this_year,
                                     rownames(ICIO2021econFD[this_year,,]) %>% str_subset(excl_countries, negate=TRUE),
                                     colnames(ICIO2021econFD[this_year,,]) %>% str_subset(excl_countries, negate=TRUE)] %>% 
    rowSums(na.rm=TRUE) %>% diag
  diag_FD_excl_RUS <- ICIO2021econFD[this_year,
                                     rownames(ICIO2021econFD[this_year,,]) %>% str_subset(paste0(this_country,"|",excl_countries), negate=TRUE),
                                     colnames(ICIO2021econFD[this_year,,]) %>% str_subset(paste0(this_country,"|",excl_countries), negate=TRUE)] %>% 
    rowSums(na.rm=TRUE) %>% diag
  
  # skapa matris LFD
  mat_LFD_with_RUS <- mat_L_with_RUS %*% diag_FD_with_RUS
  mat_LFD_excl_RUS <- mat_L_excl_RUS %*% diag_FD_excl_RUS
  
  
  # Hämta VA
  icioVA_with_RUS <- ICIO2021econVA[this_year,
                                    names(ICIO2021econVA[this_year,]) %>% str_subset(excl_countries, negate=TRUE)] 
  icioVA_excl_RUS <- ICIO2021econVA[this_year,
                                    names(ICIO2021econVA[this_year,]) %>% str_subset(paste0(this_country,"|",excl_countries), negate=TRUE)] 
  
  # Join VA GO och skapa diag(VA/GO)
  diag_VAGO_with_RUS <- as_tibble(icioX_with_RUS, rownames ="ind") %>% rename(GO=value) %>%  
    left_join(as_tibble(icioVA_with_RUS, rownames="ind")) %>% rename(VA=value) %>% 
    mutate(VAGO = VA/GO) %>% 
    pull(VAGO, name=ind) %>% diag
  
  rownames(diag_VAGO_with_RUS) <- colnames(diag_VAGO_with_RUS) <- names(icioX_with_RUS)
  
  diag_VAGO_excl_RUS <- as_tibble(icioX_excl_RUS, rownames ="ind") %>% rename(GO=value) %>%  
    left_join(as_tibble(icioVA_excl_RUS, rownames="ind")) %>% rename(VA=value) %>% 
    mutate(VAGO = VA/GO) %>% 
    pull(VAGO, name=ind) %>% diag
  
  rownames(diag_VAGO_excl_RUS) <- colnames(diag_VAGO_excl_RUS) <- names(icioX_excl_RUS)
  
  # matris VAGOLFD
  VAGOLFD_with_RUS <- diag_VAGO_with_RUS %*% mat_LFD_with_RUS
  VAGOLFD_excl_RUS <- diag_VAGO_excl_RUS %*% mat_LFD_excl_RUS
  
  colnames(VAGOLFD_with_RUS) <- rownames(VAGOLFD_with_RUS) <- names(icioVA_with_RUS)
  colnames(VAGOLFD_excl_RUS) <- rownames(VAGOLFD_excl_RUS) <- names(icioVA_excl_RUS)
  
  ### jämför GVKI (vertikal VA) med och utan Ryssland
  global_VVA <- VAGOLFD_with_RUS %>% colSums(na.rm=TRUE) %>% 
    as_tibble(rownames="ind") %>% mutate(country = substr(ind,1,3), industry = substr(ind,5,20)) %>% 
    rename(VVA_with = value) %>% 
    filter(country!=this_country) %>% 
    
    # join med VAGOLFD_excl colsums
    left_join(
      VAGOLFD_excl_RUS %>% colSums(na.rm=TRUE) %>% 
        as_tibble(rownames="ind") %>% mutate(country=substr(ind,1,3), industry = substr(ind,5,20)) %>% 
        rename(VVA_excl= value)
    ) %>% 
    
    # beräkna differens och relativ 
    relocate(ind, country, industry) %>% 
    mutate(diff_excl_RUS = VVA_excl - VVA_with, 
           ratio_excl_RUS = VVA_excl / VVA_with-1) %>% 
    
    # summera per land
    group_by(country) %>%
    mutate(countrysum_VVA_with = sum(VVA_with, na.rm=TRUE), 
           countrysum_VVA_excl = sum(VVA_excl, na.rm=TRUE), 
           diff_country_VVA_excl = countrysum_VVA_excl - countrysum_VVA_with, 
           ratio_country_VVA_excl = countrysum_VVA_excl / countrysum_VVA_with -1) %>%
    
    # lägg till svenska labels för diagram
    left_join(country_labels_df) %>% 
    ungroup %>% 
    
    # returna endast resultattabell
    return()
}



################################################################################
### hypothetical extraction Ryssland
# hyp_ext_RUS <- my_function_hypothetical_extraction("RUS")
# hyp_ext_RUS %>% write_rds("hyp_ext_RUS.rds")

hyp_ext_RUS <- read_rds("hyp_ext_RUS.rds")

### Graf: resultat hypothetical extraction Ryssland
hypext_prc_df <-  hyp_ext_RUS %>% 
  distinct(ratio_country_VVA_excl, .keep_all=TRUE) %>% 
  slice_min(ratio_country_VVA_excl, n=15)  %>% 
  rename(value= ratio_country_VVA_excl)
  
g_hypext_prc <- hypext_prc_df %>% 
  ggplot(aes(y=value*100, fill=value, 
             x=reorder(country_label, value))) + 
  geom_col() +
  scale_fill_gradient() +
  geom_text(data= hypext_prc_df %>% slice_min(value, n=9), 
            aes(label=country_label, y=(value*100) + .1), angle=90, color="white", hjust=0, size=2.5) +
  geom_text(data= hypext_prc_df %>% slice_max(value, n=6), 
            aes(label=country_label, y=(value*100) - .1), angle=90, color="black", hjust=1, size=2.5) +
  theme(legend.position = "none", 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        text=element_text(size=8)) +
  labs(x=NULL, y="Procent skillnad", 
       subtitle="Procent")

g_hypext_prc

hypext_min_df <- hyp_ext_RUS %>% 
  drop_na(country_label) %>% 
  distinct(diff_country_VVA_excl, country, country_label) %>% 
  slice_min(diff_country_VVA_excl, n=15)  %>% 
  rename(value= diff_country_VVA_excl)  
  
g_hypext_diff <- hypext_min_df %>% 
  ggplot(aes(y=value/1000, fill=value, 
             x=reorder(country_label, value))) + 
  geom_col() +
  scale_fill_gradient() +
  geom_text(data=hypext_min_df %>% slice_min(value, n=7), 
            aes(label=country_label, y=value/1000 +.5), angle=90, color="white", hjust=0, size=2.5) +
  geom_text(data=hypext_min_df %>% slice_max(value, n=8), 
            aes(label=country_label, y=value/1000 -.5), angle=90, color="black", hjust=1, size=2.5) +
  theme(legend.position = "none", 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        text=element_text(size=8)) +
  labs(x=NULL, y="Mdr USD", 
       subtitle="Amerikanska dollar")


(g_hypext_prc + g_hypext_diff)
(g_hypext_prc + g_hypext_diff) %>% 
  ggsave(filename="g_hypext_ryssland.pdf", width=7, heigh=4)






################################################################################
### Hypothecial extraction Kina
### CN1 och CN2: exkl processbeartning, samt enbart processbearbetning
#hyp_ext_CN1 <- my_function_hypothetical_extraction("CN1")
#hyp_ext_CN1 %>% write_rds("hyp_ext_CN1.rds")

#hyp_ext_CN2 <- my_function_hypothetical_extraction("CN2")
#hyp_ext_CN2 %>% write_rds("hyp_ext_CN2.rds")

hyp_ext_CN1 <- read_rds("hyp_ext_CN1.rds")
hyp_ext_CN2 <- read_rds("hyp_ext_CN2.rds")

hyp_ext_china <- hyp_ext_CN1 %>% 
  select(country, countrysum_VVA_with, cn1_excl = countrysum_VVA_excl, cn1_diff = diff_country_VVA_excl) %>% 
  distinct() %>% 
  left_join(hyp_ext_CN2 %>% select(country,  cn2_excl = countrysum_VVA_excl, cn2_diff = diff_country_VVA_excl) %>% 
              distinct) %>% 
  mutate(diff_sum = cn1_diff + cn2_diff, 
         ratio_sum = diff_sum/countrysum_VVA_with) %>% 
  left_join(country_labels_df) %>% 
  drop_na(country_label)

g_hypext_china_1 <- hyp_ext_china %>% 
  slice_min(diff_sum, n=10) %>% 
  pivot_longer(ends_with("diff")) %>% 
  ggplot(aes(x=reorder(country, value), 
             y=value/1000, 
             fill=name)) +
  geom_col(position="stack") +
  geom_text(aes(label=if_else(name=="cn1_diff" & country=="USA",country_label, NULL)), 
            angle=90, color="white", hjust=1, size=2.5) +
  geom_text(aes(y=value/1000 -25, label=if_else(name=="cn1_diff" & country!="USA",country_label, NULL)), 
            angle=90, color="black", hjust=1, size=2.5) +
  theme(axis.text.x=element_blank() ,
        text=element_text(size=8), 
        axis.ticks.x=element_blank(), 
        legend.position = "none") +
  labs(x=NULL, y="Mdr USD", 
       subtitle="Amerikanska dollar") 
  scale_fill_discrete(name=NULL, 
                      labels=c("Exkl exportbearbetning", "Exportbearbetning"))
  
g_hypext_china_2 <- hyp_ext_china %>% 
  slice_min(ratio_sum, n=10) %>%
  mutate(cn1_ratio = cn1_excl / countrysum_VVA_with-1, 
         cn2_ratio = cn2_excl / countrysum_VVA_with-1) %>% 
  pivot_longer(ends_with("ratio")) %>% 
  
  ggplot(aes(x=reorder(country, value), 
             y=value*100, 
             fill=name)) +
  geom_col(position="stack") +
  geom_text(aes(y=value*100, label=if_else(name=="cn1_ratio" & country=="MX2",country_label, NULL)), 
            angle=90, color="white", hjust=1, size=2.5) + 
  geom_text(aes(y=value*100 - 4, label=if_else(name=="cn1_ratio" & country!="MX2", country_label, NULL)), 
            angle=90, color="black", hjust=1, size=2.5) +
  theme(axis.text.x=element_blank() ,
        text=element_text(size=8), 
        axis.ticks.x=element_blank()) +
  labs(x=NULL, y="Procent", 
       subtitle="Procent") +
  scale_fill_discrete(name=NULL, 
                      labels=c("Exkl exportbearbetning", "Exportbearbetning"))

(g_hypext_china_1 + g_hypext_china_2)
(g_hypext_china_1 + g_hypext_china_2) %>%  
  ggsave(filename="g_hypext_china.pdf", width=7, height = 4)



################################################################################
### hypothetical extraction Indien
#hyp_ext_IND <- my_function_hypothetical_extraction("IND")
#hyp_ext_IND %>% write_rds("hyp_ext_IND.rds")
hyp_ext_IND <- read_rds("hyp_ext_IND.rds")

hex_ind_prc_df <- hyp_ext_IND %>% 
  drop_na(country_label) %>% 
  distinct(ratio_country_VVA_excl, .keep_all=TRUE) %>% 
  slice_min(ratio_country_VVA_excl, n=15) %>% 
  rename(value= ratio_country_VVA_excl) 

g_hex_ind_1 <- hex_ind_prc_df %>% 
  ggplot(aes(y=value*100, fill=value, 
             x=reorder(country_label, value))) + 
  geom_col() +
  scale_fill_gradient2(high="greenyellow", low="green4") +
  geom_text(data= hex_ind_prc_df %>% slice_min(value, n=6), 
            aes(label=country_label, y=value*100 + .1), angle=90, color="white", hjust=0, size=2.5) +
  geom_text(data= hex_ind_prc_df %>% slice_max(value, n=9), 
            aes(label=country_label, y=value*100 - .1), angle=90, color="black", hjust=1, size=2.5) +
  theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        text=element_text(size=8)) +
  labs(x=NULL, y="Procent skillnad", 
       subtitle="Procent")

hex_ind_diff_df <- hyp_ext_IND %>% 
  drop_na(country_label) %>% 
  distinct(diff_country_VVA_excl, .keep_all=TRUE) %>% 
  slice_min(diff_country_VVA_excl, n=15) %>% 
  rename(value= diff_country_VVA_excl) 

g_hex_ind_2 <- hex_ind_diff_df %>% 
  ggplot(aes(y=value/1000, fill=value, 
             x=reorder(country_label, value))) + 
  geom_col() +
  scale_fill_gradient2(high="greenyellow", low="green4") +
  geom_text(data=hex_ind_diff_df %>% slice_min(value, n=4), 
            aes(label=country_label, y=value/1000 +.5), angle=90, color="white", hjust=0, size=2.5) +
  geom_text(data=hex_ind_diff_df %>% slice_max(value, n=11), 
            aes(label=country_label, y=value/1000 -.5), angle=90, color="black", hjust=1, size=2.5) +
  theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        text=element_text(size=8)) +
  labs(x=NULL, y="Mdr USD", 
       subtitle="Amerikanska dollar")


(g_hex_ind_1 + g_hex_ind_2)
(g_hex_ind_1 + g_hex_ind_2) %>% 
  ggsave(filename="g_hypo_extract_ind.pdf", width=7, height=4)







################################################################################
### Beräkna olika variabler, per land
# Loop över årtalen 1-24
# loop i loopen över ländern
################################################################################
variables_per_country <- 23:24 %>% map_dfr(~{
  print(.x + 1994)
  this_year <- .x
  
  ### importera data utan de där länderna mx1, mx2...
  # VA
  icioVA <- ICIO2021econVA[this_year,
                           names(ICIO2021econVA[this_year,]) %>% str_subset(excl_countries, negate=TRUE)] 
  # total output
  icioX <- ICIO2021econX[this_year, 
                         colnames(ICIO2021econX) %>% str_subset(excl_countries, negate=TRUE)]
  # final demand
  icioFD <- ICIO2021econFD[this_year,
                           rownames(ICIO2021econFD[this_year,,]) %>% str_subset(excl_countries, negate=TRUE),
                           colnames(ICIO2021econFD[this_year,,]) %>% str_subset(excl_countries, negate=TRUE)]
  
  # flödestabellen
  icioZ <- ICIO2021econZ[this_year,
                         colnames(ICIO2021econZ[this_year,,]) %>% str_subset(excl_countries, negate=TRUE),
                         rownames(ICIO2021econZ[this_year,,]) %>% str_subset(excl_countries, negate=TRUE)]
  
  
  ##############################################################################
  ###### Loop över länder
  ##############################################################################
  all_countries %>% map_dfr(~{ 
    print(.x)
    this_country <- .x
    
    ################################################
    # this_year <- 24 ; this_country <- "SWE" ; excl_countries <- "NULL"
    ################################################
    
    ### beräkna VA/GO
    diag_vago <- 
      # VA till tabell
      icioVA[ names(icioVA) %>% str_subset(this_country) ] %>% 
      as_tibble %>% 
      rename(va = value) %>% 
      mutate(cty_industry = names( icioVA ) %>% str_subset(this_country) ) %>% 
      
      # GO till tabell
      left_join( icioX[ names(icioX) %>% str_subset(this_country) ] %>% 
                   as_tibble %>% 
                   rename(go = value) %>% 
                   mutate(cty_industry = names(icioX) %>% str_subset(this_country))
                 , by="cty_industry") %>% 
      # calc VA/GO
      mutate(vago = va/go) %>% 
      # to diag matrix
      select(vago) %>% unlist %>% diag %>% fixmatrix

    ### beräkna L
    flowtable <- icioZ[rownames(icioZ) %>% str_subset(this_country),
                       colnames(icioZ) %>% str_subset(this_country)]
    
    GO <- icioX[names(icioX) %>% str_subset(this_country)]
    
    diag_go_inv <- ((1/GO) %>% diag) %>%  fixmatrix
  
    mat_A <- flowtable %*% diag_go_inv 
    colnames(mat_A) <- colnames(flowtable) 
    
    mat_L <- solve(diag(45) - mat_A)  # solve = matrix inverse
    
    
    ### diag matris Final demand, endast inhemskt, till aktuellt land
    # fd = en kolumn, en rad per bransch
    diag_FD <- icioFD[rownames(icioFD ) %>% str_subset(this_country), 
                      colnames(icioFD) %>% str_subset(this_country)] %>% 
      as_tibble %>% rename(fd=value) %>% 
      # to diag matrix
      select(fd) %>% unlist %>% diag
    
    
    ### beräkna 45x45 matris VAGOLFD 
    # cs VAGOLFD = Vertical VA = value chain income, såsom GVKI
    VAGOLFD <- (diag_vago %*% (mat_L %*% diag_FD))
    
    ############################################################################
    ### Skapa diverse variabler 
    ############################################################################
    # variabel: VVA inhemsk
    vva_df <- VAGOLFD %>% colSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(VVA=value) 
    
    # variabel: outside VVA = cs -j VAGOLFD
    ovva_df <- (VAGOLFD - (diag(VAGOLFD) %>% diag)) %>% colSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(outside_VVA = value)
    
    # variabel: output multiplier = cs L
    omp_df <- mat_L %>% colSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(output_multiplier = value) 
    
    # variabel: vertical gross output = cs LFD
    vgo_df <- (mat_L %*% diag_FD) %>% colSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(vertical_gross_output = value)
    
    # variabel: input multiplier = rs L
    im_df <- mat_L %>% rowSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(input_multiplier = value)
    
    # variabel: outside output multiplier = cs -j L (diag = 0)
    mat_L_d0 <- mat_L - (diag(mat_L) %>%  diag)
    oom_df <-  mat_L_d0 %>% colSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(outside_output_mp = value)
    
    # variabel: horisontell VVA = rs VAGOLFD
    HVVA_df <- VAGOLFD %>% rowSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(horizontal_VVA = value)
    
    # variabel: horisontell GO = rs LFD
    HGO_df <- (mat_L %*% diag_FD) %>% rowSums(na.rm=TRUE) %>% 
      as_tibble %>% rename(horizontal_GO = value)
    
    
    # variabel: output multiplier KIBS = 
    om_kibs_df <- mat_L[rownames(mat_L) %>% str_subset("69T75"),] %>% 
      as_tibble %>%  rename(output_multipl_kibs = value)
    
    # variabel: vanliga va
    va_df <- icioVA[names(icioVA) %>% str_subset(this_country) ] %>% 
      as_tibble %>% rename(va = value)
    
    # variabel: vanliga go
    go_df <- icioX[names(icioX) %>% str_subset(this_country) ] %>%
      as_tibble %>% rename(go = value)
    
    
    
    
    
    ############################################################################
    ### Funktion: total import multiplikator för ett eller flera länder
    # from_country , till aktuellt land
    ############################################################################
    calc_import_multiplier_fr_country <- function(from_country) {
      
      #from_country <- richworld
      # final demand
      mat_fd_import_fr_country_x <- icioFD[
                      # rader > sändare: tex Ryssland
                      rownames(icioFD) %>% str_subset(from_country),
                      # kolumner > mottagare: aktuellt land
                      colnames(icioFD) %>% str_subset(this_country) ] %>% 
        as_tibble(rownames="countryrow") %>%  
        mutate(industry = substr(countryrow, 5,10)) %>% 
        group_by(industry) %>% 
        # sum col för insatsimport per sektor
        mutate( across( starts_with(this_country),  sum )) %>%      
        # behåll en rad per bransch
        filter(row_number()==1) %>%
        
        ungroup %>% select(-countryrow, -industry) %>% as.matrix 
      
      # insatsimport
      mat_inputimport_fr_country_x <- icioZ[
                      # rader > från gruppen
                      rownames(icioZ) %>% str_subset(from_country),
                      # kolumner > till detta land 
                      colnames(icioZ) %>% str_subset(this_country) ] %>% 
        as_tibble(rownames="countryrow") %>%  
        mutate(industry = substr(countryrow, 5,10)) %>% 
        group_by(industry) %>% 
        # sum col för insatsimport per sektor
        mutate( across( starts_with(this_country),  sum )) %>%      
        # behåll en rad per bransch
        filter(row_number()==1) %>%
        
        ungroup %>% select(-countryrow, -industry) %>% as.matrix  
      
      rs_inputimport_fr_country_x <- mat_inputimport_fr_country_x %>% rowSums(na.rm=TRUE) %>% as.vector
      
      # Summera import till FD och import till insatsproduktion
      mat_import_fr_x <- (mat_fd_import_fr_country_x + rs_inputimport_fr_country_x) %>% as.vector
      
      mat_Aim_x <- (diag(mat_import_fr_x) %*% diag_go_inv) %>% fixmatrix
      # använd matris L för detta land, från ovan
      mat_Limbl_x <- (mat_Aim_x %*% mat_L) %>% fixmatrix
      mat_Limbl_x %>% 
        colSums(na.rm=TRUE) %>% 
        as_tibble %>% 
        
        return()
    }
    
    ############################################################################
    ### Importmultiplikator, från...
    # ...alla länder, till detta land
    allcountries <- c(all_countries,"ROW") %>% setdiff(this_country) %>% paste0(collapse="|")
    
    import_multiplier_from_allcountries <- calc_import_multiplier_fr_country(allcountries) %>% 
      rename(import_multiplier_from_allcountries = value)
    
    # ...Rika världen
    richworld <- c("AUS","AUT","BEL","CAN","CHE","DEU","DNK","ESP","FIN","FRA","GBR",
                   "GRC","IRL","ITA","JPN","KOR","LUX","NLD","NOR","PRT","SWE","USA") %>% 
      setdiff(this_country) %>% paste0(collapse="|")
    
    import_multiplier_from_richworld <- calc_import_multiplier_fr_country(richworld) %>% 
      rename(import_multiplier_from_richworld = value)
    
    # ...EU15 + GBR
    EU15 <- c("AUT","BEL","DNK","FIN","FRA","DEU","GRC","IRL","ITA","LUX","NLD","PRT","ESP","SWE","GBR") %>% 
      setdiff(this_country) %>% paste0(collapse="|")
    
    import_multiplier_from_eu15 <- calc_import_multiplier_fr_country(EU15) %>% 
      rename(import_multiplier_from_eu15=value)
    
    # ...fr EU27 + GBR
    EU27 <- c(EU15,"BGR","HRV","CYP","CZE","EST","HUN","LVA","LTU","MLT","POL","ROU","SVK","SVN") %>% 
      setdiff(this_country) %>% paste0(collapse="|")
    
    import_multiplier_from_eu27 <- calc_import_multiplier_fr_country(EU27) %>% 
      rename(import_multiplier_from_eu27=value)
    
    # ...Ryssland
    import_multiplier_from_russia <- calc_import_multiplier_fr_country("RUS") %>% 
      rename(import_multiplier_from_russia = value)
    
    # ...USA
    import_multiplier_from_usa <- calc_import_multiplier_fr_country("USA") %>% 
      rename(import_multiplier_from_usa = value)
    
    # ...Kina
    import_multiplier_from_china <- calc_import_multiplier_fr_country("CHN") %>% 
      rename(import_multiplier_from_china = value)
    # ...Kina 1
    import_multiplier_from_cn1 <- calc_import_multiplier_fr_country("CN1") %>% 
      rename(import_multiplier_from_cn1 = value)
    # ...Kina 2
    import_multiplier_from_cn2 <- calc_import_multiplier_fr_country("CN2") %>% 
      rename(import_multiplier_from_cn2 = value)
    
    # ...Indien
    import_multiplier_from_india <- calc_import_multiplier_fr_country("IND") %>% 
      rename(import_multiplier_from_india = value)
    # ...Sverige
    import_multiplier_from_sweden <- calc_import_multiplier_fr_country("SWE") %>% 
      rename(import_multiplier_from_swe = value)
    
    
    
    
    
    ############################################################################
    ### Funktion: insatsimport multiplikator för ett eller flera länder
    # from_country , till aktuellt land
    ############################################################################
    calc_input_import_multiplier_fr_country <- function(from_country) {
      
      #from_country <- richworld
      
      # insatsimport
      mat_inputimport_fr_country_x <- icioZ[
        # rader > från gruppen
        rownames(icioZ) %>% str_subset(from_country),
        # kolumner > till detta land 
        colnames(icioZ) %>% str_subset(this_country) ] %>% 
        as_tibble(rownames="countryrow") %>%  
        mutate(industry = substr(countryrow, 5,10)) %>% 
        group_by(industry) %>% 
        # sum col för insatsimport per sektor
        mutate( across( starts_with(this_country),  sum )) %>%      
        # behåll en rad per bransch
        filter(row_number()==1) %>%
        
        ungroup %>% select(-countryrow, -industry) %>% as.matrix  
      
      # rs_inputimport_fr_country_x <- mat_inputimport_fr_country_x %>% rowSums(na.rm=TRUE) %>% as.vector
      # mat_Aim_x <- (diag(mat_import_fr_x) %*% diag_go_inv) %>% fixmatrix
      # mat_Limbl_x <- (mat_Aim_x %*% mat_L) %>% fixmatrix
      # mat_Limbl_x %>% colSums(na.rm=TRUE) %>% as_tibble %>% 
      
      ## Dessa kommandon beräknar A_im och därefter L_imbl = A_im * L,
      # matris L = inhemska Leontief, skapas ovan
      ((((mat_inputimport_fr_country_x %*% diag_go_inv) %>% 
           fixmatrix) %*% mat_L) %>% 
          fixmatrix) %>% 
        colSums(na.rm=TRUE) %>% 
        as_tibble %>% 
        
        return()
    }
    
    ############################################################################
    ### Insatsimportmultiplikator, från...
    # ...alla länder, till detta land
    input_import_multiplier_from_allcountries <- calc_input_import_multiplier_fr_country(allcountries) %>% 
      rename(input_import_multiplier_from_allcountries = value)
    
    # ...Rika världen
    input_import_multiplier_from_richworld <- calc_input_import_multiplier_fr_country(richworld) %>% 
      rename(input_import_multiplier_from_richworld = value)
    
    # ...EU15 + GBR
    input_import_multiplier_from_eu15 <- calc_input_import_multiplier_fr_country(EU15) %>% 
      rename(input_import_multiplier_from_eu15=value)
    
    # ...fr EU27 + GBR
    input_import_multiplier_from_eu27 <- calc_input_import_multiplier_fr_country(EU27) %>% 
      rename(input_import_multiplier_from_eu27=value)
    
    # ...Ryssland
    input_import_multiplier_from_russia <- calc_input_import_multiplier_fr_country("RUS") %>% 
      rename(input_import_multiplier_from_russia = value)
    
    # ...USA
    input_import_multiplier_from_usa <- calc_input_import_multiplier_fr_country("USA") %>% 
      rename(input_import_multiplier_from_usa = value)
    
    # ...Kina
    input_import_multiplier_from_china <- calc_input_import_multiplier_fr_country("CHN") %>% 
      rename(input_import_multiplier_from_china = value)
    # ...Kina 1
    input_import_multiplier_from_cn1 <- calc_input_import_multiplier_fr_country("CN1") %>% 
      rename(input_import_multiplier_from_cn1 = value)
    # ...Kina 2
    input_import_multiplier_from_cn2 <- calc_input_import_multiplier_fr_country("CN2") %>% 
      rename(input_import_multiplier_from_cn2 = value)
    
    # ...Indien
    input_import_multiplier_from_india <- calc_input_import_multiplier_fr_country("IND") %>% 
      rename(input_import_multiplier_from_india = value)
    # ...Sverige
    input_import_multiplier_from_sweden <- calc_input_import_multiplier_fr_country("SWE") %>% 
      rename(input_import_multiplier_from_swe = value)
    
    
  
    
    ############################################################################
    ### funktion: Total export multiplier, 
    # to_country, fr aktuellt land
    ############################################################################
    calc_export_multiplier_to_country <- function(to_country) {
      
      #   to_country <- allcountries
      # FD export till to_country, från aktuellt land
      mat_fd_export_to_x <- icioFD[
                       # rader > sändare: aktuellt land
                       rownames(icioFD) %>% str_subset(this_country),
                       # kolumner > mottagare: Ryssland
                       colnames(icioFD) %>% str_subset(to_country)] %>% 
          as.matrix 
      # insatsexport till land X  
      mat_inputexport_to_country_x <- icioZ[
                      # rader > från aktuellt land
                      rownames(icioZ) %>% str_subset(this_country),
                      # kolumner > till 
                      colnames(icioZ) %>% str_subset(to_country)] %>% 
          as.matrix
        
      rs_fdexport_to_country_x <- mat_fd_export_to_x %>% rowSums(na.rm=TRUE) %>% as.vector
      
      rs_inputexport_to_country_x <- mat_inputexport_to_country_x %>% rowSums(na.rm=TRUE) %>% as.vector
      
      mat_export_to_x <- (rs_fdexport_to_country_x + rs_inputexport_to_country_x) %>% as.vector
      
      EXPGOL_to_x <- (diag(mat_export_to_x %*% diag_go_inv %>% as.vector %>% fixmatrix) %*% mat_L) %>% fixmatrix
      rownames(EXPGOL_to_x) <- colnames(EXPGOL_to_x)
      
      EXPGOL_to_x %>% colSums(na.rm=TRUE) %>% as_tibble %>% 
        
        return()
    }
    ############################################################################
    ##### beräkna export multiplier till... 
    # ...alla andra länder, fr aktuellt land
    export_multiplier_to_allcountries <- calc_export_multiplier_to_country(allcountries) %>% 
      rename(export_multiplier_to_allcountries = value)
    
    # ...EU27
    export_multiplier_to_eu27 <- calc_export_multiplier_to_country(EU27) %>% 
      rename(export_multiplier_to_eu27 = value)
    
    # ...EU15
    export_multiplier_to_eu15 <- calc_export_multiplier_to_country(EU15) %>% 
      rename(export_multiplier_to_eu15 = value)
    
    # ...22 rika länder
    export_multiplier_to_richworld <- calc_export_multiplier_to_country(richworld) %>% 
      rename(export_multiplier_to_richworld = value)
    
    # ...Ryssland, fr aktuellt land
    export_multiplier_to_russia <- calc_export_multiplier_to_country("RUS") %>% 
      rename(export_multiplier_to_russia = value)
    
    # ...Sverige, 
    export_multiplier_to_sweden <- calc_export_multiplier_to_country("SWE") %>% 
      rename(export_multiplier_to_sweden =value)
        
    # ...Kina, 
    export_multiplier_to_china <- calc_export_multiplier_to_country("CHN") %>% 
      rename(export_multiplier_to_china = value)
    
    # ...Kina 1, fr aktuellt land
    export_multiplier_to_cn1 <- calc_export_multiplier_to_country("CN1") %>% 
      rename(export_multiplier_to_cn1 = value)
    # ...Kina 2, fr aktuellt land
    export_multiplier_to_cn2 <- calc_export_multiplier_to_country("CN2") %>% 
      rename(export_multiplier_to_cn2 = value)
    
    # ...Indien
    export_multiplier_to_india <- calc_export_multiplier_to_country("IND") %>% 
      rename(export_multiplier_to_india = value)
    
    # ...USA, fr aktuellt land
    export_multiplier_to_usa <- calc_export_multiplier_to_country("USA") %>% 
      rename(export_multiplier_to_usa = value)
    
    
    
    
    
    ############################################################################
    ### Samla variabler i tabell för detta land
    ############################################################################
    cbind(
      # export multipliers
      export_multiplier_to_allcountries,
      export_multiplier_to_richworld,
      export_multiplier_to_eu27,
      export_multiplier_to_eu15,
      
      export_multiplier_to_russia,
      export_multiplier_to_usa,
      export_multiplier_to_china,
      export_multiplier_to_cn1,
      export_multiplier_to_cn2,
      export_multiplier_to_india,
      export_multiplier_to_sweden,
      
      # import multipliers
      import_multiplier_from_allcountries,
      import_multiplier_from_richworld, 
      import_multiplier_from_eu27,
      import_multiplier_from_eu15,
      
      import_multiplier_from_russia,
      import_multiplier_from_usa,
      import_multiplier_from_china,
      import_multiplier_from_cn1,
      import_multiplier_from_cn2,
      import_multiplier_from_india,
      import_multiplier_from_sweden, 
      
      # input import multipliers
      input_import_multiplier_from_allcountries,
      input_import_multiplier_from_richworld, 
      input_import_multiplier_from_eu27,
      input_import_multiplier_from_eu15,
      
      input_import_multiplier_from_russia,
      input_import_multiplier_from_usa,
      input_import_multiplier_from_china,
      input_import_multiplier_from_cn1,
      input_import_multiplier_from_cn2,
      input_import_multiplier_from_india,
      input_import_multiplier_from_sweden, 
      
      
      ### Inhemska
      vva_df,
      ovva_df,
      omp_df,
      vgo_df,
      im_df,
      oom_df,
      HVVA_df,
      HGO_df,
      om_kibs_df,
          
      ### vanlig va och go
      va_df,
      go_df
      )  %>% 
      
      # skapa variabel med branscher, en lista per land
      # bra för säkerhetskoll senare osv
      mutate(industry_global = names(icioVA) %>% str_subset(this_country) ) %>% 
      
    return()
    }) %>% 
    # lägg till år, för alla länder
    mutate(year=this_year + 1994) %>% 
    return()
  })  %>% 
  # skapa variablerna 
  mutate(country = substr(industry_global, 1,3),
         industry = substr(industry_global, 5,99)) %>% 
  relocate(country, industry, year)

################################################################################
### exportera lite resultat
setwd(resultatkat)

#domestic_df %>% write_rds("domestic_df.rds")
#domestic_df %>% write_xlsx("domestic_df_211127.xlsx")
variables_per_country %>% write_rds("variables_per_country.rds")
variables_per_country %>% 
  write_xlsx("temp resultat diverse Ryssland 220402.xlsx")

variables_per_country <- read_rds("variables_per_country.rds")

variables_per_country %>% colnames




################################################################################
### diagram 
################################################################################

### andra länders import-multiplikator i Ryssland

richworld <- richworld %>% str_split(pattern="\\|") %>% unlist

multiplier_label_df = tibble(name = variables_per_country %>% select(starts_with("input_import")) %>% colnames ,  
                          multiplier_label= c("Alla länder", 
                                              "22 rika länder*",
                                              "EU27 inkl UK",
                                              "EU15 inkl UK",
                                              "Ryssland",
                                              "USA",
                                              "Kina",
                                              "Kina \nexkl exportbearbetning",
                                              "Kina \nexportbearbetning",
                                              "Indien",
                                              "Sverige") )

mlp_df1 <- variables_per_country %>% 
  # fokus på Ryssland
  filter(country=="RUS", year==2018) %>% 
  select(-input_import_multiplier_from_russia , 
         -input_import_multiplier_from_china,
         #-input_import_multiplier_from_all_countries, 
         #-input_import_multiplier_from_richworld
         ) %>% 

  # medelvärde över alla branscher
  select(country, industry, year, starts_with("input_import")) %>% 
  pivot_longer(starts_with("input_import")) %>% 
  group_by(country, year, name) %>% 
  mutate(mean_value = mean(value, na.rm=TRUE)) %>% 
  distinct(mean_value) %>% 
  ungroup %>% 
  # Add labels for the groups
  left_join(multiplier_label_df)
    

g_input_import_multipliers_russia <- mlp_df1 %>% 
  ggplot(aes(y=reorder(multiplier_label, mean_value), 
             x=mean_value, 
             fill=mean_value)) + 
  geom_col() +
  theme(legend.position = 'none',
        text=element_text(size=8),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  scale_fill_gradient2() +
  labs(y=NULL, x="Import per \nefterfrågad enhet") +
  geom_text(data= mlp_df1 %>% slice_max(mean_value, n=4), size=2.5,
            aes(label=multiplier_label, x=mean_value -.002), hjust=1, color="white") +
  geom_text(data= mlp_df1 %>% slice_min(mean_value, n=5), size=2.5,
            aes(label=multiplier_label, x=mean_value +.002), hjust=0, color="black")

  
g_input_import_multipliers_russia
g_input_import_multipliers_russia %>% 
  ggsave(filename="g_input_import_multipliers_russia.pdf", width=3, height=3)




variables_per_country %>% 
  select(country, industry, year, input_import_multiplier_from_russia) %>% 
  filter(country=="SWE") %>% 
  group_by(country, year) %>% 
  mutate(countrysum_imp_multipl_russia = mean(input_import_multiplier_from_russia, na.rm=TRUE)) %>% 
  
  distinct(year, countrysum_imp_multipl_russia) %>% 
  ggplot(aes(x=year, y=countrysum_imp_multipl_russia)) + geom_line()







################################################################################
### Rysslands importmultplikator i andra länder
################################################################################

variables_per_country %>% colnames

russian_mpl_df <- variables_per_country %>% 
  # fokus på andra länder, men Rysslands importmultiplikator
  filter(year==2018, 
         country!="RUS") %>% 
  select(country, industry, year, value = input_import_multiplier_from_russia) %>% 
  
  # medelvärde över alla branscher
  group_by(country, year) %>% 
  mutate(mean_value = mean(value, na.rm=TRUE)) %>% 
  distinct(mean_value) %>% 
  ungroup %>% 
  left_join(country_labels_df)

g_russias_importmultipl_selected <- russian_mpl_df %>% 
  # selected rich countries
  filter(country %in% c(comparison_countries %>% setdiff("CHN"),"CN1","CN2")) %>% 
  # graf
  ggplot(aes(x=mean_value, 
             y=reorder(country, mean_value), 
             fill=mean_value)) +
  geom_col() +
  geom_text(aes(label=country_label, x=mean_value + .001),
            color="black", hjust=0, size=2.5) +
  scale_fill_gradient2() + 
  labs(y=NULL, 
       x="Import fr Ryssland \nper efterfrågad enhet", 
       subtitle="Utvalda länder") +
  theme(text=element_text(size=8), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        legend.position = "none")


g_russias_importmultipl_top <- russian_mpl_df %>% 
  # top 15
  slice_max(mean_value, n=15) %>% 
  # graf
  ggplot(aes(x=mean_value, 
             y=reorder(country, mean_value), 
             fill=mean_value)) +
  geom_col() +
  geom_text(aes(label=country_label, x=mean_value + .01), 
            color="black", hjust=0, size=2.5) +
  scale_fill_gradient2() + 
  labs(y=NULL, 
       x="Import från Ryssland, \nper efterfrågad enhet", 
       subtitle="Topp 15") +
  theme(text=element_text(size=8), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        legend.position = "none")
  
(g_russias_importmultipl_top + g_russias_importmultipl_selected)
(g_russias_importmultipl_top + g_russias_importmultipl_selected) %>% 
  ggsave(filename = "g_russias_importmultipl_other_countries.pdf", width=7, height =4)




################################################################################
### Rysslands exportmultiplikator
################################################################################
variables_per_country %>% colnames

rus_exp_mpl_df <- variables_per_country %>% 
  select(country, year, industry, export_multiplier_to_russia) %>% 
  group_by(country, year) %>% 
  mutate(mean_value = mean(export_multiplier_to_russia, na.rm=TRUE)) %>% 
  distinct(mean_value) %>% 
  ungroup %>% 
  # endast 2018, ej Ryssland (pga Rysslands exportmpl.)
  filter(year==max(year), 
         country!="RUS") %>% 
  left_join(country_labels_df) %>% 
  drop_na(country_label)


# Rysk exportmpl. top 15
g_rus_exp_mpl_top15 <- rus_exp_mpl_df %>% 
  slice_max(mean_value, n=15) %>% 
  ggplot(aes(x=mean_value, 
             y=reorder(country, mean_value), 
             fill=mean_value)) +
  geom_col() +
  geom_text(data = rus_exp_mpl_df %>% slice_max(mean_value, n=15) %>% slice_max(mean_value, n=7),
            aes(label=country_label, x=mean_value -.001),
            color="white", hjust=1, size=2.5) +
  geom_text(data = rus_exp_mpl_df %>% slice_max(mean_value, n=15) %>% slice_min(mean_value, n=8),
            aes(label=country_label, x=mean_value +.001),
            color="black", hjust=0, size=2.5) +
  scale_fill_gradient2() + 
  scale_x_continuous(breaks=seq(0,.1,by=.02), limits = c(0,.1)) +
  labs(y=NULL, 
       x="Export per \nproducerad enhet", 
       subtitle="Topp 15") +
  theme(text=element_text(size=8), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        legend.position = "none")


# Rysk export.mpl. utvalda länder
g_rus_exp_mpl_selected <- rus_exp_mpl_df %>% 
  filter(country %in% comparison_countries) %>% 
  
  ggplot(aes(x=mean_value, 
             y=reorder(country, mean_value), 
             fill=mean_value)) +
  geom_col() +
  geom_text(aes(label=country_label, x=mean_value +.001),
            color="black", hjust=0, size=2.5) +
  scale_fill_gradient2() + 
  scale_x_continuous(breaks=seq(0,.1,by=.02), limits = c(0,.1)) +
  labs(y=NULL, 
       x="Exporterad per \nproducerad enhet", 
       subtitle="Utvalda länder") +
  theme(text=element_text(size=8), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        legend.position = "none")


(g_rus_exp_mpl_top15 + g_rus_exp_mpl_selected)
(g_rus_exp_mpl_top15 + g_rus_exp_mpl_selected) %>% 
  ggsave(filename="g_rus_export_mpl.pdf", width=7, height=4)




################################################################################
### Import- och exportmultiplikator, till världen, per bransch i Ryssland
################################################################################
variables_per_country %>% colnames

# Exportmultiplikator mot världen, i Ryssland
g_exp_mpl_rus <- variables_per_country %>% 
  filter(country=="RUS", year==2018) %>% 
  select(country, year, 
         industry_code=industry, 
         export_multiplier_to_allcountries) %>% 
  left_join(industry_labels) %>% 
  select(-original, -industry_engelsk) %>%
  
  slice_max(export_multiplier_to_allcountries, n=20) %>% 
  
  ggplot(aes(x=export_multiplier_to_allcountries, 
             y=reorder(industry, export_multiplier_to_allcountries), 
             fill=export_multiplier_to_allcountries))+
  geom_col() + 
  scale_fill_gradient2() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        text=element_text(size=8), 
        legend.position = "none") +
  labs(x="Export per producerad enhet", 
       y=NULL, 
       subtitle="Export till världen, från Ryssland") +
  geom_text(aes(x=.01, label=industry), size=2.5, color="white", hjust=0)


# Insatsmultiplikator mot världen, i Ryssland
g_imp_mpl_rus <- variables_per_country %>% 
  filter(country=="RUS", year==2018) %>% 
  select(country, year, 
         industry_code=industry, 
         input_import_multiplier_from_allcountries) %>% 
  left_join(industry_labels) %>% 
  select(-original, -industry_engelsk) %>%
  
  slice_max(input_import_multiplier_from_allcountries, n=20) %>% 
  
  ggplot(aes(x=input_import_multiplier_from_allcountries, 
             y=reorder(industry, input_import_multiplier_from_allcountries), 
             fill=input_import_multiplier_from_allcountries))+
  geom_col() + 
  scale_fill_gradient2() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        text=element_text(size=8), 
        legend.position = "none") +
  labs(x="Import per producerad enhet", 
       y=NULL, 
       subtitle="Import från världen, till Ryssland") +
  geom_text(aes(x=.01, label=industry), size=2.5, color="white", hjust=0)


(g_exp_mpl_rus + g_imp_mpl_rus)
(g_exp_mpl_rus + g_imp_mpl_rus) %>% 
  ggsave(filename="g_imp_exp_mpl_rus.pdf", width=7, height=5)






################################################################################
### Korrelation, BL och FL, samt import- & exportmultiplikator
################################################################################
this_year <- 24   # 2018
this_country <- "RUS"
# flödestabellen
  icioZ <- ICIO2021econZ[this_year,
                       colnames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country),
                       rownames(ICIO2021econZ[this_year,,]) %>% str_subset(this_country)]

# total produktion per bransch
  icioX <- ICIO2021econX[this_year,names(ICIO2021econX[this_year,]) %>% str_subset(this_country)]
# för att beräkna A tar vi 1/total produktion.
mat_GOinv <- (1/icioX) %>% fixmatrix ; mat_A <- icioZ %*% diag(mat_GOinv) 
# Beräkna matris L
mat_L <- solve( diag((mat_A %>% dim)[1]) - mat_A) ; rownames(mat_L) <- colnames(mat_L) <- names(icioX)




### backward linkeage = colsum L. forward linkeage = rowsum L
cs_L <- mat_L %>% colSums(na.rm=TRUE) ; rs_L <- mat_L %>% rowSums(na.rm=TRUE) 

russia_corr_df <- cbind(cs_L, rs_L) %>% 
  as_tibble(rownames="countryrow") %>% 
  mutate(industry_code = substr(countryrow, 5,10)) %>% 
  
  left_join(
      variables_per_country %>% 
        filter(country=="RUS", year==2018) %>% 
        select(country, year, industry_code=industry, 
               export_multiplier_to_allcountries, 
               input_import_multiplier_from_allcountries) %>% 
        left_join(industry_labels) %>% 
        select(-original, -industry_engelsk) 
    ) %>% 
  left_join(
    ICIO2021econVA[this_year,names(ICIO2021econVA[this_year,]) %>% str_subset(this_country)] %>%
      as_tibble(rownames="countryrow") %>%
      mutate(industry_code = substr(countryrow, 5,10)) %>% 
      rename(value_added=value)
    )

russia_corr_df

# Top 50%
mean_cs_L <- russia_corr_df$cs_L %>% mean 
mean_rs_L <-   russia_corr_df$rs_L %>% mean
mean_inp_imp_mpl <- russia_corr_df$input_import_multiplier_from_allcountries %>% mean
mean_exp_mpl <- russia_corr_df$export_multiplier_to_allcountries %>% mean

russia_corr_df$color_g3 %>% head(45)

russia_corr_df <- russia_corr_df %>% 
  mutate(color_g1 = if_else(cs_L > mean_cs_L & export_multiplier_to_allcountries > mean_exp_mpl, 1,0), 
         color_g2 = if_else(cs_L > mean_cs_L & input_import_multiplier_from_allcountries > mean_inp_imp_mpl, 1,0), 
         
         color_g3 = if_else(rs_L > mean_rs_L & export_multiplier_to_allcountries > mean_exp_mpl, 1,0), 
         color_g4 = if_else(rs_L > mean_rs_L & input_import_multiplier_from_allcountries > mean_inp_imp_mpl, 1,0)
         )



################################################################################
#### grafer för BL 
g_bl_exp_mpl_russia <- russia_corr_df %>% 
  ggplot(aes(x=cs_L, 
             y=export_multiplier_to_allcountries, 
             color=factor(color_g1))) +
  geom_point(aes(size=value_added), 
             alpha=.7) +
  scale_size(range=c(.2,20)) +
  scale_color_manual(breaks=c(0,1), values=c("grey", "purple"))+
  geom_text_repel(aes(label=industry), 
                  size=2.5) +
  theme(legend.position = "none", 
        text=element_text(size=8)) +
  labs(x="Totalproduktion per\nefterfrågad enhet", 
       y="Export till världen\nper producerad enhet", 
       subtitle="Prickarnas storlek = förädlingsvärde. Data för 2018.")
  
g_bl_exp_mpl_russia

g_bl_imp_mpl_russia <- russia_corr_df %>% 
  ggplot(aes(x=cs_L, 
             y=input_import_multiplier_from_allcountries, 
             color=factor(color_g2))) +
  geom_point(aes(size=value_added), 
             alpha=.7) +
  scale_color_manual(breaks=c(0,1), values=c("grey", "purple"))+
  scale_size(range=c(.2,20)) +
  geom_text_repel(aes(label=industry), 
                  size=2.5) +
  theme(legend.position = "none", 
        text=element_text(size=8)) +
  labs(x="Totalproduktion per \nefterfrågad enhet", 
       y="Insatsimport från världen \nper efterfrågad enhet", 
       subtitle="Prickarnas storlek = förädlingsvärde. Data för 2018.")

(g_bl_exp_mpl_russia + g_bl_imp_mpl_russia)
(g_bl_exp_mpl_russia + g_bl_imp_mpl_russia) %>% 
  ggsave(filename="g_bl_expimp_russia.pdf", width=7, height=4)

### grafer för FL
g_fl_exp_mpl_russia <- russia_corr_df %>% 
  ggplot(aes(rs_L, export_multiplier_to_allcountries, 
             color=factor(color_g3))) + 
  geom_point(aes(size=value_added), 
             alpha=.7) + 
  scale_size(range=c(.2,20)) +
  scale_color_manual(breaks=c(0,1), values=c("grey", "purple")) +
  geom_text_repel(aes(label=industry), 
                  size=2.5) +
  theme(legend.position = "none", 
        text=element_text(size=8)) +
  labs(x="Totalproduktion per enhet\ninsatsproduktion", 
       y="Export till världen\nper producerad enhet", 
       subtitle="Prickarnas storlek = förädlingsvärde. Data för 2018.")

g_fl_exp_mpl_russia


g_fl_imp_mpl_russia <- russia_corr_df %>% 
  ggplot(aes(x=rs_L, 
             y=input_import_multiplier_from_allcountries, 
             color=factor(color_g4))) +
  geom_point(aes(size=value_added), 
             alpha=.7) +
  scale_size(range=c(.2,20)) +
  scale_color_manual(breaks=c(0,1), values=c("grey", "purple")) +
  geom_text_repel(aes(label=industry), 
                  size=2.5, max.overlaps = 40) +
  theme(legend.position = "none", 
        text=element_text(size=8)) +
  labs(x="Totalproduktion per enhet\ninsatsproduktion", 
       y="Insatsimport per\nproducerad enhet", 
       subtitle="Prickarnas storlek = förädlingsvärde. Data för 2018.")

(g_fl_exp_mpl_russia + g_fl_imp_mpl_russia)
(g_fl_exp_mpl_russia + g_fl_imp_mpl_russia) %>% 
  ggsave(filename="g_fl_expimp_russia.pdf", width=7, height=4)
