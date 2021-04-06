# in v3, I change the source of CES data
# this code does the data manipulation and saves the csv files in the Shiny app folder
# so that the app can be updated easily

library(tidyverse)
library(plotly)
library(readxl)
library(rvest)
library(lubridate)
library(readxl)
library(tidycensus)

# per_Change foe elkhart goshen is not calculated as the base date is not existent in any indicator

# Data manipulation
base_date <- '06.25.20'

setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/South Bend MSA/Jobs/Jobs Available")

total_openings_1 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>% .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           `Job Openings`=as.numeric(str_remove(`Job Openings`,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="South Bend - Mishawaka")

setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Elkhart Goshen MSA/Jobs/Jobs Available")

total_openings_2 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>% .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           `Job Openings`=as.numeric(str_remove(`Job Openings`,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="Elkhart - Goshen")

total_openings <- bind_rows(total_openings_1,total_openings_2)

df0 <- total_openings %>% select(dt,`Job Openings`,msa) %>% mutate(indicator="Total Openings") %>%
  group_by(msa) %>%
  mutate(go_no_go=ifelse(base_date %in% dt,"go","no go"),
         per_change=ifelse(go_no_go=="go",(`Job Openings`-`Job Openings`[dt==base_date])/`Job Openings`[dt==base_date],NA)) %>%
  select(-go_no_go) %>% ungroup() %>% mutate(var="Total Openings")


setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/South Bend MSA/Jobs/Industries by Advertised Jobs")

openings_industries_1 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>% .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           `Job Openings`=as.numeric(str_remove(`Job Openings`,",")))
}) %>% bind_rows()%>% tibble() %>% mutate(msa="South Bend - Mishawaka")

setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Elkhart Goshen MSA/Jobs/Industries by Advertised Jobs")

openings_industries_2 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>% .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           `Job Openings`=as.numeric(str_remove(`Job Openings`,",")))
}) %>% bind_rows()%>% tibble()%>% mutate(msa="Elkhart - Goshen")

openings_industries <- bind_rows(openings_industries_1,openings_industries_2)

df1 <- openings_industries %>% select(var=Industry,dt,`Job Openings`,msa) %>% mutate(indicator="industry") %>%
  group_by(var,msa) %>%
  mutate(go_no_go=ifelse(base_date %in% dt,"go","no go"),
         per_change=ifelse(go_no_go=="go",(`Job Openings`-`Job Openings`[dt==base_date])/`Job Openings`[dt==base_date],NA)) %>%
  select(-go_no_go) %>% ungroup()

#### Education Level of Jobs and Candidates ####
setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/South Bend MSA/Education, Training and Experience/Education Level of Jobs and Candidates")

educ_1 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>%
    .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           `Potential Candidates`=as.numeric(str_remove(`Potential Candidates`,",")),
           `Job Openings`=as.numeric(str_remove(`Job Openings`,",")))
}) %>% bind_rows() %>% tibble()%>% mutate(msa="South Bend - Mishawaka")

setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Elkhart Goshen MSA/Education, Training and Experience/Education Level of Jobs and Candidates")

educ_2 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>%
    .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           `Potential Candidates`=as.numeric(str_remove(`Potential Candidates`,",")),
           `Job Openings`=as.numeric(str_remove(`Job Openings`,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="Elkhart - Goshen")

educ <- bind_rows(educ_1,educ_2)

base_date_educ <- '07.05.20'

df_educ <- educ %>% select(var=`Education Level`,dt,`Job Openings`,msa) %>% mutate(indicator="education") %>%
  group_by(var,msa) %>%
  mutate(go_no_go=ifelse(base_date_educ %in% dt,"go","no go"),
         per_change=ifelse(go_no_go=="go",(`Job Openings`-`Job Openings`[dt==base_date_educ])/`Job Openings`[dt==base_date_educ],NA)) %>%
  select(-go_no_go) %>% ungroup()


df <- bind_rows(df0,df1,df_educ)%>%
  mutate(dt=mdy(dt)) %>%
  arrange(dt) # arrange seems to be important to avoid an additional line connecting first and last observations

write_csv(df,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/df.csv")

# save a copf for the crc website with only SB MSA data
df %>%
  filter(msa=="South Bend - Mishawaka") %>%
  write_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/citi foundation grant/Economic complexity/ECI Dashboard (shared with CRC)/data and code (economy tracker)/df.csv")

rm(df,df0,df1,df_educ)

#### Candidate level data ####
#### Total Candidates ####
setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/South Bend MSA/Candidates/Candidates Available")

total_candidates_1 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>%
    .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           Candidates=as.numeric(str_remove(Candidates,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="South Bend - Mishawaka")

setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Elkhart Goshen MSA/Candidates/Candidates Available")

total_candidates_2 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>%
    .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           Candidates=as.numeric(str_remove(Candidates,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="Elkhart - Goshen")

total_candidates <- bind_rows(total_candidates_1,total_candidates_2)

#### Candidates By Occupation Group ####
setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/South Bend MSA/Candidates/Candidates By Occupation Group")

candidate_occ_grp_1 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>%
    .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           Candidates=as.numeric(str_remove(Candidates,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="South Bend - Mishawaka")

setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Elkhart Goshen MSA/Candidates/Candidates By Occupation Group")

candidate_occ_grp_2 <- lapply(list.files(), function(x) {
  read_html(x) %>%
    html_table(header = T) %>%
    .[[1]] %>%
    mutate(dt=str_remove(x,".xls"),
           Candidates=as.numeric(str_remove(Candidates,",")))
}) %>% bind_rows() %>% tibble() %>% mutate(msa="Elkhart Goshen")

candidate_occ_grp <- bind_rows(candidate_occ_grp_1,candidate_occ_grp_2)

base_date="06.26.20"

df0_cand <- total_candidates %>% select(dt,`Candidates`,msa) %>% mutate(indicator="Total Candidates") %>%
  group_by(msa) %>%
  mutate(go_no_go=ifelse(base_date %in% dt,"go","no go"),
         per_change=ifelse(go_no_go=="go",(`Candidates`-`Candidates`[dt==base_date])/`Candidates`[dt==base_date],NA)) %>%
  mutate(var="Total Candidates")%>%
  select(-go_no_go) %>% ungroup()

df2_cand <- candidate_occ_grp %>% select(var=`Occupation Group`,dt,`Candidates`,msa) %>% mutate(indicator="Occupation Group") %>%
  group_by(var,msa) %>%
  mutate(go_no_go=ifelse(base_date %in% dt,"go","no go"),
         per_change=ifelse(go_no_go=="go",(`Candidates`-`Candidates`[dt==base_date])/`Candidates`[dt==base_date],NA)) %>%
  select(-go_no_go) %>% ungroup()

# education level of candidates
df_educ_cand <- educ %>% select(var=`Education Level`,dt,Candidates=`Potential Candidates`,msa) %>% mutate(indicator="education") %>%
  group_by(var,msa) %>%
  mutate(go_no_go=ifelse(base_date_educ %in% dt,"go","no go"),
         per_change=ifelse(go_no_go=="go",(`Candidates`-`Candidates`[dt==base_date_educ])/`Candidates`[dt==base_date_educ],NA)) %>%
  select(-go_no_go) %>% ungroup()


df_cand <- bind_rows(df0_cand,df2_cand,df_educ_cand)%>%
  mutate(dt=lubridate::mdy(dt)) %>%
  arrange(dt)

write_csv(df_cand,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/df_cand.csv")

df_cand %>%
  filter(msa=="South Bend - Mishawaka") %>%
  write_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/citi foundation grant/Economic complexity/ECI Dashboard (shared with CRC)/data and code (economy tracker)/df_cand.csv")

rm(df_cand,df0_cand,df2_cand,df_educ_cand)

# employers

setwd("C:/Users/smotghar/OneDrive - nd.edu/Data/Job postings/South Bend MSA/Jobs/Employers by Number of Job Openings")

latest_date <- list.files() %>% mdy() %>% max()

file_name <- paste0(str_pad(month(latest_date),width=2,pad=0),".",str_pad(day(latest_date),width=2,pad=0),".",substr(year(latest_date),3,4),".xls")

read_html(file_name) %>%
  html_table(header = T) %>% .[[1]] %>%
  mutate(freq=log(`Job Openings`)) %>%
  select(word=`Employer Name`,freq) %>%
  write_rds("C:/Users/smotghar/OneDrive - nd.edu/notre dame research/sbeconomydb/employers_jobs.Rds")

### UI Claims

sj_all_industries <- read_excel("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/UI Claims/UI claims by industry/sj_all_industries_edit2.xlsx", na="NA") 

long_claims <- function(x) {
  x%>%
    set_names(slice(.,2)) %>%
    slice(-2) %>%
    mutate(dt=lubridate::mdy(.$Initial[1])) %>%
    slice(-1) %>%
    gather(key=claim_type,value=claims,-NaicsTitle,-dt)
}

all_claims <- lapply(seq(2,ncol(sj_all_industries), by=2), function(y) {
  sj_all_industries %>%
    select(1,y,y+1)%>%
    long_claims()
}) %>% bind_rows() %>% mutate(claims=as.numeric(claims)) %>%
  group_by(NaicsTitle, claim_type) %>%
  mutate(times_change=claims/claims[dt=="2020-03-07"]) %>%
  ungroup()

write_csv(all_claims,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/all_claims.csv")

#### Industry Structure
area_titles <- blscrapeR::area_titles %>% mutate(area_fips=as.character(area_fips))%>%
  mutate(area_title=str_remove(area_title," MSA"))
cleaned_industry_titles <- blscrapeR::naics %>% mutate(industry_title=str_remove(industry_title,"NAICS [0-9]+ ")) %>%
  mutate(industry_title=str_remove(industry_title,"NAICS [0-9]+-[0-9]+ "))

msa_qcew_2019_q4 <- read_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/QCEW/msa/msa_qcew_2019_q4.csv")

sb_naics_sector <- msa_qcew_2019_q4 %>%
  select(area_fips,own_code,industry_code,agglvl_code,size_code,year,qtr,month3_emplvl) %>%
  filter(agglvl_code==44) %>% # MSA, Private, by NAICS Sector
  group_by(area_fips,industry_code, year,qtr) %>% 
  summarise(bls_employment=sum(month3_emplvl, na.rm = T)) %>% # aggregate over own_code
  ungroup() %>%
  filter(industry_code!=9999) %>% # remove undefined industry
  left_join(area_titles, by="area_fips") %>%
  left_join(cleaned_industry_titles, by="industry_code")  %>% # lets keep this to emphasise these are MSAs
  select(msa_fips=area_fips,msa_name=area_title,industry_code,industry_title,bls_employment,year,qtr) %>%
  filter(msa_name=="South Bend-Mishawaka, IN-MI") # the total employment numbers are very low. checj that

write_csv(sb_naics_sector,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_naics_sector.csv")

### Occupations
sb_oes <- read_excel("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/OES/oesm19ma/oesm19ma/MSA_M2019_dl.xlsx") %>%
  filter(area_title=="South Bend-Mishawaka, IN-MI", o_group=="major") %>%
  mutate(tot_emp=as.numeric(tot_emp))

write_csv(sb_oes,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_oes.csv")

# Current Employment Survey

sb_ces_supersectors <- read_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/CES/sb_ces_supersectors_10_20.csv")

sb_ces_sup <- sb_ces_supersectors %>%
  filter(year(dt)>=2019) %>%
  group_by(industry_code) %>%
  mutate(last_yr_emp=lag(total_employment,12),change=total_employment-last_yr_emp) %>%
  mutate(per_change=(change)/last_yr_emp) %>%
  ungroup()

write_csv(sb_ces_sup,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_ces_sup.csv")

write_csv(sb_ces_sup,"C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/citi foundation grant/Economic complexity/ECI Dashboard (shared with CRC)/data and code (economy tracker)/sb_ces_sup.csv")


# labor force and employment
laus_sb_areas <- read_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/LAUS/laus_from_bls/laus_sb_areas.csv")

laus_sb <- laus_sb_areas %>%
  filter(year(dt)>=2019) %>%
  separate(series_title, into = c("measure","area"),sep = ": ") %>%
  group_by(area,measure) %>%
  mutate(last_yr_value=lag(value,12),change=value-last_yr_value,
    per_change=(value-last_yr_value)/last_yr_value) %>%
  ungroup()

write_csv(laus_sb,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/laus_sb.csv")
write_csv(laus_sb,"C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/citi foundation grant/Economic complexity/ECI Dashboard (shared with CRC)/data and code (economy tracker)/laus_sb.csv")


### housing
df <- read_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Building Permits Survey/building_permits_survey_2019_20.csv")

housing_sb <- df %>%
  filter(Name=="South Bend-Mishawaka  IN-MI")  %>% 
  separate(Date, into = c("YY", "MM"), sep = c(4)) %>% 
  mutate(dt=mdy(paste0(MM,"-1-",YY))) %>%
  select(dt,Bldgs,Units) %>%
  gather(key,total,-dt) %>% group_by(key) %>% mutate(per_change=(total-total[dt=="2020-01-01"])/total[dt=="2020-01-01"])

write_csv(housing_sb,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/housing_sb.csv")
write_csv(housing_sb,"C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/citi foundation grant/Economic complexity/ECI Dashboard (shared with CRC)/data and code (economy tracker)/housing_sb.csv")

### Eviction data ###
sb_evictions <- read_csv("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/evictions/covid evictions/all_sites.csv") %>%
  filter(str_detect(city,"South Bend"))

sb_weekly <- sb_evictions %>% group_by(week_date) %>%
  summarise(total_filings=sum(filings_2020),
            avg_filings=sum(filings_avg)) %>%
  mutate(per_change=(total_filings-avg_filings)/avg_filings)

sb_weekly %>% write_rds("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_weekly_evictions.Rds")

### spending data
setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Mastercard/Mastercard_censustracts070120/Data/")

all_tracts_long <- lapply(list.files(), function(x) {
  read_excel(x) %>%
    mutate(mnth=ymd(Month),tract=str_extract(x,"[0-9]+")) %>%
    select(-Month)
}) %>% bind_rows() %>%
  gather(key,value,-mnth,-tract) %>%
  separate(key,into=c("spending_type","category","geography","origin"),sep=" - ")%>%
  mutate(value=as.numeric(value)) %>%
  group_by(tract,spending_type,category,geography,origin) %>%
  mutate(per_change=(value-value[mnth=="2020-01-01"])/value[mnth=="2020-01-01"]) %>%
  ungroup()

write_csv(all_tracts_long,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/all_tracts_long.csv")

library(tigris)
library(leaflet)
library(sf)

options(tigris_use_cache = TRUE)

census_api_key("44e4e6c54a7cfc1414d184349d1f87c4c76b4cf7")

sb_census_tracts <- get_decennial(geography = "tract", variables = c(total_pop="P001001"),
                                  state="IN",county="141",
                                  year = 2010, geometry = T, output = "wide") %>%
  filter(GEOID %in% unique(all_tracts_spending$tract))

write_rds(sb_census_tracts,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_census_tracts.Rds")

# map for sb msa
sb_mi_msa <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
              variables = c(total_pop="B01003_001"),
              year = 2017, geometry = T, output = "wide") %>% 
  filter(GEOID==43780)

write_rds(sb_mi_msa,"C:/Users/Swapnil PC/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_mi_msa.Rds")

# business licenses
sb_businesses_upto_2018 <- read_csv("C:/Users/smotghar/OneDrive - nd.edu/Data/SB business licences/Business_Licenses.csv")

sb_businesses_post_2019 <- read_excel("C:/Users/smotghar/OneDrive - nd.edu/Data/SB business licences/BusinessLicenseReport2019-2020.xlsx", 
                                      sheet = "Reader Friendly")

# total licenses (new+renewal) over time

sb_business_licenses <- bind_rows(sb_businesses_upto_2018 %>%
                             mutate(mnth=mdy(paste0(month(ymd_hms(Issue_Date)),"-1-",year(ymd_hms(Issue_Date))))) %>%
                             group_by(mnth) %>%
                             summarise(issues=n()),
                           sb_businesses_post_2019 %>%
                             mutate(mnth=mdy(paste0(month(mdy(`Issued Date`)),"-1-",`Issued Year`))) %>%
                             group_by(mnth) %>%
                             summarise(issues=n()))

write_rds(sb_business_licenses,"C:/Users/smotghar/OneDrive - nd.edu/Data/Job postings/Job Postings App/sb_business_licenses.Rds")

rm(sb_businesses_upto_2018,sb_businesses_post_2019,sb_business_licenses)

