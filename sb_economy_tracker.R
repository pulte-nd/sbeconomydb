# in 13.4, I add content to the spending tab as spending by zip code
# also change the spending by census tract to represent all categories at once instead of allowing to select
# in 13.3, I add a tab for claimants profiles

library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(lubridate)
library(readxl)
library(dashboardthemes)

library(leaflet)
library(sf)
library(tigris)
library(tidycensus)

library(economiccomplexity) # for computing economic complexity
library(networkD3)

library(wordcloud2)

library(png)

# http://www.incontext.indiana.edu/2005/november/7.asp#:~:text=The%20best%20way%20to%20distinguish,establishment%20taken%20from%20payroll%20records.

#setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/sbeconomydb/")

df <- read_csv("df.csv")
df_cand <- read_csv("df_cand.csv")
employers_jobs <- read_rds("employers_jobs.Rds")

all_claims <- read_csv("all_claims.csv")
sb_naics_sector <- read_csv("sb_naics_sector.csv")
sb_oes <- read_csv("sb_oes.csv")
sb_elk_ces_sup <- read_csv("sb_elk_ces_sup.csv")
laus_select <- read_csv("laus_select.csv")

housing_sb <- read_rds("housing_sb.Rds")
sb_weekly_evictions <- read_rds("sb_weekly_evictions.Rds")
home_prices_in <- read_rds("home_prices_in.Rds")

# claimants files
age <- read_csv("claimants_age.csv")
gender <- read_csv("claimants_gender.csv")
education <- read_csv("claimants_education.csv")
industries <- read_csv("claimants_industries.csv")
occ_grps <- read_csv("claimants_occupations.csv")
total_claimants <- read_csv("unique_claimants.csv")

# spending files - census tract
all_tracts_long <- read_csv("all_tracts_long.csv")
sb_census_tracts <- read_rds("sb_census_tracts.Rds")
pal_select <- colorBin(palette = "viridis", domain = c(0,1), n = 2)

# #conomic Complexity files
selected_msa <- "South Bend-Mishawaka, IN-MI"
sb_mi_msa <- read_rds("sb_mi_msa.Rds")

# business licenses
sb_business_licenses <- read_rds("sb_business_licenses.Rds")


#setwd("C:/Users/Swapnil PC/OneDrive - nd.edu/notre dame research/citi foundation grant/Economic complexity/ECI Dashboard (shared with CRC)/data and code/")

#### Tab 1: Map ####



#### Tab 2: Industry Structure ####
msa_4digit_2019_q4 <- read_csv("msa_4digit_2019_q4.csv")

#### Tab 3: Industry Complexity ####
msa_4digit_2019_q4_ici <- read_csv("msa_4digit_2019_q4_ici.csv")

#### Tab 4: Economic Complexity ####
eci_over_yrs_rank <- read_csv("eci_over_yrs_rank.csv")

#### Tab 5: Industry growth in past 5 years ####
employment_growth <- read_csv("employment_growth.csv")

#### Tab 6: New Products ####
new_products <- read_csv("new_products.csv")

#### Tab 7: Industry Space ####
prod_net_d3 <- read_rds("prod_net_d3.Rds")

#### Tab 8: Opportunity gain vs distance ####
complexity_COI_gain_distance_sb <- read_csv("complexity_COI_gain_distance_sb.csv") 

#ui.R-----------------------------------------------------------------------------------------------------------------------------------------
ui = dashboardPage(#skin = "black", # blue is default but not too many options
                   dashboardHeader(#title = "South Bend Economy Tracker",
                                   titleWidth = 300,
                                   tags$li(a(href = 'https://www.citigroup.com/citi/foundation/',
                                             img(src = 'citi-foundation-title.png',
                                                 title = "Sponsor", height = "30px"),
                                             style = "padding-top:10px; padding-bottom:0px;"),
                                           class = "dropdown"),
                                   
                                   tags$li(a(href = 'http://www.pulte.nd.edu',
                                             img(src = 'logo.png',
                                                 title = "Company Home", height = "50px"),
                                             style = "padding-top:0px; padding-bottom:0px;"),
                                           class = "dropdown")),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Homepage", tabName = "homepage"),
                       menuItem("Economy Tracker", tabName = "economy_tracker", icon = icon("heartbeat"), startExpanded = TRUE,
                                menuSubItem("Labor Demand and Supply", tabName = "labor_market", icon = icon("users")),
                                menuSubItem("Employment and Unemployment", tabName = "emp_unemp", icon = icon("briefcase")),
                                menuSubItem("UI Claims", tabName = "ui_claims", icon = icon("money")),
                                menuSubItem("Housing", tabName = "housing", icon = icon("home")),
                                menuSubItem("Business Activity", tabName = "business_activity", icon = icon("business-time")),
                                menuSubItem("Structural Indicators", tabName = "structural", icon = icon("database"))
                                ),
                       menuItem("Economic Complexity", tabName = "economic_complexity", icon = icon("compress-arrows-alt"), startExpanded = TRUE,
                                menuSubItem("Industry Structure", tabName = "industry_structure", icon = icon("industry")),
                                menuSubItem("Industry Complexity", tabName = "industry_complexity", icon = icon("industry")),
                                menuSubItem("Economic Complexity", tabName = "eci_over_yrs", icon = icon("compress-arrows-alt")),
                                menuSubItem("Employment Growth", tabName = "emp_growth", icon = icon("chart-line")),
                                menuSubItem("New Products", tabName = "new_products", icon = icon("file")),
                                menuSubItem("Industry Space", tabName = "industry_space", icon = icon("project-diagram")),
                                menuSubItem("Growth Opportunities", tabName = "opportunity_gain", icon = icon("seedling"))
                                ),
                       menuItem("Info", tabName = "info", icon = icon("info-circle"),
                                menuSubItem("About", tabName = "about", icon = icon("address-card")),
                                menuSubItem("Data", tabName = "data", icon = icon("database")),
                                menuSubItem("Glossary", tabName = "glossary", icon = icon("glasses"))
                                )
                       ), # use collapsed = TRUE to hide dashboard menu
                     
                   width = 300), # end of sidebar
                   # Body content
                   dashboardBody(
                     # the following lines adds a background image
                     #tags$img(
                     #  src = "South_Bend_Flag.png",
                     #  style = 'position: absolute'
                     #),
                     tabItems(
                       tabItem(tabName = "labor_market",
                               fluidRow(
                                 column(
                                   h1("Labor Demand and Supply",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 valueBoxOutput("totaljobbox", width = 3),
                                 valueBoxOutput("jobchangebox", width = 3),
                                 valueBoxOutput("totalcandidatebox", width = 3),
                                 valueBoxOutput("candidatechangebox", width = 3)
                               ),
                               
                               fluidRow(
                                 column(
                                   p("The graphs show the daily number of job openings in the SB-Mishawaka MSA and the number of candidates looking for a job. 
                                     The jobs can be disaggregated into industry groups and the number of candidates can be disaggregated into occupation type.",
                                     br(),br(),
                                     "In a healthy labor market, there are at least as many jobs as people looking for them. 
                                     The ratio of the number of candidates to jobs less than or equal to 1 suggests enough jobs for everyone.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=8),
                                 valueBoxOutput("candidatestojobbox", width = 4)
                                 ),
                               fluidRow(selectInput(
                                 inputId = "select_msa",
                                 label = "Select MSA",
                                 choices = c("South Bend - Mishawaka", "Elkhart - Goshen")
                               )),
                               fluidRow(
                                 box(plotlyOutput(outputId = 'jobPlot'), 
                                     radioButtons("abs_per", "Select Y axis",
                                                  c("Total"="Job Openings",
                                                    "Percentage Change"="per_change"),
                                                  inline = T),
                                     radioButtons("ind", "Select Industry or education",
                                                  c("All Industries"="Total Openings",
                                                    "Health Care and Social Assistance"="Health Care and Social Assistance",                                       
                                                    "Retail Trade"="Retail Trade",                                                            
                                                    "Accommodation and Food Services"="Accommodation and Food Services",                                         
                                                    "Transportation and Warehousing"="Transportation and Warehousing",                                          
                                                    "Manufacturing"="Manufacturing",                                                           
                                                    "Less than High School"="Less than High School",
                                                    "High School Diploma or Equivalent"="High School Diploma or Equivalent",
                                                    "Bachelor's Degree"="Bachelor's Degree",
                                                    "Associate's Degree"="Associate's Degree",
                                                    "Master's Degree"="Master's Degree")),width = 6),
                                 box(plotlyOutput(outputId = 'candidatePlot'),
                                     radioButtons("abs_per_cand", "Select Y axis",
                                                  c("Total"="Candidates",
                                                    "Percentage Change"="per_change"), inline = T),
                                     radioButtons("cand", "Select Occupation group",
                                                  c("All Occupations"="Total Candidates",
                                                    "Production Occupations"="Production Occupations",
                                                    "Office and Administrative Support Occupations"="Office and Administrative Support Occupations",
                                                    "Management Occupations"="Management Occupations",	
                                                    "Transportation and Material Moving Occupations"="Transportation and Material Moving Occupations",	
                                                    "Sales and Related Occupations"="Sales and Related Occupations",
                                                    "Less than High School"="Less than High School",
                                                    "High School Diploma or Equivalent"="High School Diploma or Equivalent",
                                                    "Bachelor's Degree"="Bachelor's Degree",
                                                    "Associate's Degree"="Associate's Degree",
                                                    "Master's Degree"="Master's Degree")), width = 6)
                                 ),
                               fluidRow(
                                 box(plotlyOutput(outputId = 'indjobsPlot'), width = 6),
                                 box(plotlyOutput(outputId = 'candoccgrpPlot'), width = 6)
                               )
                       ),
                       tabItem(tabName = "ui_claims",
                               fluidRow(
                                 column(
                                   h1("Unemployment Insurance Claims",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 valueBoxOutput("total_ini_claims_box", width = 2),
                                 valueBoxOutput("change_ini_claims_box", width = 2),
                                 valueBoxOutput("total_cont_claims_box", width = 2),
                                 valueBoxOutput("change_cont_claims_box", width = 2)
                               ),
                               
                               fluidRow(
                                 column(
                                   #br(),
                                   p("The graph below shows the number of initial and continued unemployment claims starting first week of March 2020 to the most recent available. 
                                     Initial claims are those that were filed in the week, while continued claims are those that were filed in previous weeks but were continued in this week.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               
                               fluidRow(
                                 box(plotlyOutput(outputId = 'claims_plot'), 
                                     radioButtons("claims_selection", "Select Industry",
                                                  c("All Industries"="All Industries",
                                                    "Accommodation and Food Services"="Accommodation and Food Services",                 
                                                    "Administrative and Support and Waste Management"="Administrative and Support and Waste Management",
                                                    "Agriculture, Forestry, Fishing and Hunting"="Agriculture, Forestry, Fishing and Hunting",
                                                    "Arts, Entertainment, and Recreation"="Arts, Entertainment, and Recreation",
                                                    "Construction"="Construction",
                                                    "Educational Services"="Educational Services",
                                                    "Finance and Insurance"="Finance and Insurance",
                                                    "Health Care and Social Assistance"="Health Care and Social Assistance",
                                                    "Information"="Information",
                                                    "Management of Companies and Enterprises"="Management of Companies and Enterprises",
                                                    "Manufacturing"="Manufacturing",
                                                    "Mining, Quarrying, and Oil and Gas Extraction"="Mining, Quarrying, and Oil and Gas Extraction",
                                                    "Other Services (except Public Administration)"="Other Services (except Public Administration)",
                                                    "Professional, Scientific, and Technical Services"="Professional, Scientific, and Technical Services",
                                                    "Public Administration"="Public Administration",
                                                    "Real Estate and Rental and Leasing"="Real Estate and Rental and Leasing",
                                                    "Retail Trade"="Retail Trade",
                                                    "Transportation and Warehousing"="Transportation and Warehousing",
                                                    "Unclassified"="Unclassified",
                                                    "Utilities"="Utilities",
                                                    "Wholesale Trade"="Wholesale Trade")),width = 9)),
                               fluidRow(
                                 box(plotlyOutput(outputId = 'age_plot'),width = 4),
                                 box(plotlyOutput(outputId = 'gender_plot'),width = 4),
                                 box(plotlyOutput(outputId = 'education_plot'),width = 4)
                               ),
                               fluidRow(
                                 box(plotlyOutput(outputId = 'claimants_industry_plot'),width = 4),
                                 box(plotlyOutput(outputId = 'claimants_occupation_plot'),width = 4),
                                 box(plotlyOutput(outputId = 'unique_claimants_plot'),width = 4)
                               )
                       ),
                       tabItem(tabName = "emp_unemp",
                               fluidRow(
                                 column(
                                   h1("Employment and Unemployment",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 valueBoxOutput("totalemploymentbox", width = 3),
                                 valueBoxOutput("employmentchangebox", width = 3),
                                 valueBoxOutput("totallaborforce", width = 3),
                                 valueBoxOutput("unemploymentbox", width = 3)
                               ),
                               fluidRow(selectInput(
                                 inputId = "select_msa_emp",
                                 label = "Select MSA",
                                 choices = c("South Bend - Mishawaka", "Elkhart - Goshen")
                               )),
                               fluidRow(
                                 column(
                                   br(),
                                   p("The graph shows the employment by industry in the SB-Mishawaka MSA.", "As compared to a year before, total employment is down by",
                                     format(sb_elk_ces_sup$change[ sb_elk_ces_sup$industry_name=="Total Nonfarm" & sb_elk_ces_sup$dt==max(sb_elk_ces_sup$dt) & sb_elk_ces_sup$msa_name=="South Bend - Mishawaka"]*-1,big.mark=",",scientific=FALSE),"or",
                                     paste0(round(sb_elk_ces_sup$per_change[ sb_elk_ces_sup$industry_name=="Total Nonfarm" & sb_elk_ces_sup$dt==max(sb_elk_ces_sup$dt) & sb_elk_ces_sup$msa_name=="South Bend - Mishawaka"]*100,2),"%"),
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 width=6),
                                 column(
                                   br(),
                                   p("The graph shows the number of people in labor force and employed in SB-Mishawaka MSA and St. Joseph county. 
                                     The gap between the labor force and employment is the number of unemployed.",
                                     "As compared to a year before, number of unemployed is up by",
                                     format(laus_select$change[ laus_select$measure=="Unemployment" & laus_select$dt==max(laus_select$dt) & laus_select$msa_name=="South Bend - Mishawaka"],big.mark=",",scientific=FALSE), "or",
                                     paste0(round(laus_select$per_change[ laus_select$measure=="Unemployment" & laus_select$dt==max(laus_select$dt) & laus_select$msa_name=="South Bend - Mishawaka"]*100,2),"%"),
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=6)
                               ),
                               
                               fluidRow(box(plotlyOutput(outputId = 'cesPlot'),
                                            radioButtons("abs_per_ces", "Select Y axis",
                                                         c("Total"="total_employment",
                                                           "Percentage Change"="per_change"), inline = T),
                                            radioButtons("ces", "Select Industry",
                                                         c("Total"="Total Nonfarm",
                                                           "Education and Health Services"="Education and Health Services",
                                                           "Manufacturing"="Manufacturing", 
                                                           "Retail Trade"="Retail Trade",
                                                           "Leisure and Hospitality"="Leisure and Hospitality",
                                                           "Trade, Transportation, and Utilities"="Trade, Transportation, and Utilities",                                     
                                                           "Financial Activities"="Financial Activities",                
                                                           "Food Services and Drinking Places"="Food Services and Drinking Places")), width = 6),
                                        box(plotlyOutput(outputId = 'lausPlot'),
                                            radioButtons("abs_per_laus", "Select Y axis",
                                                         c("Total"="value",
                                                           "Percentage Change"="per_change"), inline = T), width = 6))),
                       tabItem(tabName = "housing",
                               fluidRow(
                                 column(
                                   h1("Housing Market",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 valueBoxOutput("totalbuildingbox", width = 3),
                                 valueBoxOutput("buildingchangebox", width = 3),
                                 valueBoxOutput("totalevictionbox", width = 3),
                                 valueBoxOutput("evictionchangebox", width = 3)
                               ),
                               
                               fluidRow(
                                 column(
                                   br(),
                                   p("A good indicator for how well the economy is doing is to see how the housing construction activity is performing.
                                   The graph shows the number of new housing permits issued in SB-Mishawaka MSA. 
                                     The number has been fairly constant across 2019-2020, suggesting that COVID did not decrease house construction activity.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=6),
                                 column(
                                   br(),
                                   p("The graph shows the number of eviction filings in South Bend, IN for the year 2020 and average for the years 2016-19. 
                                     The shaded regions are where there were orders against eviction in effect. 
                                     Here too, the number of eviction filings are lower in 2020 as compared to average of 2016-19 suggesting that the stay on evictions helped in reducing the filings.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=6)
                               ),
                               
                               fluidRow(box(plotlyOutput(outputId = 'housingPlot'),
                                            radioButtons("abs_per_housing", "Select Y axis",
                                                         c("Total"="total",
                                                           "Percentage Change"="per_change"), inline = T),
                                            width=6),
                                        box(plotlyOutput(outputId = 'evictionPlot'),width=6)
                               ),
                               fluidRow(selectInput(
                                 inputId = "select_msa_homevalue",
                                 label = "Select MSA",
                                 choices = c("South Bend - Mishawaka", "Elkhart - Goshen")
                               )),
                               fluidRow(
                                 valueBoxOutput("homevaluebox", width = 3),
                                 valueBoxOutput("homevaluechangebox", width = 3)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("The graph shows the home values.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=6)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'homevaluePlot'),
                                            radioButtons("abs_per_homevalue", "Select Y axis",
                                                         c("Value"="value",
                                                           "Percentage Change"="per_change"), inline = T),
                                            width=6)#,
                                        #box(plotlyOutput(outputId = 'evictionPlot'),width=6)
                               ),
                               ),
                       tabItem(tabName = "business_activity",
                               fluidRow(
                                 column(
                                   h1("Business Activity",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(selectInput(
                                 inputId = "select_msa",
                                 label = "Select MSA",
                                 choices = c("South Bend - Mishawaka", "Elkhart - Goshen")
                               )),
                               fluidRow(
                                 column(
                                   br(),
                                   p("Below is a wordcloud of employers currently hiring.", 
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),width=6),
                                 column(
                                   br(),
                                   p("The graph shows the number of business licenses issued in South Bend, IN.", 
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),width=6)
                               ),
                               fluidRow(box(wordcloud2Output(outputId = 'employers_wordcloud'), # wordcloud 0.2.1 seems to supress running plotly. do not update this package
                                            width=6),
                                        box(plotlyOutput(outputId = 'businessactivityPlot'),
                                            width=6)
                                        )
                              ),
                       tabItem(tabName = "structural",
                               fluidRow(
                                 column(
                                   h1("Structural Indicators",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("The graph below shows the employment in different sectors in SB-Mishawaka MSA. 
                                     The size of the boxes is proportional to the number of people employed by the industry.
                                     The largest sectors are Manufacturing, Accommodation and Food Services, and Retail Trade.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=6),
                                 column(
                                   br(),
                                   p("The graph below shows the employment in different occupations in SB-Mishawaka MSA. 
                                   The size of the boxes is proportional to the number of people employed in the occupation.
                                   The most common occupations are Office and Administrative Support Occupations, Sales and Related Occupations, Food Preparation and Serving Related Occupations and Production Occupations.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=6)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'industryPlot'), width = 6),
                                   box(plotlyOutput(outputId = 'occupationPlot'), width = 6)
                                   )
                               ),
                       tabItem(tabName = "industry_structure",
                               
                               fluidRow(
                                 column(
                                   h1("Industry Structure",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("South Bend-Mishawaka MSA employed approximately 122,000 individuals in Q2 2020.",br(),
                                #     "The three largest industries are Restaurants and other eating places, Management of companies and enterprises, and Grocery stores.",br(), 
                                     "The graph below shows the employment in different industries in SB-Mishawaka MSA. The size of the boxes is proportional to the number of people employed by the industry. The industries in the same sector are grouped together and have the same color.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'industryPlot_ec'),width = 12))
                       ),
                       tabItem(tabName = "industry_complexity",
                               fluidRow(
                                 column(
                                   h1("Industry Complexity",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("Industry Complexity Index is a rank of Industries based on how likely the industry is observed with other industries. 
                                   The largest industries in SB-Mishawaka MSA have low complexity.",br(),
                                     "The graph shows the industry complexity index of different industries in SB-Mishawaka MSA. 
                                     The size of the box is proportional to the number of people employed by the industry. 
                                     The industries in the same sector are grouped together and have the same color.",br(),
                                     "The industry complexity index varies from -1.47 to +2.48. A higher complexity index indicates a complex industry.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'industryCompPlot'), width = 12))
                       ),
                       tabItem(tabName = "eci_over_yrs",
                               fluidRow(
                                 column(
                                   h1("Economic Complexity",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("Economic Complexity Index (ECI) is a rank of Metropolitan Statistical Areas (MSAs) based on how diversified and complex products they produce.
                                   South Bend-Mishawaka MSA's ECI ranking in 2014 Q1 was 235 and its ranking in 2019 was 243. The ranking has thus worsened by 8 positions across time.
                                     The graph below shows the evolution of ECI ranking for SB-Mishawaka MSA and few other MSA's around it.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'eciPlot'),width = 12))
                       ),
                       tabItem(tabName = "emp_growth",
                               fluidRow(
                                 column(
                                   h1("Employment Growth",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("SB-Mishawaka MSA has seen a static pattern of industry/ employment growth, with the largest contribution to industry growth coming from moderate complexity industries, particularly Beverage Manufacturing, and RV Parks and Recreational Camps industries.
                                   The graph below shows the industry growth by industry complexity for all industries in SB-Mishawaka MSA. Industries in same sector have the same color.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'emp_growth_Plot'),width = 12))
                       ),
                       tabItem(tabName = "new_products",
                               fluidRow(
                                 column(
                                   h1("New products",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("Economic growth is driven by diversification into new industries that are incrementally more complex. 
                                   South Bend-Mishawaka MSA has added 25 new industries since 2015 and these industries employ approximately 12,000 individuals.
                                   The graph below shows the employment in new industries in SB-Mishawaka MSA. The size of the boxes is proportional to the number of people employed by the industry.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'new_products_Plot'),width = 12))
                       ),
                       tabItem(tabName = "industry_space",
                               fluidRow(
                                 column(
                                   h1("Industry Space",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("SB-Mishawaka MSA's Industry Space illustrates the relatedness of its industries and potential paths to diversify its economy.",br(),
                                   "Each node represents an industry. A connection between nodes/ industries implies that these industries are frequently observed together in other MSAs. 
                                   Presence of an industry in South Bend-Mishawaka MSA is indicated by a colored node. While the absence is indicated by a gray node. Industries in the same sector have same color",br(),br(),
                                   "Creating a new industry is akin to turning a gray node to colored. Gray nodes that are connected to colored nodes (existing industries) are easier to develop than gray nodes unconnected to colored nodes. Thus, diversification would be faster by connecting to gray nodes that are further connected to other colored nodes.", br(),br(),
                                   "Click and zoom on the graph below to explore the industry space",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(forceNetworkOutput(outputId = 'industry_space_Plot'),width = 12))
                       ),
                       tabItem(tabName = "opportunity_gain",
                               fluidRow(
                                 column(
                                   h1("Growth Opportunities",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p("Regions grow by diversifying into new industries of increasing complexity. Strategic new industries aim to balance:",br(),
                                     strong(".	Distance to existing capabilities:")," lower distance (close to 0) signifies an industry is ",em("nearby"),"to existing knowhow.",br(),
                                     strong(".	Complexity:")," more complex industries tend to support higher wages.",br(),
                                            strong(".	Opportunity gain for future diversification:")," higher values hold more linkages to other high-complexity industries, opening more opportunities for continued diversification.",br(),
"The figures show the distribution of industries. The industries highlighted in yellow are most prime to be developed. These are (i) more complex than SB-Mishawaka MSA's ECI, and (ii) have more linkages to other new industries. Adding them would improve SB-Mishawaka MSA ECI and possibilities of diversifying into even more industries.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               ),
                               fluidRow(box(plotlyOutput(outputId = 'opportunity_gain_Plot'),width = 6),
                                        box(plotlyOutput(outputId = 'product_complexity_Plot'),width = 6))
                       )
                       ,
                       tabItem(tabName = "about",
                               fluidRow(
                                 column(
                                   h1("About",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12),
                               ),
                               fluidRow(
                                 column(
                                   #br(),
                                   p("This website has been developed by the ",tags$a(href = "https://pulte.nd.edu/", "Pulte Institute for Global Development")," at the University of Notre Dame 
                                     in collaboration with the City of South Bend. Funding was provided by the Citi Foundation.",
                                     br(),br(),
                                     "The objective is to (i)	provide clear visibility of the state of the South Bend - Mishawaka MSA economy and 
                                     (ii)	suggest industries grow the economy.",
                                     br(),br(),
                                     "The Economy tracker data will be updated monthly while the complexity analysis will be updated quarterly.",
                                     br(),br(),
                                     "The Economic Complexity Analysis was originally developed by Hausmann et al. (2014) to understand growth potential for countries. The methodology was adapted for metro areas by Escobari et al. (2019). 
                                     The website design is influenced by ",tags$a(href = "https://atlas.cid.harvard.edu/", "Atlas of Economic Complexity"),
                                     br(),br(),
                                     "All the data and the code is available ",tags$a(href = "https://github.com/pulte-nd/sbeconomydb", "here."),
                                     "For any feedback/questions, please ",tags$a(href = "mailto:smotghare@nd.edu", "contact us."),
                                     #br(),br(),
                                     #"Download newsletter ",tags$a(href = "my-report.pdf", "here"),
                                     #br(),br(),
                                     #a(href="my-report.pdf", "Download PDF", download=NA, target="_blank"),
                                     br(),br(),
                                     "References",
                                     br(),
                                     "   Hausmann, R., Hidalgo, C. A., Bustos, S., Coscia, M., and Simoes, A. (2014). The atlas of economic complexity: Mapping paths to prosperity. MIT Press.",
                                     br(),
                                     "   Escobari, M., Seyal, I., Morales-Arilla, J., and Shearer, C. (2019). Growing cities that work for all: A capability-based approach to regional economic competitiveness.",
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                                 )
                               ),
                       tabItem(tabName = "data",
                               fluidRow(
                                 column(
                                   h1("Data Sources",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   #br(),
                                   p("Data on this website is sourced from publicly available databases including",
                                     br(),br(),
                                     tags$a(href = "https://www.bls.gov/", "U.S. Bureau of Labor Statistics"),br(),
                                     tags$a(href = "https://www.bea.gov/", "U.S. Bureau of Economic Analysis"),br(),
                                     tags$a(href = "https://www.indianacareerconnect.com/", "Indiana Career Connect"),br(),
                                     tags$a(href = "https://www.census.gov/", "U.S. Census Bureau"),br(),
                                     tags$a(href = "http://www.hoosierdata.in.gov/", "Hoosiers by the Numbers"),br(),
                                     tags$a(href = "https://www.in.gov/dwd/", "Indiana Department of Workforce Development"),br(),
                                     tags$a(href = "https://evictionlab.org/", "The Eviction Lab"),br(),
                                     tags$a(href = "https://southbendin.gov/", "The City of South Bend"),br(),
                                     tags$a(href = "https://www.zillow.com/", "Zillow"),br(),
                                     
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               )
                       ),
                        tabItem(tabName = "homepage",
                                fluidRow(
                                  column(
                                    br(),
                                    h1("South Bend Economy Database",
                                       style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px") ,br(),
                                    h4(
                                     "Research and Data Visualization tools to understand the economic dynamics and new growth opportunities for South Bend, Indiana.", br(), br()
                                     ,
                                      style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                                     ),
                                    width=6),
                                  column(
                                    box(
                                      status = "primary", solidHeader = F, width = 12,
                                      imageOutput("myImage", height = "auto", width = "auto")), 
                                    width = 6)
                                ),
                                
                                fluidRow(
                                  column(leafletOutput(outputId = 'mapPlot_msa'),width = 6),
                                  column(
                                    br(),
                                    p("South Bend-Mishawaka Metropolitan Statistical Area (SB-Mishawaka MSA), sometimes referred to as Michiana, 
                                     is an area consisting of two counties - one in northern Indiana (St. Joseph) and one in southwest Michigan (Cass). 
                                     It is anchored by the cities of South Bend and Mishawaka in Indiana.", br(), br(),
                                      "It has a population of 323,613 (as of 2019) and has a 216 th largest GDP among all MSAs (as of 2018).",br(), br(),
                                      "With an Economic Complexity Index (ECI) of 0.0691, it ranks as the 243rd most complex MSA in the ECI ranking of 388 MSAs. 
                                     Compared to five years prior, SB-Mishawaka MSA's economy has become less complex, worsening 8 positions in the ECI ranking.",br(), br(),
                                      "Click on the links in the left pane to explore the region.",
                                      style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                    width=6)
                                  
                                
                                )),
                       tabItem(tabName = "glossary",
                               fluidRow(
                                 column(
                                   h1("Glossary",
                                      style="text-align:justify;color:white;background-color:darkblue;padding:15px;border-radius:10px"),br(),
                                   width=12)
                               ),
                               fluidRow(
                                 column(
                                   br(),
                                   p(strong("Economic Complexity:")," A measure of the knowledge in a society as expressed in the products it makes.",br(),
                                     strong("Economic Complexity Index (ECI):")," A rank of Metropolitan Statistical Areas (MSAs) based on how diversified and complex products they produce.",br(),
                                     strong("Industry Complexity Index (ICI):")," A rank of Industries based on how likely the industry is observed with other industries.",br(),
                                     strong("Industry Space:")," Industry Space illustrates the relatedness of its industries and potential paths to diversify its economy.",br(),
                                     strong("Opportunity gain:")," Measures how much a location could benefit in opening future diversification opportunities by developing a particular industry.",br(),
                                     strong("Distance to existing capabilities:")," This measures the difficulty of developing a new industry. A lower distance signifies an industry is", em("nearby")," to existing knowhow and hence the industry can be developed relatively easily.",br(),
                                     strong("Opportunity Gain:")," Opportunity gain for future diversification: higher values hold more linkages to other high-complexity industries, opening more opportunities for continued diversification.",
                      
                                     style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                   width=12)
                               )
                       )
                       
                   )
) # end of dashboardbody
) #end dashboardPage




#server.R  -------------------------------------------------------------------------------------------------

server = function(input, output, session) {
  
  output$jobPlot <- renderPlotly({
    
    fig <- df %>%
      filter(var==input$ind,
             msa==input$select_msa) %>%
      plot_ly(x=~dt, y=~get(input$abs_per), 
              type="scatter",mode="lines",
              line = list(shape = 'spline'), # more data would make this look nicer
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                          '</br> Job Openings: ', `Job Openings`,
                          '</br> Percentage Change: ', paste(round(per_change*100,2),"%"))) %>% 
      layout(showlegend = FALSE,
             title = paste0("Job Openings in ",input$select_msa," Metropolitan Statistical Area"),
             xaxis = list(title = "",range(mdy("06.25.20"),Sys.Date())), # use zeroline = FALSE to remove zero line :)
             yaxis = list(title=""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: www.indianacareerconnect.com",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2)
             )
    
    if(input$abs_per=="per_change") {
      fig <- fig%>% 
        layout(yaxis = list(tickformat = "%"))
    }
    
    
    fig
    
  })
  
  output$indjobsPlot <- renderPlotly({
    
    latest_date=max(df$dt)
    
    fig <- df %>%
      filter(indicator=="industry",
             msa==input$select_msa) %>%
      filter(dt==max(dt)) %>%
      filter(dt==max(dt), indicator=="industry") %>%
      mutate(per=`Job Openings`/sum(`Job Openings`)) %>%
      plot_ly(
        labels = ~ paste0(var," (",round(per*100,0),"%",")"),
        parents = NA,
        values = ~ `Job Openings`,
        type = 'treemap',
        hovertemplate = "%{label}<br>Job openings: %{value}<extra></extra>"#,
        #text=~paste('Total Jobs =', bls_employment,
        #'<br>Percentage =',paste(round(per_jobs*100,0),"%"))
      ) %>%
      layout(autosize = TRUE,
             title = paste0("Job Openings by Industry in ",input$select_msa," as of ",format(latest_date,"%b %d, %Y")),
             title = paste0("Job Openings by Industry in South Bend-Mishawaka MSA as of ",format(latest_date,"%b %d, %Y")),
             margin = list(l = 0, r = 0, t = 30, b = 30),
             annotations = list(text = "Source: www.indianacareerconnect.com",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = 0.01,
                                yref = 'paper', y = -0.05))
    
    
    fig
  })
  
  output$candidatePlot <- renderPlotly({
    
    fig <- df_cand %>%
      filter(var==input$cand,
             msa==input$select_msa)  %>%
      plot_ly(x=~dt, y=~get(input$abs_per_cand),
              type="scatter",mode="lines",
              line = list(shape = 'spline'),
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                '</br> Candidates: ', Candidates,
                          '</br> Percentage Change: ', paste(round(per_change*100,2),"%")))%>% 
      layout(showlegend = FALSE,
             title = paste0("Candidates in ",input$select_msa," Metropolitan Statistical Area"),
             xaxis = list(title = "",range(mdy("06.25.20"),Sys.Date())), # use zeroline = FALSE to remove zero line :)
             yaxis = list(title=""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: www.indianacareerconnect.com",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
    
    if(input$abs_per_cand=="per_change") {
      fig <- fig%>% 
        layout(yaxis = list(tickformat = "%"))
    }
    
    
    fig
  })
  
  output$candoccgrpPlot <- renderPlotly({
    
    latest_date=max(df_cand$dt)
    
    fig <- df_cand %>%
      filter(indicator=="Occupation Group",
             msa==input$select_msa) %>%
      filter(dt==max(dt)) %>%
      filter(dt==max(dt), indicator=="Occupation Group") %>%
      mutate(per=Candidates/sum(Candidates)) %>%
      plot_ly(
        labels = ~ paste0(var," (",round(per*100,0),"%",")"),
        parents = NA,
        values = ~ Candidates,
        type = 'treemap',
        hovertemplate = "%{label}<br>Candidates: %{value}<extra></extra>"#,
        #text=~paste('Total Jobs =', bls_employment,
        #'<br>Percentage =',paste(round(per_jobs*100,0),"%"))
      ) %>%
      layout(autosize = TRUE,
             title = paste0("Candidates by Occupation in ",input$select_msa," as of ",format(latest_date,"%b %d, %Y")),
             title = paste0("Candidates by Occupation in South Bend-Mishawaka MSA as of ",format(latest_date,"%b %d, %Y")),
             margin = list(l = 0, r = 0, t = 30, b = 30),
             annotations = list(text = "Source: www.indianacareerconnect.com",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = 0.01,
                                yref = 'paper', y = -0.05))
    
    
    fig
  })
  
  
  output$cesPlot <- renderPlotly({
    fig <- sb_elk_ces_sup %>%
      filter(industry_name==input$ces,
             msa_name==input$select_msa_emp) %>%
      plot_ly(x=~dt, y=~get(input$abs_per_ces),
              type="scatter",mode="lines", line = list(shape = 'spline'),
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                          '</br> Employment: ', total_employment,
                          '</br> Percentage Change: ', paste(round(per_change*100,2),"%"))) %>% 
      layout(title = paste0("Employment by Industry in ",input$select_msa_emp," MSA"),
             xaxis = list(title = ""),
             yaxis = list(title=""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: State and Metro Area Employment, BLS",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
    
    if(input$abs_per_ces=="per_change") {
      fig <- fig%>% 
        layout(yaxis = list(tickformat = "%"))
    }
    
    fig
  })
  
  output$lausPlot <- renderPlotly({
    fig <- laus_select %>%
      filter(msa_name==input$select_msa_emp,
             (measure=="Labor Force" | measure=="Employment")) %>%
      plot_ly(x=~dt, y=~get(input$abs_per_laus), 
              split = ~measure,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                          '</br> Employment: ', value,
                          '</br> Percentage Change: ', paste(round(per_change*100,2),"%"))) %>% 
      layout(title = "Labor force, employment and unemployment",
             xaxis = list(title = ""),
             yaxis = list(title=""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Local Area Unemployment Statistics, BLS",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
    
    if(input$abs_per_laus=="per_change") {
      fig <- fig%>% 
        layout(yaxis = list(tickformat = "%"))
    }
    
    fig
  })
  
  output$claims_plot <- renderPlotly({
    all_claims %>%
      filter(NaicsTitle==input$claims_selection) %>%
      arrange(dt) %>% # this is important!
      plot_ly(x=~dt, y=~claims,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~claim_type,
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                          '</br> Claims: ', claims,
                          '</br> Times Change: ', round(times_change,2)))%>% 
      layout(title = "Initial and Continued Claims in St. Joseph County",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: https://www.hoosierdata.in.gov/infographics/weekly-unemployment-claims.asp",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$age_plot <- renderPlotly({
    age %>%
      arrange(mnth) %>% 
      plot_ly(x=~mnth, y=~Claimants,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~`Age Group`,
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Claimants: ', Claimants))%>% 
      layout(title = "Monthly claims by Age",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Indiana Department of Workforce Development",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$gender_plot <- renderPlotly({
    gender %>%
      filter(!is.na(Category)) %>%
      arrange(mnth) %>% # this is important!
      plot_ly(x=~mnth, y=~Claimants,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~Category,
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Claimants: ', Claimants))%>% 
      layout(title = "Monthly claims by Gender",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Indiana Department of Workforce Development",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$education_plot <- renderPlotly({
    education %>%
      filter(Category!="Unknown") %>%
      arrange(mnth) %>% 
      plot_ly(x=~mnth, y=~Claimants,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~Category,
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Claimants: ', Claimants))%>% 
      layout(title = "Monthly claims by Education",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Indiana Department of Workforce Developmentp",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$claimants_industry_plot <- renderPlotly({
    industries %>%
      arrange(mnth) %>% 
      plot_ly(x=~mnth, y=~Claimants,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~`NAICS Title`,
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Industry: ', `NAICS Title`,
                          '</br> Claimants: ', Claimants))%>% 
      layout(title = "Monthly claims by Industry",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             showlegend=F,
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Indiana Department of Workforce Development",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$claimants_occupation_plot <- renderPlotly({
    occ_grps %>%
      arrange(mnth) %>% 
      plot_ly(x=~mnth, y=~Claimants,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~`Occupational Group Description`,
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Occupation: ', `Occupational Group Description`,
                          '</br> Claimants: ', Claimants))%>% 
      layout(title = "Monthly claims by Occupation",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             showlegend=F,
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Indiana Department of Workforce Development",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$unique_claimants_plot <- renderPlotly({
    total_claimants %>%
      gather(Category,value,-mnth) %>%
      arrange(mnth) %>% 
      plot_ly(x=~mnth, y=~value,
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~`Category`,
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Occupation: ', Category,
                          '</br> Claimants: ', value))%>% 
      layout(title = "Monthly claims by claim type",
             xaxis = list(title = ""),
             yaxis = list(title="Claims"),
             showlegend=F,
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Indiana Department of Workforce Development",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$housingPlot <- renderPlotly({
    fig <- housing_sb %>%
      filter(year(dt)>=2019) %>%
      #filter(key=="Bldgs") %>%
      plot_ly(x=~dt, y=~get(input$abs_per_housing),
              type="scatter",mode="lines", line = list(shape = 'spline'),
              split=~key,
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                          '</br> Permits: ', total))%>% 
      layout(title = "New permits issued in SB-Mishawaka MSA",
             xaxis = list(title = ""),
             yaxis = list(title=""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: Building Permits Survey, US Census Bureau",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
    
    if(input$abs_per_housing=="per_change") {
      fig <- fig%>% 
        layout(yaxis = list(tickformat = "%"))
    }
    
    fig
  })
  
  output$homevaluePlot <- renderPlotly( {
    fig <- home_prices_in %>%
      filter(msa==input$select_msa_homevalue) %>%
      plot_ly(x=~dt, y=~get(input$abs_per_homevalue), 
              type="scatter",mode="lines",
              line = list(shape = 'spline'), # more data would make this look nicer
              hoverinfo = 'text',
              text=~paste('</br> Date: ', dt,
                          '</br> Home value: ', `value`,
                          '</br> Percentage Change: ', paste(round(per_change*100,2),"%"))) %>% 
      layout(showlegend = FALSE,
             title = paste0("Home values in ",input$select_msa_homevalue," Metropolitan Statistical Area"),
             xaxis = list(title = ""), # use zeroline = FALSE to remove zero line :)
             yaxis = list(title = ""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: www.zillow.com",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2)
      )
    
    if(input$abs_per_homevalue=="per_change") {
      fig <- fig%>% 
        layout(yaxis = list(tickformat = "%"))
    }
    
    
    fig
  })
  
  output$businessactivityPlot <- renderPlotly({
    fig <- sb_business_licenses %>%
      filter(mnth>=ymd("2015-02-01")
      ) %>%
      plot_ly(x=~mnth, y=~issues,#y=~get(input$abs_per), 
              type="scatter",mode="lines",
              line = list(shape = 'spline'), # more data would make this look nicer
              hoverinfo = 'text',
              text=~paste('</br> Month: ', mnth,
                          '</br> Licenses Issued: ', `issues`)) %>% 
      layout(showlegend = FALSE,
             title = "Business Licenses isssued (new and renewals) in South Bend, IN",
             xaxis = list(title = ""), # use zeroline = FALSE to remove zero line :)
             yaxis = list(title=""),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: City of South Bend",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2)
      )
    
    fig
  })
  
  output$employers_wordcloud <- renderWordcloud2({
    employers_jobs %>%
      filter(msa==input$select_msa) %>% select(-msa) %>%
    wordcloud2(size=0.3, color='random-light', 
                      backgroundColor="black", minRotation = 0, maxRotation = 0, rotateRatio = 1)
  })
  
  
  output$evictionPlot <- renderPlotly({
    
  fig <- sb_weekly_evictions %>%
      plot_ly(x=~week_date, y=~total_filings, 
              type="scatter",mode="lines",
              line = list(shape = 'spline'), # more data would make this look nicer
              hoverinfo = 'text',
              text=~paste('</br> Week ending: ', week_date,
                          '</br> Eviction filings: ', `total_filings`),
              name="Total eviction filings") %>%
      add_trace(y=~avg_filings, name="Average filings 2016-19",
                text=~paste('</br> Week ending: ', week_date,
                            '</br> Average Eviction filings: ', `avg_filings`))%>%
      layout(title = "Weekly eviction filings in South Bend, IN",
             xaxis = list(title = ""), # use zeroline = FALSE to remove zero line :)
             yaxis = list(title = ""),
             legend = list(#shapes=list(type='line', y0= mdy("07-01-2020"),  x0=120, line=list(dash='dot', width=1)),
               orientation = "h",   # show entries horizontally
               xanchor = "center",  # use center of legend as anchor
               x = 0.5,y=-0.1),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: www.evictionlab.org",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.25),
             shapes = list(
               list(type = "rect",
                    fillcolor = "green", line = list(color = "green"), opacity = 0.3,
                    x0 = "2020-03-19", x1 = "2020-08-14", xref = "x",
                    y0 = 0, y1 = 100, yref = "y"),
               list(type = "rect",
                    fillcolor = "yellow", line = list(color = "yellow"), opacity = 0.3,
                    x0 = "2020-09-04", x1 = "2021-06-30", xref = "x",
                    y0 = 0, y1 = 100, yref = "y"))
      )
    
    
    fig
  })
  
  
  
  output$industryPlot <- renderPlotly({
    sb_naics_sector %>%
      mutate(per_jobs=bls_employment/sum(bls_employment)) %>%
      plot_ly(
        labels = ~ paste('Industry=',industry_title),
        parents = NA,
        values = ~ bls_employment,
        type = 'treemap',
        hovertemplate = "Industry: %{label}<br>Employment: %{value}<extra></extra>",
        text=~paste('Total Jobs =', bls_employment,
                    '<br>Percentage =',paste(round(per_jobs*100,0),"%"))) %>%
      layout(autosize = TRUE, #margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: 2019 Quarterly Census of Employment and Wages, BLS",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  
  output$occupationPlot <- renderPlotly({
    sb_oes %>%
      mutate(per_jobs=tot_emp/sum(tot_emp)) %>%
      plot_ly(
        labels = ~ paste('Occupation=',occ_title),
        parents = NA,
        values = ~ tot_emp,
        type = 'treemap',
        hovertemplate = "Occupation: %{label}<br>Employment: %{value}<extra></extra>",
        text=~paste('Total Employment =', tot_emp,
                    '<br> Percentage =',paste(round(per_jobs*100,0),"%"))) %>%
      layout(autosize = TRUE, #margin = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
             margin = list(l = 50, r = 50, t = 60, b = 60),
             annotations = list(text = "Source: 2019 Occupational Employment Statistics, BLS",
                                font = list(size = 12),
                                showarrow = FALSE,
                                xref = 'paper', x = -0.03,
                                yref = 'paper', y = -0.2))
  })
  

  
# Economic Complexity #
output$mapPlot_msa <- renderLeaflet({
  
  sb_mi_msa %>%
    #mutate(color_var=ifelse(GEOID==43780,1,0)) %>%
    st_transform() %>% # Transform or convert coordinates of simple feature
    leaflet() %>%
    addProviderTiles(provider = "CartoDB.Positron") %>% # free third-party basemaps
    addPolygons(popup = ~ NAME, # the text to show when clicked
                stroke = FALSE,
                smoothFactor = 0.5,
                fillOpacity = 0.5,
                weight = 1,
                fillColor = "green",
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    flyTo(lng = -86.17639118581819 , lat = 41.738542466611605, zoom = 8)
  
})

output$industryPlot_ec <- renderPlotly( {
  msa_4digit_2019_q4 %>%
    filter(msa_name==selected_msa) %>%
    mutate(two_digit_industry_code=substr(industry_code,1,2)) %>%
    plotly::plot_ly(
      labels = ~ industry_title,
      parents = NA,
      values = ~ bls_employment,
      type = 'treemap',
      hovertemplate = "Industry: %{label}<br>Employment: %{value}<extra></extra>"
    )
})

output$industryCompPlot <- renderPlotly( {
  msa_4digit_2019_q4_ici %>%
    filter(msa_name==selected_msa) %>%
    mutate(two_digit_industry_code=substr(industry_code,1,2)) %>%
    plotly::plot_ly(
      labels = ~ industry_title,
      parents = NA,
      values = ~ bls_employment,
      type = 'treemap',
      #hovertemplate = "Industry: %{label}<br>Complexity: %{ici}",
      text=~paste('</br> Complexity index: ', round(ici,2)),
      marker=list(colors=~ici)) %>%
    layout(showlegend=T)
    
})

output$eciPlot <- renderPlotly( {
  fig <- eci_over_yrs_rank %>%
    plot_ly(x = ~yr_qtr, y = ~eci_rank,type = "scatter", mode = "line", split =~msa_name, color = ~color_var, fill=~color_var,
            text=~eci_rank, label="text", colors = c("grey", "red"), opacity=0.8,
            line = list(shape = 'spline')) %>%
    hide_legend() %>% hide_colorbar() %>% 
    layout(yaxis = list(title="ECI Rank",
                        autorange = "reversed", zeroline=F),
           title = paste0("Evolution of ECI for ",selected_msa),
           xaxis = list(title = "Quarter")) # use zeroline = FALSE to remove zero line 
  
  fig
})

output$emp_growth_Plot <- renderPlotly( {
  fig <- filter(employment_growth, msa_name==selected_msa) %>%
    plot_ly(x = ~ici, y = ~cagr,opacity = 0.8,
            color = ~industry_title,        
            text = ~industry_title, 
            hoverinfo = "text",
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 20))
  fig <- fig %>% layout(title = selected_msa,
                        xaxis = list(title = "Product Complexity"), # use zeroline = FALSE to remove zero line :)
                        yaxis = list(title="Annual employment growth (2014-2019)"))
  
  fig <- fig %>% hide_legend()
  
  fig
})

output$new_products_Plot <- renderPlotly( {
  p <- plot_ly(
    new_products,
    labels = ~ industry_title,
    parents = NA,
    values = ~ bls_employment,
    type = 'treemap',
    hovertemplate = "Industry: %{label}<br>Employment: %{value}<extra></extra>"
  )
  
  p
})

output$industry_space_Plot <- renderForceNetwork ({
  forceNetwork(Links = prod_net_d3$links, Nodes = prod_net_d3$nodes,
               Source = 'source', Target = 'target', NodeID = 'name', 
               Nodesize = "Nodesize", radiusCalculation = JS("Math.sqrt(d.nodesize/10000)+6"),
               Group = 'group', opacity = 1, opacityNoHover=1, charge = -5, fontFamily = "calibri",
               colourScale = 'd3.scaleOrdinal().domain(["absent","Accommodation and Food Services","Administrative and Support and Waste Management and Remediation Services",
             "Agriculture, Forestry, Fishing and Hunting","Construction","Educational Services","Finance and Insurance","Manufacturing",
             "Information","Health Care and Social Assistance","Other Services (except Public Administration)","Professional, Scientific, and Technical Services",
             "Real Estate and Rental and Leasing","Retail Trade","Transportation and Warehousing","Utilities","Wholesale Trade"]).
             range(["grey","darkgreen","darkmagenta","darkorange","deepskyblue","firebrick","gold","deeppink","green","ndianred","khaki","lavender","lightcoral",
             "lightsalmon","lightseagreen","maroon","seashell"])',legend = T, zoom=T,
               height = 700,width = 1200)
  
})

output$opportunity_gain_Plot <- renderPlotly( {
  fig <- plot_ly(complexity_COI_gain_distance_sb,x = ~distance, y = ~COI_gain, opacity=0.5,
                 color = ~h_light_coi, 
                 text = ~industry_title, 
                 hoverinfo = "text",
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10)
  ) 
  
  fig <- fig %>% 
    hide_legend() %>% hide_colorbar() 
  
  fig <- fig %>% 
    layout(title = selected_msa,
           xaxis = list(title = "Distance", zeroline=F), # use zeroline = FALSE to remove zero line :)
           yaxis = list(title="Opportunity Gain", zeroline=F))
  
  fig
})

output$product_complexity_Plot <- renderPlotly( {
  fig <- plot_ly(complexity_COI_gain_distance_sb,x = ~distance, y = ~ici, opacity=0.5,
                 color = ~h_light_ici, 
                 text = ~industry_title, 
                 hoverinfo = "text",
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10)) %>% 
    hide_legend() %>% hide_colorbar() %>% 
    layout(title = selected_msa,
           xaxis = list(title = "Distance", zeroline=F), # use zeroline = FALSE to remove zero line :)
           yaxis = list(title="Industry Complexity", zeroline=F))
  
  fig
})

output$myImage <- renderImage({
  #width  <- session$clientData$output_myImage_width
  #height <- session$clientData$output_myImage_height
  # Return a list containing the filename
  list(src = 'www/South_Bend_Flag.png',
       contentType = 'image/png',
       width="100%",
       alt = "Alignment",
       style="text-align:justify;color:black;padding:15px;border-radius:10px")
}, deleteFile = FALSE)

output$myImage2 <- renderImage({
  #width  <- session$clientData$output_myImage_width
  #height <- session$clientData$output_myImage_height
  # Return a list containing the filename
  list(src = 'www/South_Bend_Flag.png',
       contentType = 'image/png',
       width="100%",
       alt = "Alignment",
       style="text-align:justify;color:black;padding:15px;border-radius:10px")
}, deleteFile = FALSE)

# switch this on if need to include image in etire page
#output$picture <- renderImage({
#  return(list(src = "www/South_Bend_Flag.png",contentType = "image/png",alt = "Alignment"))
#}, deleteFile = FALSE) #where the src is wherever you have the picture
  
#value boxes-------------------------------------------------------------------------------------------
  output$totaljobbox = renderValueBox({
    recent_date = max(df$dt)
    my_query = "Total Jobs as of SAMPLE"
    med_trips = df$`Job Openings`[ df$indicator=="Total Openings" & df$dt==recent_date & df$msa==input$select_msa ]
    valueBox(
      paste0(med_trips), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-briefcase"),
      color = "yellow")
  }) 
  output$jobchangebox = renderValueBox({
    recent_date = max(df$dt)
    my_query = "Percent change as of SAMPLE"
    shl_trips = paste0(round(df$per_change[ df$indicator=="Total Openings" & df$dt==recent_date & df$msa==input$select_msa]*100,2),"%")
    valueBox(
      paste0(shl_trips), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-percent"),
      color = "green")
  }) 
  output$totalcandidatebox = renderValueBox({ 
    recent_date = max(df_cand$dt)
    my_query = "Total Candidates as of SAMPLE"
    ubers_etc = df_cand$`Candidates`[ df_cand$indicator=="Total Candidates" & df_cand$dt==recent_date & df_cand$msa==input$select_msa]
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-user-graduate"),
      color = "maroon")
  })
  output$candidatechangebox = renderValueBox({ 
    recent_date = max(df_cand$dt)
    my_query = "Percent change as of SAMPLE"
    ubers_etc = paste0(round(df_cand$per_change[ df_cand$indicator=="Total Candidates" & df_cand$dt==recent_date & df_cand$msa==input$select_msa]*100,2),"%")
    valueBox(
      paste0(ubers_etc), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-percent"),
      color = "purple")
  })
  
  output$candidatestojobbox = renderValueBox({ 
    recent_date = max(df_cand$dt)
    my_query = "Number of candidates per job as of SAMPLE"
    candidates = df_cand$`Candidates`[ df_cand$indicator=="Total Candidates" & df_cand$dt==recent_date & df_cand$msa==input$select_msa]
    jobs= df$`Job Openings`[ df$indicator=="Total Openings" & df$dt==recent_date & df$msa=="South Bend - Mishawaka" ]
    cand_per_job=round(candidates/jobs,2)
    valueBox(
      paste0(cand_per_job), 
      p(sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query),
      br(),
      "A number less than or equal to one is good."),
      icon = icon("fas fa-file-medical-alt"),
      color = "blue")
  })
  
  # ces data
  output$totalemploymentbox = renderValueBox({
    recent_date = max(sb_elk_ces_sup$dt)
    my_query = "Total Employment as of SAMPLE"
    employment = sb_elk_ces_sup$total_employment[ sb_elk_ces_sup$industry_name=="Total Nonfarm" & sb_elk_ces_sup$dt==recent_date & sb_elk_ces_sup$msa_name==input$select_msa_emp ]
    valueBox(
      format(employment,big.mark=",",scientific=FALSE), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-briefcase"),
      color = "yellow")
  }) 
  output$employmentchangebox = renderValueBox({
    recent_date = max(sb_elk_ces_sup$dt)
    emp_change = sb_elk_ces_sup$change[ sb_elk_ces_sup$industry_name=="Total Nonfarm" & sb_elk_ces_sup$dt==recent_date & sb_elk_ces_sup$msa_name==input$select_msa_emp]
    emp_change_per = round(sb_elk_ces_sup$per_change[ sb_elk_ces_sup$industry_name=="Total Nonfarm" & sb_elk_ces_sup$dt==recent_date & sb_elk_ces_sup$msa_name==input$select_msa_emp]*100,2)
    valueBox(
      #emp_change, 
      paste0(format(emp_change,big.mark=",",scientific=FALSE),"(",emp_change_per,"%)"),
      "Change compared to the year before", icon = icon("fas fa-percent"),
      color = "green")
  }) 
  
  # laus data
  output$totallaborforce = renderValueBox({ 
    recent_date = max(laus_select$dt)
    my_query = "Total labor force as of SAMPLE"
    labor_force = laus_select$value[ laus_select$measure=="Labor Force" & laus_select$dt==recent_date & laus_select$msa_name==input$select_msa_emp]
    valueBox(
      format(labor_force,big.mark=",",scientific=FALSE), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-user-graduate"),
      color = "maroon")
  })
  output$unemploymentbox = renderValueBox({ 
    recent_date = max(laus_select$dt)
    my_query = "Unemployed as of SAMPLE"
    unemployed = laus_select$value[ laus_select$measure=="Unemployment" & laus_select$dt==recent_date & laus_select$msa_name==input$select_msa_emp]
    unemployed_per= round(laus_select$value[ laus_select$measure=="Unemployment Rate" & laus_select$dt==recent_date & laus_select$msa_name==input$select_msa_emp],2)
    valueBox(
      paste0(format(unemployed,big.mark=",",scientific=FALSE),"(",unemployed_per,"%)"), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("fas fa-percent"),
      color = "purple")
  })
  
  # claims data
  output$total_ini_claims_box = renderValueBox({ 
    recent_date = as.character(max(all_claims$dt))
    my_query = "Total initial claims as of 'SAMPLE'"
    ubers_etc = all_claims$claims[ all_claims$NaicsTitle=="All Industries"&all_claims$claim_type=="Initial" & all_claims$dt==recent_date ]
    valueBox(
      format(ubers_etc,big.mark=",",scientific=FALSE), sub("SAMPLE",recent_date,my_query),
      color = "yellow")
  })
  
  output$change_ini_claims_box = renderValueBox({ 
    recent_date = as.character(max(all_claims$dt))
    comparison_date= as.character(min(all_claims$dt))
    my_query = "Times initial claims as of 'SAMPLE'"
    ubers_etc = all_claims$times_change[ all_claims$NaicsTitle=="All Industries"&all_claims$claim_type=="Initial" & all_claims$dt==recent_date ]
    valueBox(
      paste0(round(ubers_etc,0)), sub("SAMPLE",comparison_date,my_query),
      color = "yellow")
  })
  
  output$total_cont_claims_box = renderValueBox({ 
    recent_date = as.character(max(all_claims$dt)-weeks(1)) # continued claims are available one week older
    my_query = "Total continued claims as of 'SAMPLE'"
    ubers_etc = all_claims$claims[ all_claims$NaicsTitle=="All Industries"&all_claims$claim_type=="Continued" & all_claims$dt==recent_date ]
    valueBox(
      format(ubers_etc,big.mark=",",scientific=FALSE), sub("SAMPLE",recent_date,my_query),
      color = "purple")
  })
  
  output$change_cont_claims_box = renderValueBox({ 
    recent_date = as.character(max(all_claims$dt)-weeks(1)) # continued claims are available one week older
    comparison_date= as.character(min(all_claims$dt))
    my_query = "Times continued claims as of 'SAMPLE'"
    ubers_etc = all_claims$times_change[ all_claims$NaicsTitle=="All Industries"&all_claims$claim_type=="Continued" & all_claims$dt==recent_date ]
    valueBox(
      paste0(round(ubers_etc,0)), sub("SAMPLE",comparison_date,my_query),
      color = "purple")
  })
  
  # housing boxes
  output$totalbuildingbox = renderValueBox({ 
    recent_date = as.character(max(housing_sb$dt)) 
    my_query = "Total permits issued as of 'SAMPLE'"
    n_units = housing_sb$total[housing_sb$key=="Units" & housing_sb$dt==recent_date ]
    valueBox(
      n_units, sub("SAMPLE",recent_date,my_query),
      color = "purple")
    
  })
  
  output$buildingchangebox = renderValueBox({ 
    recent_date = as.character(max(housing_sb$dt)) 
    my_query = "Percent change since 'SAMPLE'"
    comparison_date = "January 2020"
    per_change = paste0(round(housing_sb$per_change[housing_sb$key=="Units" & housing_sb$dt==recent_date ]*100,2),"%")
    valueBox(
      per_change, sub("SAMPLE",comparison_date,my_query),
      color = "purple")
   
  })
  
  output$homevaluebox = renderValueBox({
    recent_date = max(home_prices_in$dt)
    my_query = "Home value as of SAMPLE"
    home_value = home_prices_in$value[home_prices_in$dt==recent_date & home_prices_in$msa==input$select_msa_homevalue ]
    valueBox(
      paste0(format(home_value,big.mark=",",scientific=FALSE)), sub("SAMPLE",format(recent_date,"%b %d, %Y"),my_query), icon = icon("home"),
      color = "yellow")
  }) 
  
  output$homevaluechangebox = renderValueBox({
    recent_date = max(home_prices_in$dt)
    homevaluechange = paste0(round(home_prices_in$per_change[ home_prices_in$dt==recent_date & home_prices_in$msa==input$select_msa_homevalue]*100,2),"%")
    valueBox(
      paste0(homevaluechange), "Change compared to the year before", icon = icon("fas fa-percent"),
      color = "green")
  })
  
  
  
#evictions boxes
  output$totalevictionbox = renderValueBox({ 
    recent_date = as.character(max(sb_weekly_evictions$week_date)) 
    my_query = "Total eviction filings in week ending 'SAMPLE'"
    n_eviction_filings = sb_weekly_evictions$total_filings[sb_weekly_evictions$week_date==recent_date ] %>% sum()
    valueBox(
      n_eviction_filings, sub("SAMPLE",recent_date,my_query),
      color = "purple")
    
  })
  
  output$evictionchangebox = renderValueBox({ 
    recent_date = as.character(max(sb_weekly_evictions$week_date)) 
    my_query = "Percent change since 'SAMPLE'"
    comparison_date = "average (2016-19)"
    per_change = paste0(round(sb_weekly_evictions$per_change[sb_weekly_evictions$week_date==recent_date ]*100,2),"%")
    valueBox(
      per_change, sub("SAMPLE",comparison_date,my_query),
      color = "purple")
    
  })


}


#Execute APP------------
shinyApp(ui, server)


