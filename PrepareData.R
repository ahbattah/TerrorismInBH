
library(tidyverse)
library(RColorBrewer)

# globalterrorism <-
#   read_csv("C:\\Users\\stsscab\\Desktop\\R files\\globalterrorismdb_0617dist.csv")
# 
# terrorism_attack <- globalterrorism %>%
#   filter(grepl('bahrain', country_txt, ignore.case = TRUE))
# 
# write_csv(terrorism_attack, paste(getwd(), "/terrorism_attack", sep = ""))

# Read data
terrorists_attack <- read_csv('terrorism_attack')

# sapply(terrorists_attack, function(x) {
#   length(which(is.na(x)))
# })

# Temove unuseful columns
terrorists_attack <-
  terrorists_attack %>% select(
    -c(
      approxdate,
      extended,
      country,
      country_txt,
      region,
      region_txt,
      attacktype3,
      attacktype3_txt,
      guncertain3,
      claimmode2,
      claimmode2_txt,
      claimmode3,
      claimmode3_txt,
      compclaim,
      weaptype4,
      weapsubtype4_txt,
      nhostkid,
      nhostkidus,
      nhours,
      ndays,
      divert,
      kidhijcountry,
      ransomamt,
      ransomamtus,
      ransompaid,
      ransompaidus,
      ransomnote,
      hostkidoutcome,
      hostkidoutcome_txt,
      nreleased
    )
  )

# Mutate decade column
terrorists_attack <- terrorists_attack %>%
  mutate(decade = ifelse(iyear < 1980, "1970s",
                         ifelse(
                           iyear < 1990, "1980s",
                           ifelse(iyear < 2000, "1990s",
                                  ifelse(iyear < 2010, "2000s", "2010s"))
                         )))
terrorists_attack$decade <- factor(terrorists_attack$decade,
                                   levels = c("1970s", "1980s", "1990s",
                                              "2000s", "2010s"))


# Change value of day if 0 to 1
terrorists_attack$iday[terrorists_attack$iday == 0] <- 1

# Fix city names
terrorists_attack$city <- terrorists_attack$city %>% 
  recode("Wadiyan" = "Wadyan",
         "Sitrah" = "Sitra",
         "Sahla" = "Sehla",
         "Quraiya" = "Qurayyah",
         "Karzakkan" = "Karzakan",
         "Duraz" = "Diraz",
         "Daih" = "Diah",
         "Demistan" = "Damistan",
         "Al-Akar" = "Eker",
         "Akr" = "Eker")

# Factors
# varsToFactor <- c("provstate", "city")
# terrorists_attack[, varsToFactor] <- lapply(terrorists_attack[, varsToFactor], factor)


# Plotting

# Attacks per decade
ggplot(terrorists_attack, aes(x = decade, fill = decade)) +
  geom_bar() +
  labs(title = "", x = "Decade", y = "Count", fill = "") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.2, size = 3) +
  theme_bw()

# Number of Killed/Wounded based on Year
terrorists_attack %>% 
  filter(nkill > 0 | nwound > 0) %>% 
  group_by(iyear) %>% 
  summarise(Killed = sum(nkill), Wounded = sum(nwound)) %>% 
  ungroup() %>% 
ggplot(aes(x = iyear)) +
  geom_line(aes(y = Killed, colour = "Killed")) + 
  geom_point(aes(y = Killed, colour = "Killed")) + 
  geom_line(aes(y = Wounded, colour = "Wounded")) + 
  geom_point(aes(y = Wounded, colour = "Wounded")) + 
  scale_x_continuous(breaks = terrorists_attack$iyear) +
  labs(title = "", x = "Year", y = "Count", colour = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Attacks per governate
ggplot(terrorists_attack, aes(x = provstate, fill = provstate)) +
  geom_bar() +
  labs(title = "", x = "Governate", y = "Count", fill = "") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.2, size = 3) +
  theme_bw() +
  theme(legend.position = "none")

# Attacks per city based on Capital, Central and Northen
terrorists_attack %>% 
  filter(provstate %in% c("Capital", "Central", "Northern")) %>% 
ggplot(., aes(x = city, fill = provstate)) +
  geom_bar() +
  labs(title = "", x = "City", y = "Count", fill = "Governate") +
  coord_flip() +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = .3, hjust = -.5, size = 3) +
  theme_bw()

# Target type by attack type
colourCount = length(unique(terrorists_attack$targtype1))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(terrorists_attack, aes(x = targtype1_txt, fill = targtype1_txt)) +
  geom_bar() +
  labs(title = "", x = "", y = "", fill = "") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.2, size = 3) +
  scale_fill_manual(values = getPalette(colourCount)) +
  facet_wrap(~ attacktype1_txt) +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


ggplot(terrorists_attack, aes(x = targtype1_txt, fill = attacktype1_txt)) +
  geom_bar(position = "dodge") +
  labs(title = "", x = "Target type", y = "", fill = "Attack Type") +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  theme_bw() +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Number of attacks by group
terrorists_attack %>% 
  group_by(gname) %>% 
  summarise(total = n()) %>% 
ggplot(., aes(x = reorder(gname, total), y = total, fill = gname)) +
  geom_bar(stat = "identity") +
  labs(title = "", x = "", y = "", fill = "") +
  scale_fill_manual(values = getPalette(length(unique(terrorists_attack$gname)))) +
  geom_text(aes(label = total), 
            hjust = -0.3, vjust = 0.2, size = 3) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

# Number of killed/Wounded by group attack name
attacks_locations <- terrorists_attack %>% 
  filter(!is.na(terrorists_attack$longitude), eventid != 199112310005)
leaflet(attacks_locations) %>% 
  addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>% 
  addMarkers(lat = attacks_locations$latitude, 
             lng = attacks_locations$longitude,
             clusterOptions = markerClusterOptions(),
             popup = paste("<strong>Date: </strong>", attacks_locations$iday,"/",attacks_locations$imonth,"/", attacks_locations$iyear,
                           "<br><br><strong>Main weapon: </strong>", ifelse(!is.na(attacks_locations$weaptype1_txt), attacks_locations$weaptype1_txt, "NA"),
                           "<br><strong>Killed: </strong>", attacks_locations$nkill,
                           "<br><strong>Wounded: </strong>", attacks_locations$nwound,
                           "<br><strong>Summay: </strong>", ifelse(!is.na(attacks_locations$summary), attacks_locations$summary, "NA")
                           
             ))
