
library(tidyverse)
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


# Change value od day if 0 to 1
terrorists_attack$iday[terrorists_attack$iday == 0] <- 1

# Add date column
terrorists_attack <- terrorists_attack %>% mutate(date = paste(iday, imonth,
                                                   iyear, sep = "/"))
terrorists_attack$date <- dmy(terrorists_attack$date)


# Plotting
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
  theme(axis.text.x = element_text(angle = 45,
                       hjust = 1,
                       vjust = 1))


# Attacks per decade
ggplot(terrorists_attack, aes(x = decade, fill = decade)) +
  geom_bar() +
  labs(title = "", x = "Decade", y = "Count", fill = "") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.2, size = 3) +
  theme_bw()


