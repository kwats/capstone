library(YaleToolkit)
whatis(new_m)
names(new_m)
numNAs <- apply(new_m, 2, function(y) sum(!is.na(y)))
numNAs <- numNAs[order(numNAs, decreasing=T)]
table(new_m$country, new_m$food_access)
hist(numNAs)

numNAs <- apply(new_m, 2, function(y) sum(!is.na(y)))
m2 <- new_m[, numNAs > 10000]
names(m2)

for (i in 1:ncol(m2)) {
  if (is.character(m2[,i])) { 
    if (sum(!is.na(as.numeric(m2[,i])))/(sum(!is.na(m2[,i]))+1) > 0.9) {
      m2[,i] <- as.numeric(m2[,i])
      cat("column", i, "changed to numeric.\n")
    }
  }
}

boxplot(as.numeric(m2$n_total) ~ m2$country)
boxplot(as.numeric(m2$n_youth_female) ~ m2$country)
prop <- m2$n_youth_female/m2$n_total
boxplot(prop ~ m2$country)

table(new_m$country, new_m$travel_abr)
table(new_m$country, new_m$job_abr)

siteYesJob <- table(new_m$site_id, new_m$job_abr)
siteYesJob <- siteYesJob[,2]
siteYesJob <- siteYesJob[siteYesJob > 0]
hist(siteYesJob)
siteYesJob <- (table(new_m$site_id, new_m$job_abr)[,2])/table(m2$site_id)

new_m$safe_child[is.na(new_m$safe_child)] <- ifelse(new_m$unsafe_child[is.na(new_m$safe_child)] == "Yes", "No", "Yes")
new_m$safe_female[is.na(new_m$safe_female)] <- ifelse(new_m$unsafe_female[is.na(new_m$safe_female)] == "Yes", "No", "Yes")
table(m2$country, m2$sec_provided)
prop.table(table(sec=new_m$sec_provided, safe=new_m$safe_female), margin = 1)
table(m2$country, m2$site_owner)
new_m$site_owner_gov <- FALSE

new_m$site_owner_gov[grep("ublic", new_m$site_owner)] <- TRUE
prop.table(table(owner=new_m$site_owner_gov, safe=new_m$safe_female), margin=1)

new_m$site_owner_gov[is.na(new_m$site_owner)] <- NA
prop.table(table(owner=new_m$site_owner_gov, safe=new_m$safe_child), margin=1)

new_m$site_owner_private <- FALSE
new_m$site_owner_private[grep("riva", new_m$site_owner)] <- TRUE
new_m$site_owner_private[is.na(new_m$site_owner)] <- NA
prop.table(table(owner=new_m$site_owner_private, safe=new_m$safe_child), margin=1)
prop.table(table(owner=new_m$site_owner_private, safe=new_m$safe_female), margin=1)

new_m$latrine_gender[grep("Yes", new_m$latrine_gender)] <- "Yes"
prop.table(table(latrineGender = new_m$latrine_gender, safe = new_m$safe_female), margin=1)

table(latrine=new_m$latrine_gender, safe=new_m$safe_female, new_m$country)

# Things we should do:
# - Make a map of sites that have job opportunities abroad and ones that do not
library(maps)
pdf("map.pdf")
map()
colors = ifelse(m2$job_abr == "Yes", "orange", "lightBlue")
points(new_m$site_longitude, new_m$site_latitude, col=colors, cex=0.01)
dev.off()

# - Combine variables indicating 'need' or 'support' and examine relationship between
#   these and variables suggesting movement (e.g. jobs abroad, travel, etc); likely
#   to be country-specific
table(new_m$country, new_m$need_food)
table(new_m$country, new_m$support_food)[, c(4,6)] 
needVariables = c("food", "edu", "health", "live", "protect")
for(i in 1:length(needVariables)) {
  new_m[is.na(new_m[, paste0("support_", needVariables[i])]),
        paste0("support_", needVariables[i])] <- new_m[is.na(new_m[, paste0("support_", needVariables[i])]),
                                                       paste0("need_", needVariables[i])]
}

needMatrix = matrix(0, ncol=5, nrow=8)
for(i in 1:length(needVariables)) {
  temp <- table(new_m$country, new_m[, paste0("support_", needVariables[i])])[, c("No", "Yes")]
  needMatrix[,i] <- temp[,2]/rowSums(temp)
}
rownames(needMatrix) <- sort(unique(new_m$country))
colnames(needMatrix) <- needVariables
table(new_m$country, new_m$support_health)
barplot(needMatrix[-c(1,2,4,6),])  # Make a legend later

# - Combine the support variables that are the same: protect and protection, edu
#   and education

# - Examine ratio of males to females at each site and the relationship with safety
new_m$percentFemale <- m2$n_adult_female / m2$n_adult
boxplot(new_m$percentFemale ~ new_m$country)
