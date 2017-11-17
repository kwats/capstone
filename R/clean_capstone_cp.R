knitr::opts_chunk$set(eval = FALSE, tidy = FALSE)
setwd("/Users/Katie/Desktop/capstone")
m <- read.csv("/Users/Katie/Desktop/capstone/csv/original/newest_merge.csv", as.is = TRUE)

new_m <- m[ , -which(names(m) %in% c("x.1500", "health.issue", "helath_issue_c", "info_src_authority"))]   
new_m <- new_m[, -grep("^column", colnames(new_m))]
new_m <- new_m[-which(new_m$round == 0), ] # BANGLADESH
new_m <- subset(new_m, !is.na(new_m$country))
names <- names(new_m)

new_m[] <- lapply(new_m, as.character)
# Updated this
new_m <- as.data.frame(lapply(new_m, function(x){
  x <- replace(x, x %in% c("NA", "N/A", "No Answer", ".", "null","noresponde", 
                           "nosabe", "no_sabe", "Unknown", "", 
                           "Sinrespuesta", "na","-", "8__unknown", "dont_know", "4__no_answer", "5__unknown","Do_not_know", "No Response", "no responde","No sabe", "Don't know","no sabe", "no_responde", "Not specified.","", "no answer", "No answer", "sin respuesta", "Sin respuesta", "Sin Informacion", "Unknown", "UNKNOWN", "unknown", "Unkown", "unkown", "Not Specified", "Desconocido","desconocido", "6__unknown", "7__no_answer", "NULL", "Sin respuesta, Por que?", "NO ANSWER"), NA)
  }))
# Updated
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(^Don't[ ]*know$)|(^[1-9_]*no_*answer$)|(^[1-9_]*Unknown$)|(^Sin[ ]*respuesta$)|(^No[ ]*answer[, ]*why[?]*$)", replacement = NA, ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[1_]*(yes|si|true)$", replacement = "Yes", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^false$|^[1-9_nf]+o\\?*$", replacement = "No", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[35_n]+[oi]n[|gunoe]*$", replacement = "None", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^(other|otro)$", replacement = "Other", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(<|less|menos).*2.*km.*", replacement = "Less_2km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(<|less|menos).*5.*km.*", replacement = "Less_5km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(<|less|menos).*10.*k+m.*", replacement = "Less_10km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(<|less|menos).*1[^0]*km.*", replacement = "Less_1km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(>|more|mas).*2.*km.*", replacement = "More_2km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(>|more|mas).*5.*km.*", replacement = "More_5km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^.*(>|more|mas).*10.*k+m.*", replacement = "More_10km", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[_1o]+n.*3.*$", replacement = "On-site (< 3km)", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[_2o]+n.*3.*$", replacement = "On-site (> 3km)", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[_3o]+f.*3.*$", replacement = "Off-site (< 3km)", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[_4o]+f.*3.*$", replacement = "Off-site (> 3km)", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(^Ons$|^[EO]N.*SIT.*)", 
                                replacement = "On_site", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "[_1-9yes]+on_*site|(^[YS]*[EI].*[OE]N.*SITE*[IO]*)$", 
                                replacement = "On_site", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "[1-9]__offsite|[1-9]__yes_o[f]+site|(^[o|]+ff+.*site+.*)|(^fuera.*sitio.*)", 
                                replacement = "Off_site", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[Yy]*[ES][SI]+.*[F]+.*site*[io]*$", 
                                replacement = "Off_site", ignore.case=TRUE)
new_m$edu_access_f <-  sub(new_m$edu_access_f, pattern = "Yes", replacement = "On_site", ignore.case=TRUE)
new_m$edu_access_f <-  sub(new_m$edu_access_f, pattern = "None", replacement = "No", ignore.case=TRUE)
new_m$job_farm <-  sub(new_m$job_farm, pattern="^[^NY].*", replacement = NA)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[2_F]+.*(emale|emenino)$", replacement = "Female", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[1_M]+.*(ale|asculino)$", replacement = "Male", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^males$", replacement = "Men")
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^females$", replacement = "Women")

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Diaria|(^[1_E]+veryday$)", replacement = "Everyday", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(^([_3O]+nce|Una).*(semana|WE+K)$)", replacement = "Once a week", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^([5_]*(Una|Once).*M[OE][SN].*$)", replacement = "Once a month", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "4__every_2_weeks|(^E.*2.*WE+K$)|(^(Cada|Dos).*semana.*$)", replacement = "Every 2 weeks", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[_6Ii]+rregular$", replacement = "Irregular", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(^[_7N]+.*(unca|ever))$", replacement = "Never", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(CH*OLERA)", replacement = "Cholera", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(DIAR+H*EA)", replacement = "Diarrhea", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(Malnutri[tc]ion)", replacement = "Malnutrition", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Conj[uo][n]*ctivit[e|i][s]*", replacement = "Conjuctivitis", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^(Common\\s)*cold[s]*$", replacement = "Colds", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(infecciones de la piel)|(^Skin\\s(infec[ct]ions|disease)$)", replacement = "Skin infections", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^Measles$", replacement = "Measles", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^Dysent[e]*ry$", replacement = "Dysentery", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(^[6_M]+.*clinic$)|(^clinicas moviles$)", replacement = "Mobile clinic", ignore.case=TRUE)

new_m$health_medicine <-  sub(new_m$health_medicine, pattern = "^[^NY].*", replacement = NA, ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Office[/.]*$", replacement = "office", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "He[al]+th", replacement = "health", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "[THER]+e*d+\\sc[ro]+[se]+[/.]*", replacement = "Red Cross", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Burea[u]*", replacement = "bureau", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Center", replacement = "center", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(^Woreda\\s+health\\s(office|center|Post)$)|(^By Woreda$)", replacement = "Woreda Health Office", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "(Gobierno|^goven*ment[.]*$)|(^gover[ne]*ment[.]*.*(RHB|DPPO)*$)", replacement = "Government", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[2_iINO]+[GN][OGN]S*$", replacement = "NGO", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^ONG Int.$", replacement = "International NGO", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Centro de salud local|^[7_L]+OCAL.*clinic", replacement = "Local clinic", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "clinic", replacement = "clinic", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "organi[zs]ation", replacement = "organization", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "Local clinic/med.*practitioners.*$", replacement = "Local clinic, medical practitioners", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[4_O]+ther$|Otro, especificar", replacement = "Other", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^church$", replacement = "Church", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "UNICEF", replacement = "UNICEF", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "save.*children.*", replacement = "Save the Children", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "(\\s*&\\s)|(\\s*,\\s*)", replacement = ",", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "(\\s*/\\s)", replacement = "/", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^No.*sist.*residuos$|^No System$|^[1N_]+o.*Waste.*Disposal.*$", replacement = "No waste disposal system", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Quema de residuos|^[3_B]+urning$", replacement = "Burning", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern =  "Fosa de residuos|^[2G_]+arbage.*pit$", replacement = "Garbage pit", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^medium", replacement = "Medium", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^high", replacement = "High", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^very.*high", replacement = "Very high", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^low$", replacement = "Low", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^V.*low$", replacement = "Very low", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^[1-91G_]+ood$", replacement = "Good", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^(Buenas(\\s\\(Higienica\\)))$|([1-9G_]+ood(\\s[\\(]*Hygienic[\\)]*)*)|(Buena)$", replacement = "Good", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Regular\\s\\(No\\smuy\\shigienicas\\)$|not_so_good|^Not\\sso\\sgood(\\s\\(Not\\shygienic\\)*)|(^[1-9_]*Not so good$)$", replacement = "Not so good", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "[1-9_N]+o[n_]+\\s*usable$", replacement = "Non_usable", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^(Malas\\s\\(No\\shay\\shigiene\\))|(^[1-9_]*poor)|(Mala)$", replacement = "Bad", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "(^[1-9_]*excel+en[te]+$)", replacement = "Excellent", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Latrines|toilets$", replacement = "latrines", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "No Sabe|No_answer|^unknown$", replacement = NA)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "^No hay$", replacement = "No")
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "HWS", replacement = "Hand Washing Station", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Autoridades", replacement = "Authorities", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Families,friends", replacement = "Families/friends", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Social Media", replacement = "Social media", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "mobile phone", replacement = "Mobile phone", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "boca a boca", replacement = "Word of mouth", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Family member/relative$", replacement = "Family member", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "En persona", replacement = "In person", ignore.case=TRUE)

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Agricultur[ea]|^Farming$", replacement = "Agriculture", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Agro-pastoralism|^Agriculture/Livestock$", replacement = "Agro-Pastoralism", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "trabajador.*diario|^daily.*laborer$", replacement = "Daily Laborer", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Craftsm[ea]n", replacement = "Craftsman", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Comercio minorista|^Trade$", replacement = "Petty Trade", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], gsub, pattern = "Pesca", replacement = "Fishing", ignore.case=TRUE)


# TO DO Rework date function
library(stringi)
new_m <- new_m[order(new_m$country),] 
new_m <- new_m[order(new_m$round),] 
round <- ""
prev_date <- c(0,0)
new_m <- new_m[order(new_m$country),] 
new_m <- new_m[order(new_m$round),] 
round <- ""
prev_date <- c(0,0)
for(j in grep(pattern="(^.*date$)|(^date.*$)", x=colnames(new_m), value=TRUE)) {
  for (col in which(colnames(new_m)==j)) {
    for (i in 1:(nrow(new_m)-1)) {
      if (!is.na(new_m[i,col])) {
        date <- as.character(new_m[i,col])
        date <-unlist(strsplit(date,"\\/|-"))
        date[2][1] <- sub("Jan","01", date[2][1])               # fix written months
        date[2][1] <- sub("Feb","02", date[2][1])
        date[2][1] <- sub("Mar","03", date[2][1])
        date[2][1] <- sub("Apr","04", date[2][1])
        date[2][1] <- sub("May","05", date[2][1])
        date[2][1] <- sub("Jun","06", date[2][1])
        date[2][1] <- sub("Jul","07", date[2][1])
        date[2][1] <- sub("Aug","08", date[2][1])
        date[2][1] <- sub("Sep","09", date[2][1])
        date[2][1] <- sub("Oct","10", date[2][1])
        date[2][1] <- sub("Nov","11", date[2][1])
        date[2][1] <- sub("Dec","12", date[2][1])
        if (nchar(date[1][1])>3) {
          temp <- date[1]
          date[1] <- date[3]
          date[3] <- date[1]
        }
        if (nchar(date[2][1])<2) stri_sub(date[2][1], 1, 0) <- 0 # padding
        if (nchar(date[1][1])<2) stri_sub(date[1][1], 1, 0) <- 0 # padding
        if (nchar(date[3][1])>2) date[3][1] <- stri_sub(date[3][1], 3, 4)
        if (new_m$round[i] == round) {
          dm <- as.integer(date[1][1]) - as.integer(prev_date[1])
          md <- as.integer(date[2][1]) - as.integer(prev_date[2])
          if (abs(dm) < abs(md)) {
            temp <- date[1]
            date[1] <- date[2]
            date[2] <- temp
          }
        }
        if (as.integer(date[2][1]) > 12) {
          temp <- date[1]
          date[1] <- date[2]
          date[2] <- temp
        }
        round <- new_m$round[i]
        prev_date[1] <- date[1][1]
        prev_date[2] <- date[2][1]
        date_new<-paste(date, collapse = '/')
        print(date_new)
        new_m[i,col] <- date_new
      }
    }
  }
}

# NEW

for (i in 1:ncol(new_m)) {
  if (is.character(new_m[,i])) { 
    if (sum(!is.na(as.numeric(new_m[,i])))/(sum(!is.na(new_m[,i]))+1) > 0.9) {
      new_m[,i] <- as.numeric(new_m[,i])
      cat("column", i, "changed to numeric.\n")
    }
  }
}

new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^Cerrado$", replacement = "Closed", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^iom$", replacement = "IOM", ignore.case=TRUE)
new_m$edu_access <-  sub(new_m$edu_access, pattern = "^Closed$", replacement = "No", ignore.case=TRUE)
new_m$health_center_dist <- sub(new_m$health_center_dist, pattern = "^Mobile clinic/health extension worker visit$", replacement = "Mobile clinic", ignore.case=TRUE)
new_m$hyg_f_product <-  sub(new_m$hyg_f_product, pattern = "^All$", replacement = "Yes", ignore.case=TRUE)

none_to_no <- c("edu_access_i", "health_center","hyg_f_product", "hyg_evidence", "hyg_kit_soap")
for (i in 1:length(none_to_no)) {
  index <- grep(none_to_no[[i]], colnames(new_m))[1]
  new_m[[index]] <- sub(new_m[[index]], pattern = "^None$", replacement = "No", ignore.case=TRUE)
}

# temp fix #
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "\\sHand Washing Station$", replacement = "Hand Washing Station", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "sameHand Washing Station$", replacement = "same Hand Washing Station", ignore.case=TRUE)
####

## PERCENTS
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^(25%orless)|(1%-25%)|(<[ ]*25[ ]*[% ]*)|([1-9_]*less[_ ]*(than)*[_ ]*25[_ ]*(%|percent)+)|(25% or less)|(meno[sr]*[_dea ]*25%)$", replacement = "<25%", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^(2[56]+%[to -]+50%)|(<[ ]*50[ ]*[% ]*)|([1-9_]*less[_ ]*(than)[_ ]*50[_ ]*(%|percent)+)|(50% or less)|(meno[sr]*[_dea ]*50%)|([entre ]*26%* [toy]+ 50%)$", replacement = "<50%", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^(51%[to-]+75%)|(<[ ]*75[% ]*)|([1-9_]*less[_ ]*(than)*[_ ]*75[_ ]*(%|percent)+)|(75% or less)|(meno[sr]*[_dea ]*75%)|([entre ]*51%* [toy]+ 75%)$", replacement = "<75%", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^(>[ ]*75[% ]*)|([1-9_]*more[_ ]*(than)*[_ ]*75[_ ]*(%|percent)+)|(more than 75%)|(ma[ysorade_ ]*75%)|(76%-.*)$", replacement = ">75%", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = ".00%$", replacement = "%", ignore.case=TRUE)
# Temp fix, percents #
new_m$per_hh_income <-  sub(new_m$per_hh_income, pattern = "^[0-9 ]+$", replacement = NA, ignore.case=TRUE)
new_m$per_hh_income <-  sub(new_m$per_hh_income, pattern = "^[A-Z ]+$", replacement = NA, ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^([1-9_]*none)|(Ninguna)$", replacement = "None", ignore.case=TRUE)
#######

# Time #
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^Less than ", replacement = "<", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^More than", replacement = ">", ignore.case=TRUE)
new_m$site_dist_origin <-  sub(new_m$site_dist_origin, pattern = "^< ", replacement = "<", ignore.case=TRUE)
new_m$site_dist_origin <-  sub(new_m$site_dist_origin, pattern = "hour", replacement = "hour", ignore.case=TRUE)
new_m$site_dist_origin <-  sub(new_m$site_dist_origin, pattern = "^2[-3 ]+hours", replacement = "2-3 hours", ignore.case=TRUE)
new_m$site_dist_origin <-  sub(new_m$site_dist_origin, pattern = "^10 minutes or less", replacement = "<10 mins", ignore.case=TRUE)
new_m$site_dist_origin <-  sub(new_m$site_dist_origin, pattern = "Half hour to", replacement = "30 mins -", ignore.case=TRUE)

# Urban / Rural
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[area]*urba[nao]+$", replacement = "Urban", ignore.case=TRUE)
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^[area]*rural$", replacement = "Rural ", ignore.case=TRUE)


# Winter
new_m[,1:ncol(new_m)] <- lapply(new_m[,1:ncol(new_m)], sub, pattern = "^winterclothes$", replacement = "Winterclothes", ignore.case=TRUE)

for (i in 88:90) {
  print(names(new_m)[i])
  print(table(new_m[,i]))
}

# QUICK FIX
n <- 1
for (n in 1:nrow(new_m)) {
  if (new_m$country[n] %in% c("Nigeria")) {
    new_m$per_hh_nets[n] <- NA
    new_m$sma_exist[n] <- NA
  }
  if (new_m$country[n] %in% c("Bangladesh")) {
    new_m$sec_provided[n] <- NA
  }
  if (new_m$country[n] %in% c("Haiti")) {
    new_m$support_not_met[n] <- NA
    new_m$support_protect[n] <- NA
    new_m$water_complaint[n] <- NA
    new_m$water_container_date[n] <- NA
  }
  new_m$sec_provided <- as.character(new_m$sec_provided)
  new_m$support_not_met <- as.character(new_m$support_not_met)
  new_m$water_container_date <- as.character(new_m$water_container_date)
}
new_m$sec_reported <-  sub(new_m$sec_reported, pattern = "^Yes.*$", replacement = "Yes", ignore.case=TRUE)

print(new_m[which(new_m$water_complaint %in% c("Off_site")), 14]) # MALAWI

##### END

write.csv(new_m, "csv/clean/newest_clean.csv", row.names=FALSE, fileEncoding="UTF-8")
m <- read.csv("/Users/Katie/Desktop/capstone/csv/clean/newest_clean.csv", as.is = TRUE)
new_m <- m

list_countries <- split(new_m, as.factor(new_m$country))
for (i in list_countries) {
  tmp = paste("./csv/clean/countries/", i$country[1], "_dta.csv", sep = "")
  write.csv(i, tmp, row.names=FALSE, fileEncoding="UTF-8")
}
