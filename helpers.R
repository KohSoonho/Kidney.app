
# BSA ---------------------------------------------------------------------
Du_bois <- function(bw, height){
  bsa <- (bw ^ 0.425) * (height ^ 0.725) * 0.007184
  return(bsa)
}

Shintani <- function(bw, height){
  bsa <- (bw ^ 0.425) * (height ^ 0.725) * 0.007358
  return(bsa)
}

Fujimoto <- function(bw, height){
  bsa <- (bw ^ 0.444) * (height ^ 0.663) * 0.008883
  return(bsa)
}

# cockcroft ---------------------------------------------------------------

cockcroft <- function(bw, age, cre, sex){
  gfr <- bw * (140 - age) / 72 / cre
  return(ifelse(sex == "male", gfr, gfr * 0.85))
}

# CCr-Sanford -------------------------------------------------------------

sanford <- function(bw, height, age, cre, sex){
  BMI <- bw / (height / 100) ^ 2
  inch <- height / 2.54
  IBW_male <- ifelse(inch <= 60, 50, 50 + 2.3 * (inch - 60))
  IBW_female <- ifelse(inch <= 60, 45, 45 + 2.3 * (inch - 60))
  crcl <- if((BMI > 30 | bw >= IBW_male * 1.2) & sex == "male"){
    (137 - age) * ((0.285 * bw) + (12.1 * ((height / 100) ^ 2 ))) / (51 * cre)
  }else if((BMI > 30 | bw >= IBW_male * 1.2) & sex == "female"){
    (146 - age) * ((0.287 * bw) + (9.74 * ((height / 100) ^ 2 ))) / (60 * cre)     
  }else if(sex == "male"){
    ((140 - age) * (IBW_male)) / (72 * cre)
  }else{
    (((140 - age) * (IBW_female)) / (72 * cre)) * 0.85
  }
  return(crcl)
}

# MDRD --------------------------------------------------------------------

mdrd_jaffe <- function(age, cre, sex, race){
  gfr <- 186 * (age ^ -0.203) * (cre ^ -1.154) 
  gfr_sex <- ifelse(sex == "male", gfr, gfr * 0.742)
  gfr_race <- if(race == "Japan"){
    NaN
  }else if(race == "Blacks"){
    gfr_sex * 1.212
  }else{
    gfr_sex
  }
  return(gfr_race)
}

mdrd_jaffe_adj <- function(bw, height, age, cre, sex, race, bsa){
  switch(bsa, 
         "Du bois" = mdrd_jaffe(age, cre, sex, race) * Du_bois(bw, height) / 1.73, 
         "Shintani" = mdrd_jaffe(age, cre, sex, race) * Shintani(bw, height) / 1.73, 
         "Fujimoto" = mdrd_jaffe(age, cre, sex, race) * Fujimoto(bw, height) / 1.73)
}

mdrd_enzyme <- function(age, cre, sex, race){
  gfr <- 175 * (age ^ -0.203) * (cre ^ -1.154) 
  gfr_sex <- ifelse(sex == "male", gfr, gfr * 0.742)
  gfr_race <- if(race == "Japan"){
    gfr_sex * 0.741
  }else if(race == "Blacks"){
    gfr_sex * 1.212
  }else{
    gfr_sex
  }
  return(gfr_race)
}

mdrd_enzyme_adj <- function(bw, height, age, cre, sex, race, bsa){
  switch(bsa, 
         "Du bois" = mdrd_enzyme(age, cre, sex, race) * Du_bois(bw, height) / 1.73, 
         "Shintani" = mdrd_enzyme(age, cre, sex, race) * Shintani(bw, height) / 1.73, 
         "Fujimoto" = mdrd_enzyme(age, cre, sex, race) * Fujimoto(bw, height) / 1.73)
}

# Japan CKD guideline -----------------------------------------------------

j_gfr <- function(age, cre, sex, race){
  gfr <- 194 * (age ^ -0.287) * (cre ^ -1.094)
  gfr_sex <- ifelse(sex == "male", gfr, gfr * 0.739)
  return(ifelse(race == "Japan", gfr_sex, NaN))
}

j_gfr_adj <- function(bw, height, age, cre, sex, race, bsa){
  switch(bsa, 
         "Du bois" = j_gfr(age, cre, sex, race) * Du_bois(bw, height) / 1.73, 
         "Shintani" = j_gfr(age, cre, sex, race) * Shintani(bw, height) / 1.73, 
         "Fujimoto" = j_gfr(age, cre, sex, race) * Fujimoto(bw, height) / 1.73)
}


# Ureine Cre --------------------------------------------------------------

U_CRE <- function(bw2, height2, SC, UC, UV, bsa){
  CCr <- 1.73 * UC * UV / SC / 1440 
  return(switch(bsa, 
                "Du bois2" =  CCr / Du_bois(bw2, height2), 
                "Shintani2" = CCr / Shintani(bw2, height2), 
                "Fujimoto2" = CCr / Fujimoto(bw2, height2))
  )
}