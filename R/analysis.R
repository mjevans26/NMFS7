library(dplyr)
library(plotly)
library(psych)
library(viridis)
library(cooccur)

plot_ly(data = group_by(good_data, NMFS.Tracking.Number)%>%
          summarize(Type = first(Type))%>%
          group_by(Type)%>%summarize(count = n()), x = ~Type, y = ~count, type= "bar",
        text = ~count, textposition = 'outside')%>%
  layout(yaxis = list(title = "# Consultations"))

plot_ly(data = group_by(good_data, Common.Name, Population)%>%
          summarize(count = n())%>%
          group_by(Common.Name)%>%
          summarize(count = sum(count))%>%
          arrange(desc(count)), x = ~Common.Name, y = ~count, type= "bar",
        text = ~Common.Name, textposition = 'outside')

#Frequency of Consultations by Agency
ag_dt <- filter(good_data, grepl(".", Agency), grepl("20[0-9][0-9]", Fiscal.Year))%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(Agency = first(Agency))%>%
  group_by(Agency)%>%
  summarize(count = n())%>%
  top_n(10, count)

vec <- ag_dt$count
names(vec) <- ag_dt$Agency

plot_ly(data = ag_dt, y = ~Agency, x = ~count, type= "bar", orientation = 'h',
        text = ~Agency, textposition = c('inside', rep('outside', 9)),
        textfont = list(color = c('white', rep('black', 9)), size = 14))%>%
  layout(title = "Action Agency",
         titlefont = list(color = 'black', size = 16),
         yaxis = list(categoryorder = 'array',
                      categoryarray = names(sort(vec)),
                      title = "",
                      showticklabels = FALSE),
         xaxis = list(title = "", range = c(0, 15000),
                      showgrid = FALSE,
                      tickfont = list(color = 'black', size = 12))
  )

#Frequency of Consultations by Species
sp_dt <- filter(good_data, grepl(".", Common.Name), grepl("20[0-9][0-9]", Fiscal.Year))%>%
  group_by(Common.Name)%>%
  summarize(count = length(unique(NMFS.Tracking.Number)))%>%
  top_n(10, count)

vec <- sp_dt$count
names(vec) <- sp_dt$Common.Name

plot_ly(data = sp_dt, y = ~Common.Name, x = ~count, type= "bar", orientation = 'h',
        text = ~Common.Name,
        #textposition = 'outside',
        #textfont = list(color = 'black', size = 14)
        textposition = c('inside', rep('outside', 3), 'inside', rep('outside', 5)),
        textfont = list(color = c('white', rep('black', 3), 'white', rep('black', 5)), size = 14)
        )%>%
  layout(title = "Species",
         titlefont = list(color = 'black', size = 16),
         yaxis = list(categoryorder = 'array',
                      categoryarray = names(sort(vec)),
                      title = "",
                      showticklabels = FALSE),
         xaxis = list(title = "", range = c(0, 15000),
                      showgrid = FALSE,
                      tickfont = list(color = 'black', size = 12))
  )

#Frequency of Consultations by Work Category
cat_dt <- filter(good_data, grepl(".", Category), grepl("Formal", Type), grepl("20[0-9][0-9]", Fiscal.Year))%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(Category = first(Category))%>%
  group_by(Category)%>%
  summarize(count = n())%>%
  top_n(10, count)

vec <- cat_dt$count
names(vec) <- cat_dt$Category

plot_ly(data = cat_dt, y = ~Category, x = ~count, type= "bar", orientation = 'h',
        text = ~Category,
        textposition = c(rep('outside', 9), 'inside'),
        textfont = list(color = c(rep('black', 9), 'white'), size = 14))%>%
  layout(barmode = 'stack',
         title = "Work Type",
         titlefont = list(color = 'black', size = 16),
         yaxis = list(categoryorder = 'array',
                      categoryarray = names(sort(vec)),
                      title = "",
                      showticklabels = FALSE),
         xaxis = list(title = "",
                      tickfont = list(color = 'black', size = 12),
                      showgrid = FALSE,
                      range = c(0, 3000))
         )


#Frequency of conclusions per determination
plot_ly(data = filter(good_data, Type == "Formal Consultation"| Type == "Informal Consultation", Sp != "Technical Assistance Provided", grepl("20[0-9][0-9]", Fiscal.Year))%>%
          group_by(Type, CH)%>%
          summarize(count = n()), type = "bar", x = ~CH, y = ~count, color = ~Type,
        colors = viridis(5),
        text = ~count, textposition = 'outside')%>%
  layout(#barmode = 'stack',
    margin = list(b = 100),
         xaxis = list(title = ""),
         yaxis = list(title = "Number of Determinations"),
         legend = list(x = 0.1, y = 1))

#Frequency of conclusions per consultation
plot_ly(data = filter(good_data, Type == "Formal Consultation"| Type == "Informal Consultation", Sp != "Technical Assistance Provided", grepl("20[0-9][0-9]", Fiscal.Year))%>%
          group_by(Type, Sp)%>%
          summarize(count = n_distinct(NMFS.Tracking.Number)), type = "bar", x = ~Sp, y = ~count, color = ~Type,
        colors = viridis(5),
        text = ~count, textposition = 'outside')%>%
  layout(#barmode = 'stack',
    xaxis = list(title = ""),
    yaxis = list(title = "Number of Consultations"),
    legend = list(x = 0.1, y = 1),
    margin = list(b = 100))


#TimeLine
plot_ly(data = filter(good_data, grepl("20[0-16]", Fiscal.Year))%>%
                        group_by(NMFS.Tracking.Number)%>%
          summarize(FY = first(Fiscal.Year), Type = first(Type))%>%
          group_by(FY, Type)%>%
          summarize(count= n()),
        x = ~FY, y = ~count, color = ~ Type, type = 'bar', colors = viridis(5))%>%
  layout(barmode = 'stack',
         xaxis = list(title = "Fiscal Year"),
         yaxis = list(title = "Consultations"),
         legend = list(x = 0, y = 1),
         margin = list(b = 100))

#Percentage of Jeopardy No-Jeopardy
plot_ly(data = filter(good_data, grepl("Formal", Type), !is.na(Sp), grepl("20", Fiscal.Year))%>%
          group_by(Fiscal.Year, Sp)%>%
          summarize(count= n())%>%
          left_join(        filter(good_data, grepl("Formal", Type), !is.na(Sp))%>%
                              group_by(Fiscal.Year)%>%
                              summarize(total = n()), by = "Fiscal.Year")%>%
          mutate(prop = count/total),

        x = ~Fiscal.Year, y = ~prop, color = ~ Sp, type = 'bar', colors = viridis(5))%>%
  layout(xaxis = list(title = "Fiscal Year"),
         yaxis = list(title = "Proportion of Determinations"),
         legend = list(y = 0.9),
         margin = list(b = 100),
         barmode = 'stack')


#Percentage of Jeopardy No-Jeopardy by Category
catprop_dt <- filter(good_data, grepl(".", Category), grepl("Formal", Type), Sp == "Jeopardy")%>%
  group_by(Category, NMFS.Tracking.Number)%>%summarize(count = n())%>%
  group_by(Category)%>%
  summarize(count= n())%>%
  left_join(filter(good_data, grepl(".", Category), grepl("Formal", Type), !is.na(Sp))%>%
              group_by(Category, NMFS.Tracking.Number)%>%summarize(count = n())%>%
              group_by(Category)%>%
              summarize(total = n()), by = "Category")%>%
  mutate(prop = count/total, non = total - count)%>%
  filter(total >= 10)
  top_n(10, prop)%>%arrange(prop)

vec <- catprop_dt$prop
names(vec) <- catprop_dt$Category

plot_ly(data = catprop_dt, type = 'bar')%>%
  add_trace(x = ~Category, y = ~ count, name = "Jeopardy",
        marker = list(color = viridis(2)[2]))%>%
  add_trace(x = ~Category, y = ~ non, name = "Other",
            marker = list(color = viridis(2)[1]),
            text = ~paste(round(prop * 100, 1), "%"), textposition = 'outside',
            textfont = list(color = 'black', size = 12))%>%
  layout(xaxis = list(title = "Work Category",
                      titlefont = list(color = 'black', size = 14),
                      categoryorder = 'array',
                      categoryarray = names(sort(vec)),
                      tickfont = list(color = 'black', size = 12)),
         yaxis = list(title = "Number of Consultations",
                      titlefont = list(color = 'black', size = 14),
                      tickfont = list(color = 'black', size = 12)),
         margin = list(b = 100),
         legend = list(x = 0.75, y = 0.75, font = list(color = 'black', size = 14)),
         barmode = 'stack')

#Percentage of Jeopardy No-Jeopardy by Species
spprop_dt <- filter(good_data, grepl(".", Common.Name), grepl("Formal", Type),
                    Sp =="Jeopardy")%>%
  group_by(Common.Name, NMFS.Tracking.Number)%>%summarize(count = n())%>%
  group_by(Common.Name)%>%
  summarize(count= n())%>%
  left_join(filter(good_data, grepl(".", Common.Name), grepl("Formal", Type), !is.na(Sp))%>%
              group_by(Common.Name, NMFS.Tracking.Number)%>%summarize(count = n())%>%
              group_by(Common.Name)%>%
              summarize(total = n()), by = "Common.Name")%>%
  mutate(prop = count/total, non = total - count)%>%
  filter(total >= 10)%>%
  top_n(10, prop)%>%arrange(prop)

spprop_chi <- chisq.test(x = rbind(spprop_dt$count, spprop_dt$non))

vec <- spprop_dt$prop
names(vec) <- spprop_dt$Common.Name

plot_ly(data = spprop_dt, type = 'bar')%>%
  add_trace(x = ~Common.Name, y = ~ count, name = "Jeopardy",
            marker = list(color = viridis(2)[2]))%>%
  add_trace(x = ~Common.Name, y = ~ non, name = "Other",
            marker = list(color = viridis(2)[1]),
            text = ~paste(round(prop * 100, 1), "%"), textposition = 'outside',
            textfont = list(color = 'black', size = 12))%>%
  layout(showlegend = FALSE,
         xaxis = list(title = "Species",
                      titlefont = list(color = 'black', size = 14),
                      categoryorder = 'array',
                      categoryarray = names(sort(vec)),
                      tickangle = 60,
                      tickfont = list(color = 'black', size = 12)),
         yaxis = list(title = "Number of Consultations",
                      titlefont = list(color = 'black', size = 14),
                      tickfont = list(color = 'black', size = 12)),
         margin = list(b = 150),
         legend = list(x = 0.85, y = 1),
         barmode = 'stack')
#State map
plot_geo(data = filter(good_data, grepl("20", Fiscal.Year), grepl("Informal", Type))%>%
           group_by(NMFS.Tracking.Number)%>%
           summarize(State = first(State))%>%
           group_by(State)%>%summarize(count = n()), locations = ~State, locationmode = 'USA-states')%>%
  add_trace(z = ~count, text = "boner", colors = viridis(256))%>%
  layout(geo = g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  ))

#

#Chi-square consultations by Region
dt <- filter(good_data, grepl("20", Fiscal.Year), grepl("Formal", Type))%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(LR = first(NMFS.Lead.Region))%>%
  mutate(newLR = ifelse(LR == "SWR", "WCR", ifelse(LR == "NWR", "WCR", as.character(LR))))%>%
  group_by(newLR)%>%
  summarize(count = n())%>%
  select(count)

reg_chi <- chisq.test(dt)

#Linear model consultations through time
dt <- filter(good_data, grepl("20", Fiscal.Year), Fiscal.Year != "2017", grepl("Informal", Type))%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(FY = first(Fiscal.Year), Type = first(Type), LR = first(NMFS.Lead.Region))%>%
  mutate(newLR = ifelse(LR == "SWR", "WCR", ifelse(LR == "NWR", "WCR", as.character(LR))))%>%
  group_by(FY, Type, newLR)%>%
  summarize(count= n())

lm_reg <- lm(data = group_by(dt, FY)%>%summarize(count= sum(count)), count ~ as.numeric(FY))

#coocurrence analysis
species_jeopardies <- filter(good_data, grepl(".", Common.Name), Sp == "Jeopardy", grepl("20[0-9][0-9]", Fiscal.Year))%>%
  group_by(Common.Name, NMFS.Tracking.Number)%>%summarize(Category = first(Category))
species_x_category <- as.matrix(table(species_jeopardies$Common.Name[grepl(".", species_jeopardies$Category)], species_jeopardies$Category[grepl(".", species_jeopardies$Category)]))
species_x_biop <- table(species_jeopardies$Common.Name, species_jeopardies$NMFS.Tracking.Number)

pop_jeopardies <- filter(good_data, grepl(".", Common.Name), Sp == "Jeopardy", grepl("20[0-9][0-9]", Fiscal.Year))%>%
  group_by(Common.Name, Population, NMFS.Tracking.Number)%>%summarize(Category = first(Category))%>%
  mutate(Name = paste(Common.Name, " (", Population, ")", sep = ""))
pop_x_biop <- table(pop_jeopardies$Name, pop_jeopardies$NMFS.Tracking.Number)
pop_x_category <- as.matrix(table(pop_jeopardies$Name[grepl(".", pop_jeopardies$Category)], pop_jeopardies$Category[grepl(".", pop_jeopardies$Category)]))

combos <- as.data.frame(group_by(good_data, Common.Name, Population)%>%summarize(count = n()))

comat <- table(good_data$Common.Name[grepl("20[0-9][0-9]", good_data$Fiscal.Year) & grepl(".", good_data$Common.Name)], good_data$NMFS.Tracking.Number[grepl("20[0-9][0-9]", good_data$Fiscal.Year)& grepl(".", good_data$Common.Name)])
comat_out <- cooccur(comat, type = "spp_site", thresh = FALSE, spp_names = TRUE, only_effects = TRUE, eff_matrix = TRUE)

comat_jeopardies <- table(good_data$Common.Name[grepl("20[0-9][0-9]", good_data$Fiscal.Year) & grepl(".", good_data$Common.Name) & good_data$Sp == "Jeopardy"], good_data$NMFS.Tracking.Number[grepl("20[0-9][0-9]", good_data$Fiscal.Year)& grepl(".", good_data$Common.Name) & good_data$Sp == "Jeopardy"])
comat_jeopardies_out <- cooccur(species_x_biop, type = "spp_site", thresh = FALSE, spp_names = TRUE, only_effects = TRUE, eff_standard = FALSE, eff_matrix = TRUE)
comat_pop_jeopardies <- cooccur(pop_x_biop[grepl("Steelhead|Salmon", rownames(pop_x_biop)),], type = "spp_site", thresh = FALSE, spp_names = TRUE, only_effects = TRUE, eff_standard = TRUE, eff_matrix = TRUE)
coprob_jeopardies <- cooccur(species_x_biop, type = "spp_site", thresh = TRUE, spp_names = TRUE)
coprob_pop_jeopardies <- cooccur(pop_x_biop[grepl("Steelhead|Salmon", rownames(pop_x_biop)),], type = "spp_site", thresh = TRUE, spp_names = TRUE)
coprob_table <- coprob_jeopardies$results
coprob_pop_table <- coprob_pop_jeopardies$results

prob_pop_mat <- write.csv(acast(arrange(coprob_pop_table, sp1_name, sp2_name), sp2_name ~ sp1_name, value.var = "prob_cooccur"), file = "probmat.csv")

over_pop_mat <- matrix(NA, nrow = 48, ncol = 48)
rownames(over_pop_mat) <- sort(unique(c(dist$sp1_name, dist$sp2_name)))
colnames(over_pop_mat) <- sort(unique(c(dist$sp1_name, dist$sp2_name)))
for(i in 1:nrow(dist)){
  over_pop_mat[rownames(over_pop_mat) == dist$sp1_name[i], colnames(over_pop_mat)==dist$sp2_name[i]] <- dist$Overlap[i]
}

mantel(prob_pop_mat, over_pop_mat, na.rm = TRUE, permutations = 999)

plot_ly(z = as.matrix(comat.out),
        x = ~colnames(as.matrix(comat.out)),
        y = ~rownames(as.matrix(comat.out)),
        type = 'heatmap', zmin = 0, zmax = 0.1)%>%
  layout(margin = list(b = 100, l = 100),
         xaxis = list(title = ""),
         yaxis = list(title = ""))

plot_ly(z = as.matrix(comat_jeopardies_out*ncol(comat_jeopardies)),
        x = ~colnames(as.matrix(comat_jeopardies_out)),
        y = ~rownames(as.matrix(comat_jeopardies_out)),
        type = 'heatmap', zmin = -2, zmax = 8)%>%
  colorbar(title = "Effect <br>Size",
           orientation = 'h',
           titlefont = list(size = 16, color = 'black'),
           tickfont = list(size = 14, color = 'black'))%>%
  layout(legend = list(orientation = 'h'),
         margin = list(b = 150, l = 150),
         xaxis = list(title = "", tickangle = 60, tickfont = list(color = 'black', size = 12)),
         yaxis = list(title = "", tickfont = list(color = 'black', size = 12))
  )

#Create 1000 simulated speciex x category jeopardy frequency matrices
permtest <- permatfull(species_x_category, fixedmar = 'both', mtype = "count", times = 1000)
#For each simulation, record whether the observed frequency was lower
permtests <- lapply(permtest$perm, function(i){return(i >= species_x_category)})

#calculate mean and variance for simulations
arr <- array(NA, dim = c(45, 9, 1000))
for (i in 1:length(permtest$perm)){
  arr[, , i] <- permtest$perm[[i]]
}

permmean <- apply(arr, c(1,2), mean)
dimnames(permmean) <- dimnames(permeff)
permsd <- apply(arr, c(1,2), sd)
dimnames(permsd) <- dimnames(permeff)

#Calculate the difference between the observed frequencies and mean simulated frequency
permeff <- species_x_category - permmean

permupper <- species_x_category - (permmean+permsd)
permlower <- species_x_category - (permmean-permsd)

#mean frequency from simulations
permmean <- Reduce('+', permtest$perm)/1000

#p-value
permstats <- Reduce('+', permtests)/1000

p <- plot_ly(z = permeff, #species_x_category
        x = ~colnames(species_x_category),
        y = ~rownames(species_x_category), type = 'heatmap',
        zmin = -5, zmax = 10
)%>%
  colorbar(title = "Effect<br>Size", titlefont = list(color = 'black', size = 16),
           tickfont = list(color = 'black', size = 14))%>%
  layout(margin = list(b = 100, l = 200),
         xaxis = list(title = "Work Type",
                      titlefont = list(color = 'black', size = 16),
                      tickfont = list(color = 'black', size = 14),
                      tickangle = 45),
         yaxis = list(title = "Listed Species",
                      titlefont = list(color = 'black', size = 16),
                      tickfont = list(color = 'black', size = 14),
                      tickangle = 330))

table(good_data$Sp[grepl("Formal", good_data$Type)], good_data$Action.Agency.Proposed.Effect.Determination..Species.[grepl("Formal", good_data$Type)])
table(good_data$Agency[grepl("Formal", good_data$Type)])

plot_ly(data = filter(good_data, !is.na(Discrepancy), grepl("Formal", Type))%>%
          group_by(Agency, Discrepancy)%>%
          summarize(count = n(), tot = n_distinct(NMFS.Tracking.Number))%>%
          group_by(Agency)%>%
          mutate(freq = count/sum(count))%>%
          filter(sum(tot) > 20)%>%
          bind_rows(filter(good_data, !is.na(Discrepancy), grepl("Formal", Type), grepl("20[0-9][0-9]", Fiscal.Year))%>%
                      group_by(Discrepancy)%>%
                      summarize(count = n())%>%
                      mutate(freq = count/sum(count), Agency = "All Agencies")),
        type = 'bar', x = ~freq, y = ~Agency, color = ~Discrepancy,
        #text = ~count, textposition = 'outside',
        colors = c(plasma(12)[c(1,4,6)], 'grey', plasma(12)[c(8,10,12)]),
        hoverinfo = 'text',
        text = ~paste(Agency, count, round(freq, 2))
        )%>%
  layout(barmode = 'stack',
         legend = list(orientation = 'h', x = 0.5, y = 100),
         xaxis = list(title = "Proportion of Determinations",
                      titlefont = list(color = 'black'),
                      tickfont = list(color = 'black')),
         yaxis = list(title = "",
                      tickfont = list(color = 'black', size = 10)),
         margin = list(l = 300, r = 0, t = 0))

kstests <- bind_rows(
  lapply(unique(good_data$Agency), function(i){
  n <- filter(good_data, grepl("Formal", Type), Agency == i)%>%summarize(count = n_distinct(NMFS.Tracking.Number))
  if(n >= 10){
    tst <- ks.test(as.numeric(good_data$Discrepancy[good_data$Agency == i & grepl("Formal", good_data$Type)]), as.numeric(good_data$Discrepancy[grepl("Formal", good_data$Type)]))
    p <- tst$p.value
    stat <- tst$statistic
  }else{
    p <- NA
    stat <- NA
  }
  out <- data.frame(Agency = i, count = n, KS = stat, sig = p)
  return(out)
  })
)

as.data.frame(filter(good_data, grepl("Formal", Type), grepl("20[0-9][0-9]", Fiscal.Year))%>%
                group_by(Population)%>%
                summarize(total = n_distinct(NMFS.Tracking.Number))%>%
  left_join(filter(good_data, Sp == "Jeopardy", grepl("20[0-9][0-9]", Fiscal.Year))%>%
                group_by(Population)%>%
                summarize(count = n_distinct(NMFS.Tracking.Number)))%>%
    mutate(p = count/total)%>%
    arrange(desc(p)))

#Kappa statistics
#starting point for weights matrix
cktests <- bind_rows(
  lapply(unique(kstests$Agency[!is.na(kstests$KS)]), function(i){
    #n <- filter(good_data, grepl("Formal", Type), Agency == i)%>%summarize(count = n_distinct(NMFS.Tracking.Number))
    Kap <- tryCatch({
      dat <- filter(good_data, grepl("Formal", Type),
                    Agency == "Bureau of Land Management", !is.na(Discrepancy),
                    !grepl("Proposed", Action.Agency.Proposed.Effect.Determination..Species.))%>%
        select(Action.Agency.Proposed.Effect.Determination..Species., Sp)
      kweights <- cohen.kappa(dat,
        w =  1-matrix(c(1.00,0.75,0.50,0.25,0.00,
                      0.75,1.00,0.75,0.50,0.25,
                      0.50,0.75,1.00,1.00,1.00,
                      0.25,0.50,1.00,1.00,0.75,
                      0.00,0.25,1.00,0.75,1.00),
                    nrow = 5),
        levels = c('No Effect', 'Not Likely to Adversely Affect', 'Likely to Adversely Affect', 'No Jeopardy', 'Jeopardy')
      )
      wk <- kweights$kappa
    },
    error = function(e){
      return(NA)
    }
    )
    out <- data.frame(Agency = i, Kap = Kap)
    return(out)
  }
  )
)


##HOMEMADE KAPPA STATISTIC USING ALL CONSULTATIONS AS REFERENCE DISTRIBUTION
total <- filter(good_data, grepl("Formal", Type),
              !is.na(Discrepancy),
              !grepl("Proposed", Action.Agency.Proposed.Effect.Determination..Species.))

tot <- select(total, Action.Agency.Proposed.Effect.Determination..Species., Sp)

levs <- c("No Effect", "Not Likely to Adversely Affect", "Likely to Adversely Affect", "No Jeopardy", "Jeopardy")

expect <- table(tot)
expect <- cbind(expect, "Likely to Adversely Affect" = c(0,0,0))%>%
  rbind("Jeopardy" = c(0,0,0,0,0), "No Jeopardy" = c(0,0,0,0,0))
expect <- expect[levs, levs]
marg1 <- margin.table(expect/sum(expect), 1)
marg2 <- margin.table(expect/sum(expect), 2)
expect <- marg1%o%marg2
w <-  1-matrix(c(1.00,0.75,0.50,0.25,0.00,
                 0.75,1.00,0.75,0.50,0.25,
                 0.50,0.75,1.00,1.00,1.00,
                 0.25,0.50,1.00,1.00,0.75,
                 0.00,0.25,1.00,0.75,1.00),
               nrow = 5)

cktests <- lapply(unique(kstests$Agency[!is.na(kstests$KS)]), function(i){
  wKap <- tryCatch({
    dat <- filter(total, Agency == i)%>%
      select(Action.Agency.Proposed.Effect.Determination..Species., Sp)

    obs <- table(dat)
    row_to_add <- matrix(rep(0, 5*(5-nrow(obs))), nrow = 5-nrow(obs))
    row.names(row_to_add) <- levs[!levs%in%row.names(obs)]
    col_to_add <- matrix(rep(0, nrow(obs)*(5-ncol(obs))), ncol = 5-ncol(obs))
    colnames(col_to_add) <- levs[!levs%in%colnames(obs)]

    obs <- cbind(obs, col_to_add)%>%
      rbind(row_to_add)

    obs <- prop.table(obs[levs, levs])

    wK <- 1-(sum(w*obs)/sum(w*expect))
  },
  error = function(e){
    return(NA)
    })
  out <- data.frame(Agency = i, wKap = wKap)
  return(out)
}
)%>%bind_rows()

agree_tests <- left_join(kstests, cktests, by = "Agency")

plot_ly(data = agree_tests, type = 'scatter', mode = 'markers',
        x = ~KS, y = ~wKap,
        text = ~Agency, hoverinfo = 'text')
