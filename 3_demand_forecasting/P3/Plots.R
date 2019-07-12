#Run the script "Spatial_df_for_predictions.R"
HH <- prepare_HH_post(FFF, 1)

#All
tt <- melt(HH[, c("high", "medium", "low", "Date")], id.vars = "Date")
tt[,c("Variable")] <- "rateable"
tt[,c("Spatial.Aggregation")] <- c("All.Households")

All_gardens <- tt
All_occupancy <- tt
All_tax <- tt
All_status <- tt
All_ACORN <- tt
All_rateable <- tt

ALL <-  rbind(All_gardens, All_occupancy, All_tax, All_status, All_ACORN, All_rateable)
colnames(All_rateable) <- c("Date", "Characteristic", "value", "Variable","Spatial.Aggregation")

#Large
tt <- melt(HH[, c(gardens, "Date")], id.vars = "Date")
tt[,c("Variable")] <- "gardens"

tt[,c("Spatial.Aggregation")] <- c("Large.Postcodes")

Large_gardens <- tt
Large_occupancy <- tt
Large_tax <- tt
Large_status <- tt
Large_ACORN <- tt
Large_rateable <- tt

Large <-  rbind(Large_gardens, Large_occupancy, Large_tax, Large_status, Large_ACORN, Large_rateable)
ALL_Large <-  rbind(Large, ALL)

colnames(Large) <- c("Date", "Characteristic", "value", "Variable","Spatial.Aggregation")

#Small  
tt <- melt(HH[, c(rateable, "Date")], id.vars = "Date")
tt[,c("Variable")] <- "rateable"

tt[,c("Spatial.Aggregation")] <- c("Small.Postcodes")

Small_gardens <- tt
Small_occupancy <- tt
Small_tax <- tt
Small_status <- tt
Small_ACORN <- tt
Small_rateable <- tt

Small <-  rbind(Small_gardens, Small_occupancy, Small_tax, Small_status, Small_ACORN, Small_rateable)
colnames(Small) <- c("Date", "Characteristic", "value", "Variable","Spatial.Aggregation")

ALL_ALL <-  rbind(Small, Large, ALL)

#Save your result
p <- 
  ggplot(All_rateable, aes(colour = Characteristic, x = Date, y = value)) +
  #  geom_point(size = 2, alpha =0.5) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  theme_minimal()+
  ylab('Occurence (%)')

pp <- 
  p + facet_grid(rows = vars(Variable), cols = vars(Spatial.Aggregation))+ 
  scale_fill_manual(values = col_vector)

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("Contr_Distr.png", pp, width = 7.5, height = 6, dpi = 1500)  

#Plot showing the consumption distribution for different aggregations of properties  
tt <- melt(HH[, c("X8", "Date")], id.vars = "Date")
tt[,c("Spatial.Aggregation")] <- c("Postcode_Areas")

Small <-  tt
Large <- tt
All <- tt

ALL_ALL <-  rbind(Ind, Small, Large, All)

ALL_ALL$Spatial.Aggregation_f = factor(ALL_ALL$Spatial.Aggregation, levels=
                                         c('All.Households','Large.Postcodes','Small.Postcodes','Indiv.Households'))

#Save your result
pp <- 
  ggplot(All, aes(x = Date, y = value)) +
  #  geom_point(size = 2, alpha =0.5) +
  geom_line(colour = "darkblue") +
#  scale_y_continuous(labels = comma) +
  theme_minimal()+
  ylab('Consumption (L/person/day)')+
  theme(legend.position="none")

pp <- 
  pp + facet_grid(rows = vars(Spatial.Aggregation_f))+ 
  # scale_fill_manual(values = col_vector)+
  ggtitle("Consumption distribution for different aggregations of properties")

setwd('//isad.isadroot.ex.ac.uk/UOE/User/Desktop')
ggsave("Cons_Distr.png", pp, width = 6, height = 4.8, dpi = 1500)  