require(googlesheets)
require(magrittr)
require(Bchron)
#Read in data from google sheets - authetication required (copy and paste link into browser)
(my_sheets <- gs_ls())

#First register the sheets
gap <- gs_title("durville-age-depth")

#read sheet into dataframe
age_depth <- gap %>%
  gs_read(ws = "depths")

build_age_model <- function(site_location){
  #iterate through each site and create age depth model
  for(locations in site_location){
    site = locations$location[1]
    age_model = Bchronology(ages=locations$ages,ageSds=locations$ageSds,
                            calCurves=locations$calcurves,positions=locations$position,
                            positionThicknesses=locations$thickness,ids=locations$Id,
                            predictPositions=seq(0,locations$position[4]+25,by=10))
    
    #plot age depth model and send site name for title
    plot_model(age_model, site)
    
  }
}

plot_model <- function(site_to_plot, site) {
  #plot model - ifelse statements for clean plot axis
  plot(site_to_plot,main=site,xlab=ifelse((site=="Ohana"),"Age (cal years BP)",""),ylab=ifelse((site=="Moawhitu"),'Depth below sediment surface (cm)',""),las=1, dateHeight=3, legLoc = c(1000,1000))
}

#split data into list by individual locations
agelist = split(age_depth, age_depth$location)

#set ouput file as png
png(height=9, width=15, filename="age-depth-model-all.png", res=300,units = "cm")
#set rows, columns and margins (bottom, left, top, right) for plot
par(mfrow = c(1, 3), mar=c(5,5,2,1))

build_age_model(agelist)

dev.off()


