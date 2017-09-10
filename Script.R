setwd('/Users/Ariel/Documents/School/Year 2/Spring/ECON 20900/Final Project')

# Load paired county data
pairsdata <- read.csv('County Pairs.csv')

# Clean up duplicate pairs
onetwo <- paste(pairsdata$County1FIPS, pairsdata$County2FIPS, sep = ":")
twoone <- paste(pairsdata$County2FIPS, pairsdata$County1FIPS, sep = ":")
singles <- rep(-1, length(onetwo))
set.seed(2017)
order <- sample(1:length(onetwo))
for (i in order)
{
  if (singles[i] == -1)
  {
    index <- match(onetwo[i], twoone)
    singles[index] = 0
    singles[i] = 1
  }
}
singlesrow <- 1 %in% singles
pairsdataU <- pairsdata[singles==1,]

pairsreg <- lm(100*pairsdataU$DiffUR ~ pairsdataU$DiffB + pairsdataU$DiffT + pairsdataU$DiffTb + pairsdataU$DiffSBTCI + pairsdataU$DiffGDPperW)
summary(pairsreg)

# Map paired counties
library(choroplethr)
library(choroplethrMaps)

countiesinpairs <- pairsdata$County1FIPS
region <- df_pop_county$region
value <- as.numeric(is.element(region, countiesinpairs))
df_countiesinpairs <- data.frame(region,value)

county_choropleth(df_countiesinpairs, num_colors = 1)


# County panel
countypanel <- read.csv('Final Data/County Panel CSV.csv')
countyfe <- plm(Unemployment.Rate ~ Average.Weekly.Benefit + Maximum.Weeks + Interaction + Compensation.Ratio + Income.per.Capita, index = c("County","Period"), model = "within", effect = "twoway", data = countypanel)
countyre <- plm(Unemployment.Rate ~ Average.Weekly.Benefit + Maximum.Weeks + Interaction + Compensation.Ratio + Income.per.Capita, index = c("County","Period"), model = "random", effect = "twoway", data = countypanel)
phtest(countyfe, countyre)

summary(countyfe)

sqrt(diag(vcovBK(countyfe, method = "white1", type = "HC0", cluster = "group")))


# Pairs panel
county1 <- rep(pairsdataU$County1FIPS,12)
county2 <- rep(pairsdataU$County2FIPS,12)
pair <- paste(county1, county2, sep = "&")
period <- ceiling(1:15660/1305)
county1period <- paste(county1, period, sep = ":")
county2period <- paste(county2, period, sep = ":")
dUR <- countypanel$Unemployment.Rate[match(county1period, countypanel$FIPS.Period)] - countypanel$Unemployment.Rate[match(county2period, countypanel$FIPS.Period)]
dAWB <- countypanel$Average.Weekly.Benefit[match(county1period, countypanel$FIPS.Period)] - countypanel$Average.Weekly.Benefit[match(county2period, countypanel$FIPS.Period)]
dMW <- countypanel$Maximum.Weeks[match(county1period, countypanel$FIPS.Period)] - countypanel$Maximum.Weeks[match(county2period, countypanel$FIPS.Period)]
dI <- countypanel$Interaction[match(county1period, countypanel$FIPS.Period)] - countypanel$Interaction[match(county2period, countypanel$FIPS.Period)]
dCR <- countypanel$Compensation.Ratio[match(county1period, countypanel$FIPS.Period)] - countypanel$Compensation.Ratio[match(county2period, countypanel$FIPS.Period)]
dIPC <- countypanel$Income.per.Capita[match(county1period, countypanel$FIPS.Period)] - countypanel$Income.per.Capita[match(county2period, countypanel$FIPS.Period)]
pairspanel <- data.frame(pair,period,dUR,dAWB,dMW,dI,dCR,dIPC)

pairsfe <- plm (dUR ~ dAWB + dMW + dI + dCR + dIPC, index = c("pair","period"), model = "within", effect = "twoway", data = pairspanel)
summary(pairsfe)

sqrt(diag(vcovBK(pairsfe, method = "white1", type = "HC0", cluster = "group")))

# Create tables
stargazer(countypanel)
stargazer(countyfe, pairsfe)
