require(mixtools)
require(doBy)

fp = "/home/admin123/MLExperiments/PLFA/house-votes-84.csv"
hvdf = read.table(fp, na.strings = "?", sep = ",", stringsAsFactors = FALSE)
col.names = c("Party", "handicapped-infants", "water-project-cost-sharing",
              "adoption-of-the-budget-resolution", "physician-fee-freeze", 
              "el-salvador-aid", "religious-groups-in-schools",
              "anti-satellite-test-ban","aid-to-nicaraguan-contras",
              "mx-missile", "immigration","synfuels-corporation-cutback",
              "education-spending","superfund-right-to-sue", "crime",
              "duty-free-exports", "export-administration-act-south-africa")
names(hvdf) = col.names
hvdf[is.na(hvdf)] = 'n'
# Segregate into individual parties
democrats.df = filter(hvdf, Party == "democrat")

republican.df = filter(hvdf, Party == "republican")

# we do not need the Party column for counts
hvdf = hvdf[,2:17]
democrats.df = democrats.df[,2:17]
republican.df = republican.df[,2:17]
# Count the yes votes for each party


freq.dems = summarise_each(democrats.df, funs (nrow(filter(democrats.df, . == 'y'))))
rownames(freq.dems) = "democrat"

freq.reps = summarise_each(republican.df, funs (nrow(filter(republican.df, . == 'y'))))
rownames(freq.reps) = "republican"

freq.cong = summarise_each(hvdf, funs (nrow(filter(hvdf, . == 'y'))))

freq.table = rbind(freq.dems,freq.reps)
freq.table = t(freq.table)

fit = multmixmodel.sel(freq.table, comps = 1:10, epsilon = 0.001)

# The above fit tells us that optimum number of clusters is 6, so lets fit a 6 mixture
# model to the data

theta2 <- matrix(runif(28), ncol = 2)
theta.init <- theta2[1:6,]
mult6 <- multmixEM(freq.table, lambda = rep(1, 6)/6, theta = theta.init)