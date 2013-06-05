# This is a file for testing the package, so we don't have to install during development.

# Load the Lalonde data set from CEM for testing. 
library(cem)
data(LL)
detach(package:cem)

# Function for loading all the files in a subdirectory
sourceDir <- function(path, trace = TRUE, ...) {
   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")           
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
   }
}

# Load files in subdirectory
sourceDir(paste(getwd(), '/finalFrontier', sep = ''))

# Create frontiers
my.L1.frontier <- finalFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"),
                                metric = 'L1')

my.Mahal.frontier <- finalFrontier(treatment="treated", dataset=LL, drop=c("re78", "treated"),
                                metric = 'Mahal')

# Make pretty picture
myform <- as.formula(re78 ~ treated +age + education + black + married + nodegree
                     + re74 + re75 + hispanic + u74 + u75)

# With mahalanobis
frontierPlot(my.Mahal.frontier, LL, myform, zoom = NULL)

# With L1
frontierPlot(my.L1.frontier, LL, myform, zoom = NULL)

# With Diff in means
# frontierPlot(my.Diff.frontier, LL, myform, zoom = NULL)

# Zoom in on 175 - 200 in the mahalanobis frontier
frontierPlot(my.Mahal.frontier, LL, myform, zoom = 175:200)

# Zoom a little closer - SEs disappear because we're so close, I'm working on this
frontierPlot(my.Mahal.frontier, LL, myform, zoom = 190:195)

# Now, we'll export the data set before the jump in effect size
Mahal.data <- generateDataset(my.Mahal.frontier, LL, 194)

# Parallel plot
# Brief note - for the moment, I just used the parallel plot in MASS. I'm working on a more appropriate wrapper.
parcoord(Mahal.data[,!(colnames(Mahal.data) %in% 'treated')], main = 'Parallel Plot for Mahalanobis Frontier', col = Mahal.data$treated + 1)
