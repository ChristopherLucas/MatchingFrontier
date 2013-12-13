finalFrontier
=============

At the moment, the software does three frontiers. They are Mahalanobis FSATT, L1 SATT, and L1 FSATT (S = 0). 

# Key Functions

There are only a few functions you need to know. 

### makeFrontier

`makeFrontier` creates the frontier object. It stores the actual values for the frontier and the 
optimal combinations of the data but it does not do any of the estimation.

<code>makefrontier(treatment, dataset, drop, mdist = NULL, QOI, metric, S)</code>

### frontierEst

`frontierEst` estimates a quantity of interest along the defined values of the frontier. 

<code>frontierEst(frontierObject, dataset, myform=NULL, treatment=NULL, estCall=NULL, drop=NULL)</code>

### frontierPlot

`frontierPlot` plots the object from frontierEst.

<code>frontierPlot(frontierObject, dataset, frontierEstObject, zoom = NULL, drop=NULL)

### generateDataset

`generateDataset` returns a dataset corresponding to a specific point on the frontier. 

generateDataset(finalFrontierObject, dataset, number.dropped)