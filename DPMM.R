
  # A constant that defines the maximum possible clusters we expect in our data.
  MAX.CLUSTERS = 20
  
  # The following function generates a random cluster given a probability vector
  # Used to get the cluster assignment for an iteration in the DPMM procedure
  randClust = function(prob) {
    return (match(1, rmultinom(1,1,prob)))
  }
  # The following function performs stickbreaking and returns a vector of probabilities.
  # Need to specify the concentration parameter for the DPMM
  
  stick_breaking_process = function(num_weights = MAX.CLUSTERS, alpha) {
    betas = rbeta(num_weights, 1, alpha)
    remaining_stick_lengths = c(1, cumprod(1 - betas))[1:num_weights]
    weights = remaining_stick_lengths * betas
    return (weights)
  }
  
  #create a list data structure to hold the clusters
  clusters = list()
  # create the cluster labels - cluster_1, ..., cluster_MAX.CLUSTERS
  prefix = rep('cluster', MAX.CLUSTERS)
  suffix = seq(1:MAX.CLUSTERS)
  clus.labels = paste(prefix, suffix, sep = '_')
  
  
  
  # Initialize cluster assignment uniformly to these MAX.ClUSTERS
  
  init.cluster.assignments = function(num_rows) {
    init.prob = rep((1/MAX.CLUSTERS), MAX.CLUSTERS)
    
    for (i in 1:num_rows) {
      rand.clust = randClust(init.prob)
      assigned.cluster = paste('cluster', rand.clust, sep="_")
      clusters[[assigned.cluster]] <<- c(clusters[[assigned.cluster]], i)
      
    }
  }  
  



# The following function returns the cluster a point is currently assigned to

current.cluster.assigned = function(the_point) {
  the.cluster.for.point = NULL
  
  
  for (the.cluster in clus.labels) {
    
    if (the_point %in% clusters[[the.cluster]]) {
      the.cluster.for.point = the.cluster
    }
  }
  
  if(is.null(the.cluster.for.point)) {
    cat("Something wrong... could not find the cluster for the point", "\n")
  }
    
  
  return(the.cluster.for.point)
  
}


# The following function removes a point from a cluster
remove.point.from.cluster = function(point, the.cluster) {
  pl = clusters[[the.cluster]]
  pl = pl[! (pl %in% point)]
  clusters[[the.cluster]] <<- pl
  
}

# The following function adds a point to a cluster  
add.point.to.cluster = function(point, the.cluster)  {

  clusters[[the.cluster]] <<- c( clusters[[the.cluster]], point)
  
}


# The following is the simulation for DPMM

run.simulation = function(niter, num.rows) {
  init.cluster.assignments(num.rows)
  alpha = 0.38
  for (n in 1:niter) {
    for (i in 1:num.rows) {
      # do stick breaking to get a dirichelet distribution over the partitions
      dd = stick_breaking_process(MAX.CLUSTERS, alpha)
      new.cluster.for.i = randClust(dd)
      # current cluster for i
      cci = current.cluster.assigned(i)
      remove.point.from.cluster(i, cci)
      add.point.to.cluster(i, new.cluster.for.i)
      # This is just a sanity check
      cl = lapply(clusters, length)
      if( sum(unlist(cl)) != num.rows) {
        cat("Something wrong in cluster assignment logic", "\n")
      }
      # update alpha
      #alpha = updated.alpha(num.rows)
    }
     
  }
}

exp.clusters = function(alpha, n)  {
  val = alpha * log((alpha + n)/alpha)
  return (val)
}