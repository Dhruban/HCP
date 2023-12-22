DEBUG = TRUE
library(plot3D)
library(gglasso)
library(network)



plot_FC <- function(mat, p = nrow(mat)  # p x p matrix to be plotted
                    , labs = seq(1,nrow(mat)) # axis labels (brain regions, voxel no. etc.)
                    , ...  # further arguments passed to image2D
){
  image2D(mat, x = seq(1,p), y = seq(1,p), 
          # contour = list(levels = 0, col = "black", lwd = 2), 
          shade = 0, ...  # , main = "Partial Coherence Test, interval = 30", clab = "estimate"
  )
  rect(-50, 10, -20, 40, lwd = 3)
  axis(c(2), at=seq(1,p), labels=labs, las = 2, cex = 0.5) # data1$ui[seq(1,86)]
  axis(c(3), at=seq(1,p), labels=labs, las = 2, cex = 0.5) # data1$ui[seq(1,86)]
}


# pairwise correlation with parallelization
net_corr_parr = function(X
                         , method = 'pearson'){
  n = nrow(X)
  p = ncol(X)
  
  tmp = matrix(1:(p^2), nrow=p)
  ind = lowerTriangle(tmp, diag = FALSE)
  
  
  out <- foreach(kk = ind
                 , .combine = rbind
                 , .multicombine=TRUE)%dopar%{
                   j = floor((kk-1)/p)+1
                   i = kk%%p; if (!i) i = p
                   
                   r.p = matrix(0, nrow=1, ncol = 2)
                   f = cor.test(X[,i], X[,j], method = method)
                   r.p[1,1] = f$estimate
                   r.p[1,2] = f$p.value
                   
                   r.p
                 }
  r = array(0, c(p, p))
  lowerTriangle(r, diag=FALSE) <- out[,1]
  #   r = r+t(r); diag(r) = 1
  upperTriangle(r, diag=FALSE) <- lowerTriangle(r, diag=FALSE, byrow=TRUE); diag(r) = 1
  
  pval = array(0, c(p, p))
  lowerTriangle(pval, diag=FALSE) <- out[,2]
  #   pval =pval + t(pval); diag(pval) = 0
  upperTriangle(pval, diag=FALSE) <- lowerTriangle(pval, diag=FALSE, byrow=TRUE); diag(pval) = 0
  
  return(list(adj = r, pval = pval))
}




# pairwise VAR
net_pwvar_parr = function(X){
  n = nrow(X)
  p = ncol(X)
  XX = X[1:(n-1),]
  YY = X[2:n,]
  
  tmp = matrix(1:(p^2), nrow=p)
  ind = lowerTriangle(tmp, diag = FALSE)
  
  
  out <- foreach(kk = ind
                 , .combine = rbind
                 , .multicombine=TRUE)%dopar%{
                   j = floor((kk-1)/p)+1
                   i = kk%%p; if (!i) i = p
                   
                   ff1 = lm(YY[,i] ~ 0 + XX[,i] + XX[,j])
                   ff2 = lm(YY[,j] ~ 0 + XX[,i] + XX[,j])
                   
                   tmp = matrix(c(summary(ff1)$coefficients[2, c(1,4)]
                                  , summary(ff2)$coefficients[1, c(1,4)]), nrow=1, ncol=4)
                   tmp
                 }
  
  # diagonals are zero by construction here
  if (!DEBUG){ # old version - fill upper and lower triangles separately; delete later !!
    adj1 = array(0, c(p, p)); adj2 = adj1
    lowerTriangle(adj1, diag=FALSE) <- out[,1]
    lowerTriangle(adj2, diag=FALSE) <- out[,3]
    
    pval1 = array(0, c(p, p)); pval2 = pval1
    lowerTriangle(pval1, diag=FALSE) <- out[,2]
    lowerTriangle(pval2, diag=FALSE) <- out[,4]
    
    
    return(list(adj = adj1+t(adj2)
                , pval = pval1+t(pval2)
    )
    )
  }
  
  if (DEBUG){ # new version of filling lower and upper triangles
    adj = array(0, c(p, p))
    lowerTriangle(adj, diag=FALSE) <- out[,1]
    upperTriangle(adj, diag=FALSE, byrow=TRUE) <- out[,3]
    
    pval = array(0, c(p, p)); diag(pval) = 0
    lowerTriangle(pval, diag=FALSE) <- out[,2]
    upperTriangle(pval, diag=FALSE, byrow=TRUE) <- out[,4]
    
    return(list(adj=adj, pval=pval))
  }
}


fit2graph = function(adj, pval
                     , method='BH'
                     , thresh.cutoff = 0.01
                     , convert2undirected = TRUE  # change directed graphs to undirected
                     , wtd_adj = FALSE  # return graph with edge weights
                     , combine.directed.edges = 'and'){ # either 'and' or 'or'
  p = nrow(adj)
  
  grf = array(FALSE, c(p, p))
  #   lowerTriangle(pval, diag=FALSE) <- p.adjust(lowerTriangle(pval, diag=FALSE), method=method)
  #   upperTriangle(pval, diag=FALSE) <- p.adjust(upperTriangle(pval, diag=FALSE), method=method)
  
  tmp = c(lowerTriangle(pval, diag=FALSE), upperTriangle(pval, diag=FALSE))
  tmp = p.adjust(tmp, method=method)
  lowerTriangle(pval, diag=FALSE) <- tmp[seq(length(tmp)/2)]
  upperTriangle(pval, diag=FALSE) <- tmp[-seq(length(tmp)/2)]
  rm(tmp)
  
  if (any(pval!=t(pval)) & convert2undirected == TRUE){ # for a directed graph, use and/or rule to make it an undirected graph
    
    if (combine.directed.edges == 'and'){
      for (i in 1:(p-1)){
        for (j in (i+1):p){
          tmp = max(pval[i,j], pval[j,i])
          pval[i,j] = tmp
          pval[j,i] = tmp
        }
      }
    }
    
    if (combine.directed.edges == 'or'){
      for (i in 1:(p-1)){
        for (j in (i+1):p){
          tmp = min(pval[i,j], pval[j,i])
          pval[i,j] = tmp
          pval[j,i] = tmp
        }
      }
    }
    
  }  # end if (any(pval!=t(pval)) ... )
  
  if (wtd_adj == FALSE){
    grf= (pval < thresh.cutoff)+0
    diag(grf)=0  # make diagonal zero i.e no significant edges in diagonal
  }
  else{ # leave the edge weights in the graph, no self-loops though
    grf = adj*(pval < thresh.cutoff)+0
    diag(grf)=0
  }
  
  return(grf)
}
