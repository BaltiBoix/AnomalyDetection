# ######################################################################################################
# MACHINE LEARNING TOOLBOX FILES IN R
#           Copyright (C) Nelson R. Manohar
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# ######################################################################################################
# @AUTHOR:  Nelson R. Manohar Alers
# @EMAIL:   manohar.nelson@gmail.com
# @DATE:    September, 2014
# @URL:     http://www.bitbucket.org/nelsonmanohar/machinelearning
# ######################################################################################################
LICENSETEXT = "These R code samples (version Sep/2014), Copyright (C) Nelson R. Manohar,
comes with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to 
redistribute it under conditions of the GNU General Public License." 
message( LICENSETEXT )
message("")
# ######################################################################################################


# ###################################################################################################
# cross validation assessment: metrics & plot and epsilon identification
# test/predict : metrics and plot
# 1) anomaly detection
# dataset reduction into clean dataset classifier: 1 and anomalous dataset: classifier 2 using large epsilon so as to get enough samples for d2
# compare combined prediction rate against single classifier dataset against two tier classifier set
# hierarchical classifier performance pipeline
# ###################################################################################################





# ###################################################################################################
# library(scatterplot3d)
source( 'utilities.R' )
source( 't_tests.R' )
source( 'fselect.R' )
# ###################################################################################################


# ###################################################################################################
MEAN = function( x_vector ) {
      m_x = (mean(x_vector)-median(x_vector))/2.0
      return( m_x )
}
# ###################################################################################################


# ###################################################################################################
# PROB_X = function( x_vector ) { return( dnorm( x_vector, mean(x_vector), sd(x_vector)) ) }
# ###################################################################################################
PROB_X = function( x_vector ) { 
      prob_x = dnorm( x_vector, mean(x_vector), sd(x_vector))
      prob_x = ifelse ( prob_x == 0, 1E-32, prob_x )
      return ( prob_x ) 
}
# ###################################################################################################


# ###################################################################################################
PROB_PRODUCT = function( x_vector ) { 
      return ( cumprod(x_vector)[length(x_vector)] ) 
}
# ###################################################################################################


# ###################################################################################################
# (optional) reshape/redefine correlated features
# compute the multivariant probabilit ( mu_vector, cov_mat ) model
# if non-invertibility found, and m>n: 
# then identify which features are linear combinations (eigenvalues, eigenvectors?, equations: linearly)
# and redefine n-1 of them
# if this fails, revert to original model
# and explore the generation of features with multivariate effects
# generate the probabilities for training set
# output model
# ###################################################################################################
MULTIVARIATE_PROB_X = function( x_vector, mu_vector, sigma_det, sigma_inv ) {
      n = length(x_vector)
      x_vector   = DFROW_AS_VECTOR( x_vector )
      mu_vector  = DFROW_AS_VECTOR( mu_vector )
      x_minus_mu = DFROW_AS_VECTOR( x_vector - mu_vector )
      P0 = (2*pi)^(n/2) * sigma_det^(1/2)
      P1 = DFROW_AS_VECTOR(t(x_minus_mu) %*% sigma_inv) 
      P2 = (P1 %*% x_minus_mu)
      p_x = 1/P0 * exp( -P2/2.0 )
      return ( p_x )
}
# ###################################################################################################




# ###################################################################################################
DO_MULTIVARIATE_GAUSSIAN_ANOMALY_DETECTION = function( x_data, eps=1E-05, goal=0.95, debug=FALSE ) {
      x_pca = DO_PCA( x_data, silent=TRUE )
      
      m = nrow(x_data)
      n = ncol(x_data)
      
      mu_vector        = apply( x_data, 2, mean) 
      sigma_matrix     = cov( x_data, use="complete.obs", method='pearson' )
      inv_sigma_matrix = solve (sigma_matrix)
      det_sigma_matrix = det( sigma_matrix )
      z_probas         = apply( x_data, 1, MULTIVARIATE_PROB_X, mu_vector, det_sigma_matrix, inv_sigma_matrix )
      
      eps = quantile(z_probas,goal)
      
      detections = ifelse( z_probas<eps, TRUE, FALSE )
      n_anomalies = sum(detections) 
      
      while ( n_anomalies > (1.0-goal)*m ) {
            print( sprintf( "Found %8d anomalies at given %.8g epsilon threshold", n_anomalies, eps ) ) 
            eps = eps/2.00
            detections = ifelse( z_probas<eps, TRUE, FALSE )
            n_anomalies = sum(detections) 
      }
      print( sprintf( "DETECTED %s anomalies at %.6g epsilon threshold corresponding to %.1f%% non-anomalous coverage goal", n_anomalies, eps, 100*goal ) ) 
      
      psizes = ifelse( z_probas<eps, as.integer(16*(1-z_probas)), as.integer(12*(1-z_probas)) )
      colors = ifelse( z_probas<eps, "red", "blue" )
      symbs  = ifelse( z_probas<eps,  "o",    "+" )
      
      xlimits = c(min(x_pca$Z[,1])-1.0, max(x_pca$Z[,1])+1.0 )
      ylimits = c(min(x_pca$Z[,2])-1.0, max(x_pca$Z[,2])+1.0 )
      plot( x_pca$Z[,1], x_pca$Z[,2], pch=symbs, col=colors, cex=0.8, xlim=xlimits, ylim=ylimits )
      # scatterplot3d ( x_pca$Z[,1], x_pca$Z[,2], z_probas,  pch=symbs, col=colors, cex=0.8, xlim=xlimits, ylim=ylimits )
      
      Ureduce = x_pca$Ureduce
      
      retvals = list ( "epsilon"=eps, 'mu'=mu_vector, 'sigma'=sigma_matrix, "coverage_goal"=goal, "detections"=detections, "probas"=z_probas, 'Ureduce'=Ureduce, 'sigma_inv'=inv_sigma_matrix, 'sigma_det'=det_sigma_matrix )
      return ( retvals )
}
# ###################################################################################################


# ###################################################################################################
GET_ANOMALOUS_SAMPLES = function( x, detections, probas ) {
      NEWLINE(2)
      print( x[rownames(x)[detections], ] )
      NEWLINE(2)
      for ( i in 1:nrow(x) )
            if ( detections[i] )
                  print( sprintf( "%8d sample is anomalous having gaussian-derived probability %s", i, probas[i] ) )
      NEWLINE(2)
      retvals = list( 'detections'=x[detections,], 'probas'=probas[detections] )
      return( retvals )
}
# ###################################################################################################


# ###################################################################################################
TRANSFORM_OPTIONS = list(function(z) z+1/(abs(z)+1),
                         function(z) 1/(abs(z)+1),
                         function(z) 1/(log(abs(z))+1),
                         function(z) sqrt(abs(z)),
                         function(z) z+sqrt(abs(z)),
                         function(z) z^2+z+sqrt(abs(z)),
                         function(z) z^3+z^2+z+sqrt(abs(z)),
                         function(z) 1/(sqrt(abs(z)+1)),
                         function(z) 1/(z+sqrt(abs(z))+1),
                         function(z) 1/(z^2+z+sqrt(abs(z))+1),
                         function(z) log(abs(z)+1),
                         function(z) z+log(abs(z)+1),
                         function(z) z^2+z+log(abs(z)+1),
                         function(z) z^2+sqrt(abs(z))+log(abs(z)+1),
                         function(z) z^1/3+sqrt(abs(z/2))+log(abs(z)+1),
                         function(z) z^2+z+sqrt(abs(z))+log(abs(z)+1),
                         function(z) sqrt(abs(z))+log(abs(z)+1),
                         function(z) sqrt(abs(z))+1/(abs(z)+1),
                         function(z) z^2,
                         function(z) (z+1)^2,
                         function(z) (z-1)^2,
                         function(z) (z/2-1)^2,
                         function(z) z^3,
                         function(z) (z+1)^3,
                         function(z) (z-1)^3,
                         function(z) z^4,
                         function(z) abs(z)^(1/3),
                         function(z) abs(z/2)^(1/3),
                         function(z) abs(z/2+1)^(1/3),
                         function(z) abs(z/2-1)^(1/3),
                         function(z) abs(z+1)^(1/3),
                         function(z) abs(z-1)^(1/3),
                         function(z) abs(z)^1/2+abs(z)^(1/3),
                         function(z) abs(z-1)^1/2+abs(z-1)^(1/3),
                         function(z) abs(z+1)^1/2+abs(z+1)^(1/3),
                         function(z) abs(z+1)^1/2+abs(z-1)^(1/3),
                         function(z) 1/(sqrt(abs(z))+1),
                         function(z) 1/(sqrt(abs(z-1))+1),
                         function(z) 1/(log(abs(z))+sqrt(abs(z))+1),
                         function(z) 1/(z+sqrt(abs(z))+1),
                         function(z) 1/(z^1/2+sqrt(abs(z))+1),
                         function(z) z+1/(sqrt(abs(z))+1),
                         function(z) z^2+1/(sqrt(abs(z))+1),
                         function(z) (z-1)^2+1/(sqrt(abs(z))+1),
                         function(z) log(abs(z))/(sqrt(abs(z))+1),
                         function(z) sqrt(abs(z))/(log(abs(z))+1),
                         function(z) exp(-z),
                         function(z) cos(z),
                         function(z) cos(z)+exp(-z),
                         function(z) 1/(cos(z)+exp(-z)),
                         function(z) 1/(exp(-z)),
                         function(z) z )
# ###################################################################################################


# ###################################################################################################
DO_FEATURE_TRANSFORM_FOR_ANOMALY_DETECTION = function( x, plevel=5E-4, debug=FALSE, ... ) {
      new_x = data.frame()
      START = TRUE
      retvals = list()
      opt_f = list()
      opt_p = list()
      which_ones = list()
      for (i in 1:ncol(x)) {
            NEWLINE( 5 )
            summary(x[,i])
            retvals[[i]] = list()
            j = 0
            pvals = c()
            for ( f_transform in TRANSFORM_OPTIONS  ) {
                  if ( debug ) print( f_transform )
                  j = j + 1
                  z = f_transform( x[,i] )
                  rval = IS_NORMALLY_DISTRIBUTED( z, plevel=plevel, debug=debug, ... )
                  is_normal = rval$decision
                  p_value   = rval$pval
                  retvals[[i]][[j]] = list( is_normal, f_transform, p_value )
                  pvals = append( pvals, p_value )
                  if ( is_normal == TRUE ) break
            }
            
            opt_p [[i]]   = max( pvals ) 
            if ( max(pvals)>5E-7 )
                  which_one     = which(pvals==max(pvals))[1]
            else
                  which_one     = length(TRANSFORM_OPTIONS)
            which_ones = append( which_ones, which_one )
            
            opt_f [[i]]   = TRANSFORM_OPTIONS[[which_one]]
            opt_transform = TRANSFORM_OPTIONS[[which_one]]
            z = opt_transform( x[,i] )
            
            cat(HEADER)
            print( opt_transform )
            print( max(pvals ))
            print( which_one )
            cat(HEADER)
            print( summary( x[,i] ) )
            cat(HEADER)
            print( summary( z ) )
            cat(HEADER)
            
            if ( START ) {
                  new_x = data.frame(z)
                  START = FALSE
            } else {
                  new_x  = cbind( new_x, z )
            }
      }
      
      rownames(new_x)=rownames(x)
      colnames(new_x)=colnames(x)
      retval = list( 'Z'=new_x, 'opt_transforms'=opt_f, 'opt_pvals'=opt_p )
      return( retval )
}
# ###################################################################################################


# ###################################################################################################
DO_ANOMALY_DETECTION = function( x_data, eps=1E-3, goal=0.975, debug=FALSE ) {
      x_pca = DO_PCA( x_data, silent=TRUE )
      
      m = nrow(x_data)
      n = ncol(x_data)
      
      mu_vector = apply( x_data, 2, mean) 
      sd_vector = apply( x_data, 2, sd ) 
      
      z_independent_probas  = apply( x_data, 2, PROB_X )
      z_probas              = apply( z_independent_probas, 1, PROB_PRODUCT )
      
      detections = ifelse( z_probas<eps, TRUE, FALSE )
      n_anomalies = sum(detections) 
      while ( n_anomalies > (1.0-goal)*m ) {
            print( sprintf( "Found %8d anomalies at given %.8g epsilon threshold", n_anomalies, eps ) ) 
            eps = eps/2.00
            detections = ifelse( z_probas<eps, TRUE, FALSE )
            n_anomalies = sum(detections) 
      }
      print( sprintf( "DETECTED %s anomalies at %.6g epsilon threshold corresponding to %.1f%% non-anomalous coverage goal", n_anomalies, eps, 100*goal ) ) 
      
      psizes = ifelse( z_probas<eps, as.integer(16*(1-z_probas)), as.integer(12*(1-z_probas)) )
      colors = ifelse( z_probas<eps, "red", "blue" )
      symbs  = ifelse( z_probas<eps,  "o",    "+" )
      
      xlimits = c(min(x_pca$Z[,1])-1.0, max(x_pca$Z[,1])+1.0 )
      ylimits = c(min(x_pca$Z[,2])-1.0, max(x_pca$Z[,2])+1.0 )
      plot( x_pca$Z[,1], x_pca$Z[,2], pch=symbs, col=colors, cex=0.8, xlim=xlimits, ylim=ylimits )
      # scatterplot3d( x_pca$Z[,1], x_pca$Z[,2], z_probas, pch=symbs, col=colors, cex=0.8, xlim=xlimits, ylim=ylimits )
      
      Ureduce = x_pca$Ureduce
      
      retvals = list ( "epsilon"=eps, 'mu'=mu_vector, 'sigma'=sd_vector, "coverage_goal"=goal, "detections"=detections, "probas"=z_probas, 'Ureduce'=Ureduce )
      return ( retvals )
}
# ###################################################################################################


# ###################################################################################################
DO_ANOMALOUS_DETECTION_TRAINING = function( xdf, coverage_goal=0.990, ptype="univariate" ) {
      if ( ptype == "univariate" ) {
            increased_normality_dataset = DO_FEATURE_TRANSFORM_FOR_ANOMALY_DETECTION( xdf )
            TRANSFORMS_APPLIED =  increased_normality_dataset$opt_transforms
            Z = increased_normality_dataset$Z
            ANOMALY_DETECTION_VECTORS = DO_ANOMALY_DETECTION( Z, goal=coverage_goal )
      } else {
            TRANSFORMS_APPLIED =  c(rep(TRANSFORM_OPTIONS[length(TRANSFORM_OPTIONS)],ncol(xdf)))
            ANOMALY_DETECTION_VECTORS = DO_MULTIVARIATE_GAUSSIAN_ANOMALY_DETECTION( xdf, goal=coverage_goal )
      }
      
      detections = ANOMALY_DETECTION_VECTORS$detections
      z_probas   = ANOMALY_DETECTION_VECTORS$probas
      
      cat( HEADER )
      ret = GET_ANOMALOUS_SAMPLES( xdf, detections, z_probas )
      cat( HEADER )
      
      retvals = list ( 'detections'=ANOMALY_DETECTION_VECTORS, 'transforms'=TRANSFORMS_APPLIED )
      return ( retvals )
}
# ###################################################################################################


# ###################################################################################################
DO_MULTIVARIATE_ANOMALY_DETECTION_PREDICTION = function( x_sample, mu_vector, training_eps, sigma_inv, sigma_det  ) {
      x_probas         = apply( x_sample, 1, MULTIVARIATE_PROB_X, mu_vector, sigma_det, sigma_inv )
      txt = sprintf("%16.8f %16.8f %16.8f %16.8g", x_sample, x_sample, mu_vector, x_probas)
      decision = x_probas < training_eps
      retval = list( 'decision'=decision, 'probas'=x_probas, 'eps'=training_eps, 'itemization'=txt, 'transformed_x'=x_sample ) 
      return ( retval )
}
# ###################################################################################################


# ###################################################################################################
DO_ANOMALY_DETECTION_PREDICTION = function( x_sample, mu_vector, sd_vector, transforms_vector, training_eps, debug=FALSE ) {
      transformed_x = c() 
      for( i in 1:ncol(x_sample) ) {
            tx_val = transforms_vector[[i]] ( x_sample[i][[1]] )
            transformed_x = append( transformed_x, tx_val )
            if( debug ) {
                  print( x_sample[i][[1]] )
                  print( transforms_vector[[i]] )
                  print( paste( x_sample[i], transformed_x[i] ) )
            }
      }
      
      prob_x = dnorm( transformed_x, mu_vector, sd_vector )
      prob_x = ifelse( prob_x == 0, 1E-32, prob_x)
      txt = sprintf("%16.8f %16.8f %16.8f %16.8f %16.8g %s", x_sample, transformed_x, mu_vector, sd_vector, cumprod(prob_x), transforms_vector )
      
      x_probas = cumprod( prob_x )[length(prob_x)]
      decision = x_probas < training_eps
      
      retval = list( 'decision'=decision, 'probas'=x_probas, 'eps'=training_eps, 'itemization'=txt, 'transformed_x'=transformed_x ) 
      if ( debug & decision ) 
            print( retval )
      
      return ( retval )
}
# ###################################################################################################


# ###################################################################################################
NEWLINE(100)
require(mlbench)
data(BostonHousing)
x = BostonHousing[-4]
# ###################################################################################################
# ASSUME THESE TO BE THE LABELED ANOMALIES:
# [1] "     205 sample is anomalous having gaussian-derived probability 8.68177723686116e-10"
# [1] "     284 sample is anomalous having gaussian-derived probability 1.85006410594157e-10"
# [1] "     381 sample is anomalous having gaussian-derived probability 1.52463910057835e-26"
# [1] "     399 sample is anomalous having gaussian-derived probability 2.80146741651368e-10"
# [1] "     405 sample is anomalous having gaussian-derived probability 2.73998534377601e-10"
# [1] "     406 sample is anomalous having gaussian-derived probability 6.44878737067294e-18"
# [1] "     411 sample is anomalous having gaussian-derived probability 9.83378200645445e-15"
# [1] "     413 sample is anomalous having gaussian-derived probability 1.58737406670648e-10"
# [1] "     415 sample is anomalous having gaussian-derived probability 2.95596706133072e-15"
# [1] "     419 sample is anomalous having gaussian-derived probability 1.36479440137207e-22"
# [1] "     428 sample is anomalous having gaussian-derived probability 4.60761920701799e-12"
# ###################################################################################################
KNOWN_VERIFICATION_ANOMALIES = c( "381", "419" )
# ###################################################################################################
COVERAGE_GOAL  = 0.975
DETECTION_TYPE = "multivariate"
DETECTION_TYPE = "univariate"
TRAINING_TEST_SPLIT = 0.7


# ###################################################################################################
# DATA PREPROCESSING
# ###################################################################################################
X_SCALE_TRANSFORM = SCALE_DF( x, center=TRUE, scale=FALSE )
X_SCALED = X_SCALE_TRANSFORM$X
SCALED_XSAMPLES_DF = data.frame( X_SCALED )
# ###################################################################################################

# ###################################################################################################
# TRAINING/TEST SET SPLITS
# ###################################################################################################
WHAT_BEING_TRAINED_ON = sample(rownames(SCALED_XSAMPLES_DF), round(TRAINING_TEST_SPLIT*nrow(SCALED_XSAMPLES_DF)))
WHAT_BEING_TRAINED_ON = setdiff(WHAT_BEING_TRAINED_ON, KNOWN_VERIFICATION_ANOMALIES )

WHAT_BEING_TESTED_ON  = setdiff(rownames(SCALED_XSAMPLES_DF), WHAT_BEING_TRAINED_ON )
WHAT_BEING_TESTED_ON  = union(WHAT_BEING_TESTED_ON, KNOWN_VERIFICATION_ANOMALIES )

TRAIN_SET = SCALED_XSAMPLES_DF[WHAT_BEING_TRAINED_ON, ]
PRED_SET  = SCALED_XSAMPLES_DF[WHAT_BEING_TESTED_ON,  ]
# ###################################################################################################

# ###################################################################################################
# TRAIN
# ###################################################################################################
anomaly_detections_data = DO_ANOMALOUS_DETECTION_TRAINING( TRAIN_SET, coverage_goal=COVERAGE_GOAL, ptype=DETECTION_TYPE )
# ###################################################################################################

NEWLINE(10)
if ( DETECTION_TYPE == "univariate" ) {
      anomaly_detections = anomaly_detections_data[[1]]
      transforms_vector  = anomaly_detections_data[[2]]
      mu_vector          = anomaly_detections$mu
      sd_vector          = anomaly_detections$sigma
      training_eps       = anomaly_detections$epsilon
      training_Ureduce   = anomaly_detections$Ureduce
} else {
      anomaly_detections = anomaly_detections_data[[1]]
      transforms_vector  = anomaly_detections_data[[2]]
      mu_vector          = anomaly_detections$mu
      sd_matrix          = anomaly_detections$sigma
      training_eps       = anomaly_detections$epsilon
      training_Ureduce   = anomaly_detections$Ureduce
      sigma_inv          = anomaly_detections$sigma_inv
      sigma_det          = anomaly_detections$sigma_det
}

# ###################################################################################################
# TEST/PREDICT
# ###################################################################################################
# ###################################################################################################
# GENERAL SINGLE CASE BENCH MARK
# ###################################################################################################
if ( length(KNOWN_VERIFICATION_ANOMALIES)>0 ) {
      idx = KNOWN_VERIFICATION_ANOMALIES [1]
      x_sample = SCALED_XSAMPLES_DF[idx,]
      if ( DETECTION_TYPE == "univariate" ) {
            DO_ANOMALY_DETECTION_PREDICTION( x_sample, mu_vector, sd_vector, transforms_vector, training_eps )
      }
}
# ###################################################################################################

# ###################################################################################################
# TEST/PREDICT
# ###################################################################################################
anomalies = list()
for( i in rownames(PRED_SET) ) {
      x_sample = PRED_SET[i,]
      if ( DETECTION_TYPE == "univariate" ) {
            retvals = DO_ANOMALY_DETECTION_PREDICTION( x_sample, mu_vector, sd_vector, transforms_vector, training_eps )
      } else {
            retvals = DO_MULTIVARIATE_ANOMALY_DETECTION_PREDICTION( x_sample, mu_vector, training_eps, sigma_inv, sigma_det  )
      }
      if ( retvals$'decision' ) {
            # anomalous detection
            anomalies = append( anomalies, list( "rownum"=i, "analysis"=retvals ) )
            anomalous_x_transformed = retvals$'transformed_x'  
            anomalous_x_pca_sample = t(training_Ureduce) %*% DFROW_AS_VECTOR( anomalous_x_transformed )
            xx_point = anomalous_x_pca_sample[1]
            yy_point = anomalous_x_pca_sample[2] #+ 
            points( x=xx_point, y=yy_point, pch=24, bg="brown", col="brown", cex=1.0 )
            text( x=xx_point-rnorm(1,0,0.01), y=yy_point+rnorm(1,0,0.10), i, cex=0.8 )
      } else {
            # non_anomalous sample
      }
}
# ###################################################################################################

# ###################################################################################################
# USER FEEDBACK
# ###################################################################################################
NEWLINE(10)
print( anomalies )
NEWLINE(10)
cat( HEADER )
for ( j in seq(2,length(anomalies),2) ) {
      cat( HEADER )
      anomalous_sample_rownum = anomalies[j-1]
      anomalous_retvals = anomalies[j]$'analysis'
      anomalous_x_transformed = anomalous_retvals$'transformed_x'  
      anomalous_x_pca_sample = t(training_Ureduce) %*% DFROW_AS_VECTOR( anomalous_x_transformed )
      print( paste( j, length(anomalies), anomalous_sample_rownum ) )
      print( "x_orig" )
      print( anomalous_x_transformed )
      print( "x_pca" )
      print( DFROW_AS_VECTOR(anomalous_x_pca_sample) )
      cat( HEADER )
}
# ###################################################################################################