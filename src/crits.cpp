#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <numeric>

using namespace Rcpp;


/* -----------------------------------------------------------------------------
      Kling-Gupta 2012
----------------------------------------------------------------------------- */

//' Kling-Gupta Efficiency
//'
//' @description Kling-Gupta efficiency function
//'
//' @param x numeric vector
//' @param y numeric vector of same length as x
//' 
//' @return KGE value
//'
// [[Rcpp::export(.KGE2011cpp)]]
double KGE2011cpp(std::vector<double> x, std::vector<double> y) {
  double value = R_NaN;
  
  int n = x.size();
  
  // if (n == y.size()) {
    // Check if both vectors have size 1 (single values)
    if (n == 1) {
      if (x[0] == y[0]) {
        return 1.0; // Perfect match
      } else {
        return 0.0; // No match
      }
    }
    
    // Calculate means and standard deviations for both vectors
    double mean_x = std::accumulate(x.begin(), x.end(), 0.0) / n;
    double mean_y = std::accumulate(y.begin(), y.end(), 0.0) / n;
    
    // if (type == 1) {
      
      double sd_x = 0.0;
      double sd_y = 0.0;
      
      for (int i = 0; i < n; i++) {
        sd_x += std::pow(x[i] - mean_x, 2);
        sd_y += std::pow(y[i] - mean_y, 2);
      }
      
      sd_x = std::sqrt(sd_x / (n - 1));
      sd_y = std::sqrt(sd_y / (n - 1));
      
      // Calculate the correlation coefficient (rxy)
      double prodsum = 0.0;
      double rxy = 0.0;
      
      for (int i = 0; i < n; i++) {
        prodsum += x[i] * y[i];
      }
      
      rxy = (prodsum - n * mean_x * mean_y) / ((n - 1) * sd_x * sd_y);
      
      // Calculate KGE value
      value = 1 - std::sqrt(
        std::pow(mean_x / mean_y - 1, 2) +
          std::pow(sd_x / sd_y - 1, 2) +
          std::pow(rxy - 1, 2)
      );
    // } else if (type == 2) {
    //   value = 1;
    // }
    // Error from wrong input
  // } else {
  //     Rcpp::Rcerr << "Error: Vectors x and y must be of equal length.\n";
  // }
    
  return value;
    
}

/* -----------------------------------------------------------------------------
 Nash-Sutcliffe Efficiency Criteria
 ----------------------------------------------------------------------------- */

//' Nash-Sutcliffe Efficiency
//'
//' @description Nash-Sutcliffe Efficiency wrapper function
//'
//' @param x numeric vector of observed values
//' @param y numeric vector of simulated values
//' 
//' @return NSE value
//'
// [[Rcpp::export(.NSEcpp)]]
double NSEcpp(std::vector<double> x, std::vector<double> y) {
  double value = R_NaN;
  
  int n = x.size();
  
  // Check if the sizes of x and y are equal
  if (n != y.size()) {
    Rcpp::stop("Input vectors must have the same length.");
  }
  
  // Calculate mean of observed values
  double mean_x = std::accumulate(x.begin(), x.end(), 0.0) / n;
  
  // Calculate NSE
  double numerator = 0.0;
  double denominator = 0.0;
  
  for (int i = 0; i < n; ++i) {
    numerator += pow(x[i] - y[i], 2);
    denominator += pow(x[i] - mean_x, 2);
  }
  
  value = 1.0 - numerator / denominator;
  
  return value;
  
}


/* -----------------------------------------------------------------------------
 Liu Mean Efficiency Criteria
 ----------------------------------------------------------------------------- */

//' Liu Mean Efficiency
//'
//' @description Liu Mean Efficiency wrapper function. This criteria aims to \deqn{\text{LME} = 1 - \sqrt{(k_1 - 1)^2 + (\beta - 1)^2}}
//' 
//' 
//'
//' @param x numeric vector of observed values
//' @param y numeric vector of simulated values
//' 
//' @return LME value
//'
// [[Rcpp::export(.LMEcpp)]]
double LMEcpp(std::vector<double> x, std::vector<double> y) {
  double value = R_NaN;
  
  int n = x.size();
  
  // Check if the sizes of x and y are equal
  if (n != y.size()) {
   Rcpp::stop("Input vectors must have the same length.");
  }
  
  // Calculate mean of observed values
  double mean_x = std::accumulate(x.begin(), x.end(), 0.0) / n;
  
  // Calculate NSE
  double numerator = 0.0;
  double denominator = 0.0;
  
  for (int i = 0; i < n; ++i) {
   numerator += pow(x[i] - y[i], 2);
   denominator += pow(x[i] - mean_x, 2);
  }
  
  value = 1.0 - numerator / denominator;
  
  return value;
 
}










