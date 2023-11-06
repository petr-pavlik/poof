#include <Rcpp.h>
#include <cmath>
#include <vector>

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
// [[Rcpp::export]]
double kge(std::vector<double> x, std::vector<double> y) {
  double value = R_NaN;
  
  int n = x.size();
  
  if (n == y.size()) {
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
  } else {
    Rcpp::Rcerr << "Error: Vectors x and y must be of equal length.\n";
  }
  
  return value;
}

/*
// -----------------------------------------------------------------------------
 Nash-Sutcliffe
 -----------------------------------------------------------------------------
//' Nash-Sutcliff Efficiency
//'
//' @description Nash-Sutcliff efficiency function
//'
//' @param x numeric vector
//' @param y numeric vector of same length as x
//' 
//' @return KGE value
//'
// [[Rcpp::export]]
 double nse(std::vector<double> x, std::vector<double> y) {
   double value = R_NaN;
   
   int n = x.size();
   
   if (n == y.size()) {
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
   } else {
     Rcpp::Rcerr << "Error: Vectors x and y must be of equal length.\n";
   }
   
   return value;
 }
*/




