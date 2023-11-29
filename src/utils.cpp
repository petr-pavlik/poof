#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

//' [mm] to [m3/s] convert 
//'
//' @param mm milimeters
//' @param area in square kilometers
//'
//'
// [[Rcpp::export]]
std::vector<double> mm_d_m3_s(std::vector<double> mm, std::vector<double> area) {
  std::vector<double> value(mm);
  
  for (int i = 0; i < mm.size(); i++) {
    value[i] = mm[i] * 1000 * area[i] / (60 * 60 * 24);
  }
  
  return value;
  
}

//' [mm] to [m3/d] convert 
//'
//' @param mm milimeters
//' @param area in square kilometers
//'
//'
// [[Rcpp::export]]
std::vector<double> mm_d_m3_d(std::vector<double> mm, std::vector<double> area) {
  std::vector<double> value(mm);
  
  for (int i = 0; i < mm.size(); i++) {
    value[i] = mm[i] * 1000 * area[i];
  }
  
  return value;
  
}

//' [m3/d] to [mm] convert 
//'
//' @param area in square kilometers
//' @param mm milimeters
//'
//'
// [[Rcpp::export]]
std::vector<double> m3_s_mm_d(std::vector<double> mm, std::vector<double> area) {
  std::vector<double> value(mm);
   
  for (int i = 0; i < mm.size(); i++) {
    value[i] = mm[i] * 1000 * area[i];
  }
   
  return value;
   
}




//' Antecedent Precipitation Index
//'
//' @description API
//'
//' @param precipitation A vector of precipitation estimates/measurements
//' @param k Decay constant
//' @param t Number of days to account for, usually 5/7/14
//'
// [[Rcpp::export]]
std::vector<double> api(std::vector<double> precipitation, double k, int t) 
{
  
  std::vector<double> value(precipitation.size(), R_NaReal);
  std::vector<double> temp_value(t, R_NaReal);
  
  for (int i = value.size() - 1; i >= t - 1; i--)
  {
    for (int j = t - 1; j >= 0; j--)
    {
      temp_value[j] = precipitation[i - j] * std::pow(k, -j);
    }
    value[i] = std::accumulate(temp_value.begin(), temp_value.end(), 0.0);
  }
  return value;
}


