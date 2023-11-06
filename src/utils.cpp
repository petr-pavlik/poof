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
