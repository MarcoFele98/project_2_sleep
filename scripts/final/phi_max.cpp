#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double phi_max_rcpp(const LogicalVector &x, 
                    const LogicalVector &y) 
{
  IntegerMatrix occurence_table(2, 2);
  
  // Calculate occurence table
  for(int i = 0; i < x.size(); i++) {
    occurence_table.at(x.at(i), y.at(i)) += 1;
  }
  
  // Calculate phi max
  int p1, p2, q2, q1;
  p2 = sum(occurence_table(_, 1));
  p1 = sum(occurence_table(1, _));
  q2 = sum(occurence_table(_, 0));
  q1 = sum(occurence_table(0, _));
  
  double result;
  if(p2 >= p1) {
    result = sqrt(q2 * p1) / sqrt(q1 * p2);
  }
  else {
    result = sqrt(q1 * p2) / sqrt(q2 * p1);
  }
  
  return result;
}
