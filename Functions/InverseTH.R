############# Inverse Tukey H Transformation #############
library(LambertW)

# Purpose: Perform the inverse Tukey h transformation in Eq. (5) to Gaussianze the data x
# Inputs: x       data matrix 
#         omega   a parameter in Tukey h distribution indicating the scale of x
#         h       a parameter in Tukey h distribution indicating the level of heavy-tailness of x
# Output: Data after the inverse transformation

THinvfunc=function(x,omega,h){
  a=x/omega
  if(h==0){
    return(a)
  }
  if(h>0){
    return(sign(a)*sqrt(LambertW::W(h*a^2)/h))
  }
}