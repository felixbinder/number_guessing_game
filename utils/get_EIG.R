get_EIG<- function(lower, upper, guess) {
  k.cur = upper-lower+1 #number of available candidates
  h.cur = log2(k.cur) #entropy over available options (collapses from entropy calc in the case of equally likely options)
  #\Eta(X) = -\sum_{i=1}^n {\mathrm{P}(x_i) \log \mathrm{P}(x_i)}
  
  # Info if greater
  k.greater = upper-guess #upper range
  p.greater = k.greater / k.cur #probability of target being in upper range
  h.greater = ifelse(p.greater > 0, p.greater * log2(k.greater), 0) #weighted entropy
  
  # Info if lower
  k.lower = guess-lower
  p.lower = k.lower/k.cur
  h.lower = ifelse(p.lower > 0, p.lower * log2(k.lower), 0)
  
  # Info if correct
  k.correct = 1
  p.correct = k.correct/k.cur
  h.correct = p.correct * log2(k.correct)
  
  # Sum
  h.next  = h.lower + h.greater + h.correct
  
  h.cur - h.next #return expected information gain (old entropy minus new entropy for the three possible outcomes)
}