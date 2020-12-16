get_EIG<- function(lower, upper, guess) {
  k.cur = upper-lower+1
  h.cur = log2(k.cur)
  
  # Info if greater
  k.greater = upper-guess
  p.greater = k.greater / k.cur
  h.greater = ifelse(p.greater > 0, p.greater * log2(k.greater), 0)
  
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
  
  h.cur - h.next
}