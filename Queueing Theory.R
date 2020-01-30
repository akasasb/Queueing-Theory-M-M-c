m =2.8# number of servers(Rooms)
lambda =32# arrival rate
mu = 16# service rate

# efficiency
p = lambda / (m * mu)


# zero customer in the system
i = c(0:(m-1))
first = sum( (m * p) ^ i / factorial(i) )
p_0 = 1 / ( first + (m * p) ^ m / ( factorial(m) * (1-p) ) )


# n customer in the system
p_n = function(p_0, m, p, n){
  if( n <= m){ return( p_0 * ( (m*p)^n / factorial(n)) ) }
  else return( p_0 * ( m^m * p^n) / factorial(m) )
}

p_n(p_0, m, p, 4)

# the mean number of customers waiting in queue
L_q = p_0 * ( ((m^m) * p^(m+1)) / (factorial(m) * (1-p)^2) )


# Mean time customers spend in queue
W_q = L_q / lambda

# Mean time customers spend in the system
W = W_q + 1 / mu

# Mean number of customers in system
L = L_q + lambda / mu

v=lambda/mu
i = c(0:(m-1))
first = sum( ((lambda/mu)^i)*(m-i) / (m*factorial(i)) )
i = c(0:(m-1))
second.p1= sum ((v^i)/(factorial(i))) 
second.p2 <- ((v^(m))*(m))/((factorial(m))*(m-v))
second <- second.p1+second.p2
utilization <- 1-(first/second)
utilization
W_q # Mean time customers spend in queue
