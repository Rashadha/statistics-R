#a)

# Set the seed for reproducibility
set.seed(123)

# Generate 10,000 weight values from a normal distribution with mean 60 and standard deviation 3
weights <- rnorm(10000, mean = 60, sd = 3)

# Generate 10,000 height values from a normal distribution with mean 1.6 and standard deviation 0.1
heights <- rnorm(10000, mean = 1.6, sd = 0.1)

# Calculate BMI using the formula BMI = weight / height^2
BMIs <- weights / heights^2

BMIs


#b)

# draw a histogram of the BMI values
hist(BMIs,main='Histrogram of BMI',xlab='BMIs')

#c)

# Find the approximate mean and variance of BMI
mean(BMIs)
var(BMIs)

#d)

# use the mean() function to estimate the 𝑝(𝐵𝑀𝐼 ≥ 25)
mean(BMIs>=25)