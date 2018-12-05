# Before we can begin discussion on data distributions, it is essential to
# have a basic understanding of probability.  Probability is first defined
# by an event.  There are two main types of events, compound and elemetary.
# Elementary events are defined as those which can only occur in one
# subevent, while compound events can be decomposed into many subevents.
# For example, when a dice is rolled, rolling a 1 would be an elementary
# event, while rolling an even number is a compound event.  Lets look at a
# dice rolling function to help us understand probability.

diceroll(1,1)

# In this function, the first number represents the number of rolls, while
# the second represents the number of dice.  If we were to roll a pair of
# dice, we would get

diceroll(1,2)

# Since we are new to probability, what are some questions we may have
# interest in knowing when we roll dice?  Maybe we want to know the
# probability of throwing a single number on one dice, or the probability
# of throwing a number using 2 dice.  Lets say we have a scenario where we
# desperately need to throw a 6, and we have 2 dice to do it.  What is the
# probability of throwing a 6?

# If you said 2/6, that is not correct.  We must instead consider all the
# possible ways to throw a 6.  If we throw a (1,6), (2,6), (3,6), (4,6),
# (5,6), (6,6), (6,5), (6,4), (6,3), (6,2), (6,1), we see there are 11
# possible combinations that will yield a 6, out of a possible 36 (6 * 6).
# Hence, the probability of throwing a 6 is 11/36, or

11/36

# This is smaller than 2/6.  However, we won't gain a
# lot of advantage by adding a third dice.  With three, we should instead
# think about those scenarios that WILL NOT yield a 6 (i.e. 5/6 * 5/6 *
# 5/6)

1 - (125)/216

# Lets see how many tries it takes us to throw two dice and get a 6.  We
# first need to set our random number generator to the same thing.

set.seed(27);

diceroll(1,2)

# You see that we got it on our first try.  Can we repeat that?  What is
# the probability that we can repeat that?

diceroll(1,2)

# We rolled a 6 again!  The probability of throwing a six on two
# consecutive rolls is (11/36) * (11/36)

(11/36) * (11/36)

# which is less than 10%.  Can we do it three times in a row?

diceroll(1,2)

# We cannot.  That doesn't mean it is impossible, just highly unlikely.

# How can we compute the probability of throwing a total of 2 from our two
# dice?  This can only be accomplished by a single combination, (1,1).
# Since we can only obtain this result from one combination, the
# probability of throwing this combination is (1/6) * (1/6), or 1/36.  What
# number has the highest probability of being thrown?  What is that
# probability?

# Well, to answer that question, we know there are three possible ways to
# throw a 7, by (1,6), (2,5), (3,4) and their opposites.  In this case,
# there are 6 possible outcomes for throwing a 7, which is the largest
# probability, 1/6.  What is the probability of throwing a 6 or an 8?
# In the game of craps, players are particularly interested in throwing a 
# 7 or an 11.  How many ways are there to throw a 7 or an 11, given two 
# fair dice?  What is the probability of throwing these numbers?

# The probability would be (6/36) [for the 7s] + (2/36) [for the 11s].
# Lets see how many tries it takes us to roll a 7 or an 11.

set.seed(4)

diceroll(1,2) # Not here
diceroll(1,2) # Not here
diceroll(1,2) # There's a 7!

# The probability is defined as 8/36:

8/36

# so is it surprising that we threw a 7 on our third try?  Maybe a little.


# Now that we have looked at basic probability, we need to further
# discuss probability through defining what is known as the sample space,
# or the event space.  This is a set of all possible elementary events for
# a given experiment.  For our dice experiment, it is simply the
# numbers 2 - 12 (the total of our dice roll cannot be less than 2 or
# greater than 12).  It is common in theoretical probability discussions to
# represent events within a sample space by using a Venn diagram.  In a
# Venn diagram, the event space is drawn as a rectangle, and the
# corresponding events are drawn as circles in the event diagram.  Lets
# consider three possible scenarios with our dice to illustrate the Venn
# diagram:

# 1)  We have two events, those in which our dice roll is less than 7, and
# those in which it is greater than 7.  What does the Venn diagram look
# like in this case?

# 2)  Lets make it more complex. We still have two events, only now we say
# that our dice rolls are less than or equal to 7 in one event, and greater
# than or equal to 7 in the other event.  

# 3)  Finally, lets consider three events, two of which are repeated in
# scenario #1`, and the third event being the probability that our dice
# roll equals 7.  

# We can define some properties of these events based on our Venn diagram.
# First, we can call an event that is unique to a given event type as
# mutually exclusive.  Scenarios 1 and 3 contain events which are mutually
# exclusive.  We can also call the guarantee that one of the events in each
# event type will occur as collectively exhaustive.  When considering the
# breakdown of dice rolls (i.e. which numbers make them up), which events
# are mutually exclusive?  Assume that the order doesn't matter on the roll
# of the dice (i.e. rolling a 2 and a 1 is a 3, likewise rolling a 1 and a
# 2 is a 3).

# Lets look at some of the properties of probability.  The first and most
# obvious definition of probability is that the domain limit on the
# probability of an event is simply:

# 0 <= Pr[E] <= 1

# where Pr[E] represents the probability of a given event.  Obviously, a 0
# probability means there is no chance of the event; a 1 probability
# guarantees the event will occur.  

# If event E1 necessarily occurs whenever event E2 occurs, event E2 is said
# to be a subset of E1.  For example, if frozen precipitation is occurring,
# the event of precipitation occurring in any form must necessarily also be
# occurring.  In this case:

# Pr[E1] >= Pr[E2]

# In probability, we can also define the complement of an event.  The
# complement is simply the probability that the event will not occur.
# Thus, the sum of an event's probability and its complement will be 1, or
# that 

# Pr[E]c = 1 - Pr[E]

# We can also define two operators on these events, called the union and
# the intersect.  The union, represented by a U between the two events,
# indicates the joining of all elements in the individual events into a 
# single event.  In our dice example, we have two sets of dice rolls 
# for scenario #2, those which are less than and equal to 7 and those 
# which are greater than and equal to 7. Lets put those possibilities in 
# vector form.

condition.1 = c(2,3,4,5,6,7)
condition.2 = c(7,8,9,10,11,12)

# R has a function for the union, although we can see that the union
# of these two events will be all possible dice rolls for a pair of dice.

union(condition.1,condition.2)

# As you can see, our dataset now contains all possible outcomes.  Numbers which are
# common between the two sets, i.e. 7, are just given once in the union.

# In addition to the union, we can also consider the intersection.  The
# intersection of two events is simply the common elements between the
# events.  It is denoted in the Venn diagram as the area of overlap between
# the two events.  There is a function in R for this as well,
# intersect.

intersect(condition.1,condition.2)

# As you can see, the only intersection between these is the 7.  Many rules
# (see Wilks) can be developed using the union and the intersect, as well
# as the complement.  

# We can also further explore the concept of conditional probability, which
# requires that an event occurs, and is defined as the probability that
# another event will occur based on the previous event occurring.  The
# event which must occur first is called the conditioning event.  Some
# examples of conditional probabilities include the probability of a
# tornado forming, given that a supercell has formed, and the probability
# of snow, given that precipitation has begun.  It can be written as:

# Pr [E1 | E2] = Pr [E1 given that E2 is or has occurred]

# In equation form, it is simply:

# Pr [E1 | E2] = Pr [E1 intersect E2] / Pr[E2]

# We can now define the concept of independence, based on this idea of
# conditional probability.  If we write the above equation for conditional
# probability in terms of the intersection term, we can write the
# Multiplicative Law of Probability:

# Pr[E1 intersect E2] = Pr[E1 | E2]Pr[E2] = Pr[E2 | E1]Pr[E1]

# Two events are said to be independent if the occurrence OR nonoccurrence
# of one event does not affect the probability of the other.  That is, we
# can just say that the Pr[E1 | E2] = Pr [E2] and vice versa for the other
# term.  

# Lets use an example from the text to help describe conditional
# probability.  We will use Ithaca temperature and precipitation data that
# is given in the back of the textbook.  We have a file in our workspace 
# that contains these data.  If you don't have it, grab it from the class
# folder and put it in your workspace folder.

ithaca.data <-matrix(scan("ithaca_data.txt"),ncol=4,byrow=T)
           
# This command tells R to scan in our data as a matrix (we can do this 
# command for a vector too, just set ncol=1) with 4 columns and by rows
# instead of columns.  What will our data look like if we don't set 
# byrow=T?

ithaca.data <-matrix(scan("ithaca_data.txt"),ncol=4)

# As you see, it defaults to putting the data in by columns, so that our
# first row is actually the first four points in the first column.  
# This doesn't seem too helpful to me, but it is a quirk with R, so 
# don't forget your byrow=T tag when scanning in data files.

ithaca.data <-matrix(scan("ithaca_data.txt"),ncol=4,byrow=T)


  # Back to probability.  We want to determine the probability of at 
  # least 0.01 inches of precipitation when the low temperature gets at 
  # or above 0 degrees Fahrenheit.  These two events should be physically 
  # related, since cloudy skies hinder temperature decreases by inhibiting 
  # radiative cooling, so warmer minimum temperatures in January should be 
  # associated with clouds and precipitation.  We can estimate this 
  # probability using a conditional relative frequency, and using a pair 
  # of if statements in R.  We need to inspect our entire dataset.

  count.warm.days <- 0
  count.snow.days <- 0

  for (i in 1:31) {
      if (ithaca.data[i,4] >= 0) {
          count.warm.days <- count.warm.days + 1
          if (ithaca.data[i,2] >= 0.01)  {
              count.snow.days <- count.snow.days + 1
          }  # End first if statement
      } #End second if statement
   } # End the for statement
  
  conditional.prob <- count.snow.warm.days / count.warm.days
  
  # We can also calculate the unconditional probability of precipitation by
  # simply counting all precipitation days and dividing by the total number
  # of days in the month.

  count.total.snow <- 0
  
  for (i in 1:31) {
      if (ithaca.data[i,2] >= 0.01) {
          count.total.snow <- count.total.snow + 1
      } # End the if statement
  } # End the for statement

unconditional.prob <- count.total.snow / 31

# One important law is the law of total probability.  We often can compute
# probabilities based only on indirect computations because limited
# information is available.  If we consider a series of mutually exclusive
# events spanning from 1 - I on a sample space of interest, we can define
# the probability of an event A based on the sum of joint probabilities
# which can be written as:

# Pr[A] = Sum ( Pr[A intersect E])

# We can substitute our Multiplicative law of probability to write this as:

# Pr[A] = Sum (Pr[A | E] * Pr[E])

# The Venn diagram is helpful to help illustrate this law.

# One final important theorem to consider is Bayes theorem.  Bayes theorem
# can be used to invert conditional probabilities (i.e. if Pr[E1 | E2] is
# known, we can use Bayes theorem to invert it and get the Pr[E2 | E1].
# Bayes theorem is written in equation form as:

# Pr [E | A] = Pr[A | E] * Pr[E] / Pr[A]

# Lets do a final example using Bayes theorem.  We already solved for the
# probability of precipitation given certain minimum temperatures.  Lets
# now see if we can get the probability of warm temperatures given
# precipitation.  If we say that E1 represents the event where minimum
# temperature is 0 degrees or above, and that E2 is E1's complement, we can
# calculate various probabilities.  We need several probabilities to solve
# for Bayes theorem.

days.colder.than.0 <- 0
days.warmer.than.0 <- 0
days.warm.wet <- 0
days.cold.wet <- 0

for (i in 1:31) {
    if (ithaca.data[i,4] >=0) {
        days.warmer.than.0 = days.warmer.than.0 + 1
        if (ithaca.data [i,2] >= 0.01) {
            days.warm.wet <- days.warm.wet + 1
        }  # End the second if statement
    } else  { # End the first if statement, begin else statement
        days.colder.than.0 <- days.colder.than.0 + 1
        if (ithaca.data [i,2] >= 0.01) {
            days.cold.wet <- days.cold.wet + 1
        } # End the first if statement
    } #End the else statement
    
} # End the for loop

# If we use the law of total probability to solve for Pr[A], we can solve
# for Pr[A]:

prob.A <-(days.warm.wet/days.warmer.than.0)*(days.warmer.than.0/31)+(days.cold.wet/days.colder.than.0)*days.colder.than.0/31

# Thus, the conditional probability that the temperature is warmer than 0
# given precipitation occurs from Bayes theorem is simply:

prob.E.given.A = ((days.warm.wet/days.warmer.than.0)*(days.warmer.than.0/31))/prob.A

# Which comes out to 14 / 15.  We can test the other concept (i.e.
# probability that temperatures are colder than zero when there is
# precipitation, we see that probaility to be:
prob.E.given.A = ((days.cold.wet/days.colder.than.0)*(days.colder.than.0/31))/prob.A

# These should sum to zero, and they do since our solution is 1/15.  Bayes
# theorem can be used for prediction and is a very powerful tool.


