#Here are some examples of regular expressions

# Match the word cat or at or t.
# Don't match cat embedded within another word
# There can be other words present
cats = c("diplocat", "Hi cat", "mat", "at", "t!", "ct")
# The \< stands for beginning of a word and
# \> stands for the end of a word
# In R we have to escape the \ with an extra \
grep("\\<(cat|at|t)\\>", cats)
grep("\\<(ca|a)?t\\>", cats)

# The following do not work as expected
# can you figure out why?
grep("\\<c?a?t)\\>", cats)
grep("^(cat|at|t)$", cats)

# Find the word cat or caat or caaat, etc.
caats = c("cat", "caat.", "caats", "caaaat", "my cat")
grep("\\<ca+t\\>", caats)
# the {1,} is equivalent to +
grep("\\<ca{1,}t\\>", caats)

# Now we want to find dog anywhere in the string
# We don't care about capitals
dogs = c("dogmatic", "TopDog","Doggone it!", "RUN DOG RUN")
# The tolower function is handy here.
grep("dog", tolower(dogs))
grep("[Dd][Oo][Gg]", dogs)

# Finally we are looking at character vectors where
# each entry must be a number.
# The number can have an optional sign in front of it
# The number can have an optional decimal point followed by digits
nums = c("1.2", "-3000", "5lo", "hi2", "12.", "+57")
gregexpr("^[-+]?[[:digit:]]+(\\.[[:digit:]]+)?$", nums)

