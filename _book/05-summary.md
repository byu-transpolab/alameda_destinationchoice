# Limitations and Future Directions {#limitations}
The utility-based accessibility metrics we present and apply in this paper are 
evaluated from a discrete choice model estimated on simulate decision makers
constructed from a third-party passive origin-destination matrix. This methodological
choice has some strengths: Foremost among these is the ability to readily and
affordably construct a large dataset on an infrequent trip purpose. 
Most destination choice and activity location models are estimated on 
small-sample household travel surveys. Securing sufficient responses to estimate
a rich behavioral model on a trip purpose as infrequent as parks has proven 
prohibitively expensive outside of extensive research activities 
[e.g., @Kaczynski2016]. Using passive data sets to increase the effective 
sampling rate possible in a discrete choice model is a potentially powerful strategy,
and its application here is an important contribution of our work.

At the same time, passive data sets available from commercial providers do not
reveal any details about the specific trip makers beyond what can be learned 
from their residence block group. In this research we were able to determine 
whether a device resided in a block group with a high proportion of low-income
households, but could not have confidence that a particular device belonged 
to a member of such a household. Similarly, there is no information on what kind
of trip the device-holder actually accomplished at each park. These limitations
combined mean that it would likely be infeasible to directly observe devices
that traveled to the converted streets during the COVID-19 lockdowns. 
The ideal dataset for estimating individual park activity location choices generally
and in special situations would be a high-quality, large-sample household 
survey of real individuals. 

The individual-level demographic data would also be valuable in understanding
more clearly the observed heterogeneity in response among different income or
ethnic groups.  The trends and correlations revealed in the
presented models may reflect situational inequities rather than true
preferences. For example, the distinct observed parameters on size and distance
for block groups with high minority populations may indicate that areas with
large minority populations tend to have smaller parks that are more
geographically distributed relative to other areas of the region. This
interpretation could also explain some of the non-intuitive response observed in
our models, especially in regards to playgrounds.

We limited our analysis to home locations and parks in Alameda County,
California. It is possible that some Alameda residents visit parks in
neighboring counties, just as it is possible that parks in Alameda County
attract trips from outside the county borders. This is most likely for block
groups and parks on the north and south borders of the county. The scope of this 
analysis was determined by the passive data set available for the research, but 
the county boundaries are not a general requirement for all studies of this kind.

The distance to a park was represented in this study using a walk network retrieved
from the OpenStreetMap project. Though perhaps superior to a Euclidean distance, 
this measure still has many limitations. First, we were unable to verify the 
integrity of the underlying network information; based on our prior experience,
it is likely that some broken or improperly connected links artificially
inflated the measured distance for an unknown number of park / block group 
pairs. A more serious limitation, however, is that experienced travel distances
are a function of the transport mode employed by the traveler. Using bare 
distances does not provide any detail on how access to parks might be increased
with improved transit service, for example. Using a mode-choice model logsum
as a multi-modal impedance term in the activity location choice model would
enable this kind of analysis.

Of course, COVID-19 led to the closure of some park facilities --- playgrounds,
pavilions, and in some cases entire parks --- that were not
captured in this analysis. These closures would lead to a decrease in the consumer 
surplus for park access, which might overwhelm or at least change the distribution 
of positive benefits we measured here. 

# Conclusions {#conclusions}

Converting city roadways into pedestrian-oriented public spaces was in some
ways an obvious response to the COVID-19 pandemic: Vehicle traffic demand was
down, and there was a also critical need for pedestrian-oriented open spaces in
many communities. The research we present here suggests that this policy had
measurable and meaningful benefits to neighborhoods in Alameda County, California,
and that these benefits were distributed in an equitable or even a pro-social
manner. The total benefit to households in the community is estimated at over $660,000,
with higher-than-expected benefits going to Black, Hispanic and low-income 
individuals. There is, however, a less-than-expected benefit experienced 
Asian individuals that might be addressed were the policy to continue, be repeated,
or made permanent in some way.

In estimating these benefits, we applied an emerging technique to estimate park
choice preferences and utility from passive mobile device data. This technique
allowed a more nuanced measure of access that allowed us to consider the
converted streets as providing quantitatively different access than other city
parks.  A policy of permanently closing these streets to vehicle traffic would
have potentially deleterious effects on transportation access that would need to
be considered against the benefits we measured in this research. But
utility-based access measures provide a mechanism to weigh the benefits of
access against the costs of travel in a theoretically coherent manner. Adopting
such flexible methods of measuring access will help transportation and land use 
planners better understand the nunces and tradeoffs inherent in a wide range of 
policy proposals.



# Acknowledgments {-}

Graphics and tables were developed using several open-source R packages 
[@ggmap; @modelsummary;@wesanderson].
