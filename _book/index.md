---
title: "If you build it who will come? Equity analysis of park system changes using passive origin-destination data"
author:
  - name: Gregory Macfarlane
    email: gregmacfarlane@byu.edu
    affiliation: BYU
    footnote: 1
  - name: Teresa Tapia
    email: teresa.tapia@streetlightdata.com
    affiliation: StreetLight
  - name: Carole Turley-Voulgaris
    email: cvoulgaris@gsd.harvard.edu
    affiliation: Harvard
address:
  - code: BYU
    address: Brigham Young University, Civil and Environmental Engineering Department, 430 Engineering Building, Provo, Utah 84602
  - code:  Harvard
    address: Harvard Graduate School of Design, 48 Quincy St, Cambridge, Massachussetts 02138
  - code: StreetLight
    address: StreetLight Data, Inc., San Francisco, California
footnote:
  - code: 1
    text: "Corresponding Author"
date: "2021-04-15"
site: bookdown::bookdown_site
documentclass: article
journal: "Journal of Transportation and Land Use"
bibliography: [book.bib]
layout: "3p, authoryear, review"
keywords:
  - Accessibility
  - Passive Data
  - Location Choice
  - Parks
abstract: |
 During the spring and summer of 2020, many cities across the world adopted a policy of converting roadway facilities into open pedestrian spaces. This policy was designed to meaningfully improve access to public recreation areas, but the degree to which this access improved among different populations is an important research question. In particular, academic and practical methods to measure socio-spatial access to park facilites rely on arbitrary definitions and are not based in revealed preferences for park facilities. In this study, we evaluate the change in a utility-based park accessibility measure resulting from street conversions in Alameda County, California. Our utility-based accessibility measure is constructed from a park activity location choice model we estimate using mobile device data --- supplied by StreetLight Data, Inc. --- representing trips to parks in that county. The estimated model reveals heterogeneity in inferred affinity for park attributes among different sociodemographic groups. When applied to the street conversion policy in Alameda County, the model suggests an aggregate household-level consumer surplus of over \$600 thousand. This surplus is pro-social in that Black, Hispanic, and low-income households receive a disproportionate share of the surplus, relative to the population distribution. 
description: "Alameda Park Choice"
---

# Introduction {#intro}
Parks and other open spaces generate immense value for the public who are able to access them. The @CityParksAlliance categorizes the observed benefits of urban parks as encouraging active lifestyles [@Bancroft2015], contributing to local economies, aiding in stormwater management and flood mitigation, improving local air quality, increasing community engagement [@Madzia2018], and enhancing public equity.

For many, the value of public parks and open public spaces increased during the widespread lockdowns enacted in 2020 to slow the transmission of COVID-19. With other entertainment venues shuttered and people otherwise confined to their homes, periodic use of public space provided an opportunity physical and emotional relief unavailable in other forms. Paired with this increased demand for public open space --- and the with the epidemiological requirement to leave sufficient space between other users --- was the related collapse in demand for vehicular travel. As a result, cities around the world began closing select streets to automobile travel, thereby opening them as pedestrian plazas, open streets, or slow streets [@glaser_can_2021; @schlossberg_rethinking_2021; @combs2021shifting]. The effective result of this policy was to create a number of "parks" in urban areas that may have had poor access previously. Understanding the equitable distribution of these benefits is an important land use policy issue. The potential for non-emergency temporary or permanent street conversions also brings up interesting problems for land use and transportation policy; indeed, the possibility for transportation infrastructure to *become* a socially beneficial land use that goes beyond serving mobility needs is a tantalizing proposition.

Unfortunately, quantifying the benefits derived from access to parks in general is a complicated problem. Many previous attempts at quantifying access in terms of isochronal distances or open space concentration have resulted in a frustrating lack of clarity on the relationship between measured access and measures of physical and emotional health [@Bancroft2015]. Central to this confusion is the fact that people do not always use the nearest park, especially if it does not have qualities that they find attractive. A better methodology would be to evaluate the park activity location choices of people in a metropolitan area to identify which features of parks --- distance, amenities, size, etc. --- are valued and which are less valued. The resulting activity location choice model would enable the evaluation of utility benefits via the choice model logsum [@Handy1997; @DeJong2007].

In this study, we seek to evaluate the socio-spatial distribution of benefits received by residents of Alameda County, California resulting from the  temporary conversion of streets to public open spaces during the spring and summer of 2020. We estimate a park activity location choice model using location-based services (LBS) data obtained through StreetLight Data, Inc., a commercial data  aggregator. The resulting model illuminates the degree to which constructed individuals living in U.S. Census block groups of varying sociodemographic characteristics value the travel distance between block group and parks, the size of the parks, and the amenities of parks including sport fields, playgrounds, and walking trails in Alameda county. We then apply this model to examine the inferred monetary benefit resulting from the street conversion policy, and its distribution among different sociodemographic groups.

The paper proceeds in the following manner: A [discussion](#literature) of prior attempts to evaluate park accessibility and preferences is given directly. A [Methodology](#methodology) section presents our data gathering and cleaning efforts as well as the econometric framework for the location choice model. A [Results](#results) section presents the estimated choice model coefficients alongside a discussion of their implications, followed by an analysis of the implied benefits resulting from the street conversion policy. After presenting [limitations](#limitations) and associated avenues for future research, a final [Conclusions](#conclusions) section outlines the contributions of this study for recreational trip modeling and location choice modeling more generally.
