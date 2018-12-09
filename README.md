# Modeling the Effect of U.S. Arms Transfers on FDI

This projects provides tests the relationship between FDI and international security arrangements: Are U.S. arms transfers, like the presence of the U.S. military itself, a locational advantage to investors?

To test this theory, I assemble a data set of FDI flows from the U.S., military sales from the U.S., and supplementary variables likely to also affect FDI flows. I run numerous kinds of regressions, in search of the best model of this phenomenon.

I tested a number of models and found that mixed effect models best capture the data set. The final model $M_5$ performed the best, explaining 71 percent of variation in FDI on the test dataset. The model confirmed the relationship between U.S. arms sales and U.S. FDI. The more arms a country recieves from the U.S. in year $t$, the more direct foreign investment the country will recieve from the U.S. the next year $t+1$. This relationship is statistically significant, even when controlling for other well-known factors influencing FDI. 

See the final report PDF in the report directory.

## Inspiration

Biglaiser and DeRouen, 'Economics and Security: The Interdependence of US Troops and Trade in the Developing World', https://www.researchgate.net/profile/Glen_Biglaiser/publication/228431006_Economics_and_Security_The_Interdependence_of_US_Troops_and_Trade_in_the_Developing_World/links/00b4952e28b158f0fb000000.pdf, also published Following the Flag: Troop Deployment and U.S. Forieng Direct Investment, International Studies Quartlery 2007: https://www.jstor.org/stable/4621745

## Data sources

FDI: https://stats.oecd.org/index.aspx?DataSetCode=FDI_FLOW_PARTNER (2003-2013)

Arms exports: https://www.sipri.org/databases/armstransfers

Population: https://population.un.org/wpp/Download/Standard/Population/

Conflict: http://ucdp.uu.se/downloads/#d3

Regime type: Polity2 variable, http://www.systemicpeace.org/inscrdata.html,
user manual: http://www.systemicpeace.org/inscr/p4manualv2017.pdf

Formal alliances: Alliance by Dyad yearly http://www.correlatesofwar.org/data-sets/formal-alliances

