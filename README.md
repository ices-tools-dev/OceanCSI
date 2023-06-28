# EEA CSI Indicators on Nutrients, Chlorophyll a and Dissolved Oxygen

## DataSource
  * 1 ICES Bottle and low resolution CTD data
  * 2 ICES high resolution CTD data
  * 3 ICES Pump data
  * 4 EMODnet Arctic Sea
  * 5 EMODnet Atlantic Sea
  * 6 EMODnet Baltic Sea
  * 7 EMODnet Black Sea
  * 8 EMODnet Mediterranean Sea
  * 9 EMODnet North Sea
  * 10 EEA WISE data

## Sea Regions / Subregions
  * Baltic Sea
    * 1 Baltic Sea
  * North East Atlantic Ocean
    * 2 Greater North Sea, incl. the Kattegat and the English Channel
    * 3 Celtic Seas
    * 4 Bay of Biscay and the Iberian Coast
    * 5 Macaronesia
  * Mediterranean Sea
    * 6 Western Mediterranean Sea
    * 7 Adriatic Sea
    * 8 Ionian Sea and the Central Mediterranean Sea
    * 9 Aegean-Levantine Sea
  * Black Sea
    * 10 Black Sea
    * 11 Sea of Marmara
    * 12 Sea of Azov

## Nutrients
  * Parameters
    * Nitrate Nitrogen [NO3-N]
    * Nitrite Nitrogen [NO2-N]
    * Ammonium Nitrogen [NH4-N]
    * Total Nitrogen [N]
    * Phosphate Phosphorus [PO4_P]
    * Total Phosphorus [P]
  * Unit
    * umol/l
  * Aggregation of dissolved nutrients
    * Depth: <= 10 m
    * Period: Winter
        * January - March for stations within Baltic Sea east of 15 E
        * January - February for all other stations
    * Arithmetric mean of mean by station and cluster per year - TwoStageAnnualMean
  * Aggregation of total nutrients
    * Depth: <= 10 m
    * Period: Annual
    * Arithmetric mean of mean by station and cluster per year - TwoStageAnnualMean

## Chlorophyll a
  * Parameters
    * Chlorophyll a
  * Units
    * ug/l
  * Aggregation
    * Depth: <= 10 m
    * Period: Summer
      * June - September for stations within Baltic Sea north of 59 N
      * May - September for all other stations
    * Arithmetric mean of mean by station and cluster per year - TwoStageAnnualMean
  
## Dissolved Oxygen
  * Parameters
    * Dissolved Oxygen
  * Units
    * ml/l
  * Aggregation
    * Depth: <= 10 m above seafloor
    * Period: July - October
    * 1-, 5- and 10 percentile by station and cluster per year
