# EffVisR

Efficacy Data Visualizer

- App developed with Data folder containing sqlite database
- use api to read data, and allow effRviz to plot
- get all studies from sqlite3 db and transfer to oracle 
- efficacy data: tumour volume, weights

#### SQL select statement to get row count
```
select count(*) nrows 
from perk_msd t1
union all 
select count(*) nrows 
from pk_efficacy t2
union all 
select count(*) nrows 
from pk_pd t3
union all 
select count(*) nrows
from pkparams_pd t4
union all 
select count(*) nrows
from perk_wb t5
union all 
select count(*) nrows
from pk_efficacy_2 t6
union all 
select count(*) nrows 
from pkparams_efficacy t7
union all 
select count(*) nrows 
from tvbw t7
```

### sqlite db info

| TABLE_NAME  | ROW_COUNT  |
|---|---|
| perk_msd  | 669|
| perk_wb  | 1323|
| pk_efficacy  | 4025 |
| pk_efficacy_2  | 6540|
| pk_pd  |2814 |
| pkparams_pd  | 963|
| pkparams_efficacy  |1517 |
| tvbw  | 40489|


##### perk_msd

```
0|Quotation No|TEXT|0||0
1|Plate|TEXT|0||0
2|Group|TEXT|0||0
3|Group Number|REAL|0||0
4|Treatment|TEXT|0||0
5|Duration|TEXT|0||0
6|Hours Post Dose|REAL|0||0
7|Drug|TEXT|0||0
8|Dose|TEXT|0||0
9|Route|TEXT|0||0
10|Regimen|TEXT|0||0
11|Animal ID|TEXT|0||0
12|pERK_n1|REAL|0||0
13|pERK_n2|REAL|0||0
14|ERK_n1|REAL|0||0
15|ERK_n2|REAL|0||0
16|pERK/ERK_n1|REAL|0||0
17|pERK/ERK_n2|REAL|0||0
18|pERK/ERK_avg|REAL|0||0
19|%Veh_n1|REAL|0||0
20|%Veh_n2|REAL|0||0
21|%Veh_avg|REAL|0||0
```

|Quotation No|Plate|Group|Group Number|Treatment|Duration|Hours Post Dose|Drug|Dose|Route|Regimen|Animal ID|pERK_n1|pERK_n2|ERK_n1|ERK_n2|pERK/ERK_n1|pERK/ERK_n2|pERK/ERK_avg|%Veh_n1|%Veh_n2|%Veh_avg|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN03-IVP-2044|Plate1|G10|10.0|Vehicle control 3%DMSO/10 % HS-15 and 87% D5W PO BIDx1 day|1h|1.0|Vehicle control|3%DMSO/10 % HS-15 and 87% D5W|PO|BIDx1 day|37|5693.0|5148.0|317043.0|291949.0|0.017956554789098|0.0176332167604616|0.0177948857747798|94.1825542358986|92.4866386342561|93.3345964350773|


####  perk_wb

```
0|Quotation No|TEXT|0||0
1|Gel|TEXT|0||0
2|Group|TEXT|0||0
3|Drug|TEXT|0||0
4|Dose|TEXT|0||0
5|Collection Timepoint|TEXT|0||0
6|Animal No.|TEXT|0||0
7|ERK|REAL|0||0
8|p-ERK|REAL|0||0
9|p-ERK/ERK|REAL|0||0
10|Hours Post Dose|REAL|0||0
11|Group Number|REAL|0||0

```

|Quotation No |Gel |Group |Drug |Dose |Collection Timepoint |Animal No. |ERK |p-ERK |p-ERK/ERK |Hours Post Dose |Group Number|
|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2030|Gel1|G8|-|PO Single dose|1h|2|14200.0|1550.0|0.109154929577465|1.0|8.0|


#### pk_efficacy

```
0|Quotation No|TEXT|0||0
1|Sample Name|TEXT|0||0
2|Group|TEXT|0||0
3|Animal ID|TEXT|0||0
4|Hours Post Dose|REAL|0||0
5|Sample ID|TEXT|0||0
6|Sample Type|TEXT|0||0
7|Compound|TEXT|0||0
8|Dilution Factor|TEXT|0||0
9|Analyte Retention Time (min)|TEXT|0||0
10|Analyte Peak Area (counts)|TEXT|0||0
11|IS Retention Time (min)|TEXT|0||0
12|IS Peak Area (counts)|TEXT|0||0
13|Analyte Concentration (ng/mL)|TEXT|0||0
14|Calculated Concentration (ng/mL)|REAL|0||0
15|Accuracy (%)|TEXT|0||0
16|Use Record|TEXT|0||0
```


|Quotation No |Sample Name |Group |Animal ID |Hours Post Dose |Sample ID |Sample Type |Compound |Dilution Factor |Analyte Retention Time (min) |Analyte Peak Area (counts) |IS Retention Time (min) |IS Peak Area (counts) |Analyte Concentration (ng/mL) |Calculated Concentration (ng/mL) |Accuracy (%) |Use Record|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2030|PO_G2_7_24h|G2|7|24.0||Unknown|FT002787-12|1|1.22|36100|1.26|2230000|N/A|20.9|N/A|


#### pk_efficacy_2

```
0|Quotation No|TEXT|0||0
1|Group|TEXT|0||0
2|Animal ID|TEXT|0||0
3|Compound|TEXT|0||0
4|Hours|REAL|0||0
5|ng/mL|REAL|0||0
6|BLOQ|INTEGER|0||0
7|Batch ID|TEXT|0||0
8|Study Type|TEXT|0||0
9|Sex|TEXT|0||0
10|Strain|TEXT|0||0
11|Species|TEXT|0||0
12|Dose|REAL|0||0
13|Dose_Unit|TEXT|0||0
14|Dose Frequency|TEXT|0||0
15|Administration|TEXT|0||0
16|Mixture|TEXT|0||0
17|Formulation|TEXT|0||0
18|Method|TEXT|0||0
```

|Quotation No |Group |Animal ID |Compound |Hours |ng/mL |BLOQ |Batch ID |Study Type |Sex |Strain |Species |Dose |Dose_Unit |Dose Frequency |Administration |Mixture |Formulation |Method|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2030|G2|70|FT002787-12|1.0|3330.0|||||||||||||

#### pk_pd

```
0|Quotation No|TEXT|0||0
1|Sample Name|TEXT|0||0
2|Group|TEXT|0||0
3|Animal ID|TEXT|0||0
4|Hours Post Dose|REAL|0||0
5|Sample ID|TEXT|0||0
6|Sample Type|TEXT|0||0
7|Compound|TEXT|0||0
8|Dilution Factor|TEXT|0||0
9|Analyte Retention Time (min)|TEXT|0||0
10|Analyte Peak Area (counts)|TEXT|0||0
11|IS Retention Time (min)|TEXT|0||0
12|IS Peak Area (counts)|TEXT|0||0
13|Analyte Concentration (ng/mL)|TEXT|0||0
14|Calculated Concentration (ng/mL)|REAL|0||0
15|Accuracy (%)|TEXT|0||0
16|Use Record|TEXT|0||0
```

|Quotation No |Sample Name |Group |Animal ID |Hours Post Dose |Sample ID |Sample Type |Compound |Dilution Factor |Analyte Retention Time (min) |Analyte Peak Area (counts) |IS Retention Time (min) |IS Peak Area (counts) |Analyte Concentration (ng/mL) |Calculated Concentration (ng/mL) |Accuracy (%) |Use Record|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2029|PO_G8_117_48h|G8|117|48.0||Unknown|FT002787-04|1|1.29|3620|1.27|2360000|N/A|1.95|N/A|


#### pkparams_pd


```
0|Quotation No|TEXT|0||0
1|Compound ID|TEXT|0||0
2|Batch ID|TEXT|0||0
3|Fount Project Code|TEXT|0||0
4|Strain|TEXT|0||0
5|Species|TEXT|0||0
6|Sex|TEXT|0||0
7|Study Type|TEXT|0||0
8|Group|TEXT|0||0
9|Administration|TEXT|0||0
10|Dose|REAL|0||0
11|Dose_Unit|TEXT|0||0
12|Dose Frequency|TEXT|0||0
13|Mixture|TEXT|0||0
14|Formulation|TEXT|0||0
15|Method|TEXT|0||0
16|Result Type|TEXT|0||0
17|Unit|TEXT|0||0
18|Result|REAL|0||0
19|Comments|INTEGER|0||0

```

|Quotation No |Compound ID |Batch ID |Fount Project Code |Strain |Species |Sex |Study Type |Group |Administration |Dose |Dose_Unit |Dose Frequency |Mixture |Formulation |Method |Result Type |Unit |Result |Comments|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2030|FT002787|FT002787-12|KIN-04|Balb/c nude|Mouse|Female|PK/PD|G9|PO|10.0|mg/kg|QD|Suspension|0.5%MC/0.1%Tween80|Time course|T1/2|h|2.16403505636631|


#### pkparams_efficacy


```
0|Quotation No|TEXT|0||0
1|Compound ID|TEXT|0||0
2|Batch ID|TEXT|0||0
3|Fount Project Code|TEXT|0||0
4|Strain|TEXT|0||0
5|Species|TEXT|0||0
6|Sex|TEXT|0||0
7|Study Type|TEXT|0||0
8|Group|TEXT|0||0
9|Administration|TEXT|0||0
10|Dose|REAL|0||0
11|Dose_Unit|TEXT|0||0
12|Dose Frequency|TEXT|0||0
13|Mixture|TEXT|0||0
14|Formulation|TEXT|0||0
15|Method|TEXT|0||0
16|Result Type|TEXT|0||0
17|Unit|TEXT|0||0
18|Result|REAL|0||0
19|Comments|INTEGER|0||0

```

|Quotation No |Compound ID |Batch ID |Fount Project Code |Strain |Species |Sex |Study Type |Group |Administration |Dose |Dose_Unit |Dose Frequency |Mixture |Formulation |Method |Result Type |Unit |Result |Comments|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2030|FT002787|FT002787-12|KIN-04|Balb/c nude|Mouse|Female|Efficacy|G2|PO|10.0|mg/kg|QD*14|Suspension|0.5%MC/0.1%Tween80|Time course|T1/2|h|3.118298838554|


#### tvbw

```
0|Quotation No|TEXT|0||0
1|Study ID|TEXT|0||0
2|Tumor Cell Line|TEXT|0||0
3|Group|TEXT|0||0
4|Treatment|TEXT|0||0
5|Animal ID|TEXT|0||0
6|BW (g)|REAL|0||0
7|L (mm)|REAL|0||0
8|W (mm)|REAL|0||0
9|TV (mm3)|REAL|0||0
10|Comment|TEXT|0||0
11|Date|REAL|0||0
12|Days on Study Treatment|REAL|0||0
13|Days from Tumor Inoculation|REAL|0||0
14|BW % Change|REAL|0||0
15|TV % Change|REAL|0||0
16|Objectives|TEXT|0||0
17|Treatment_1|TEXT|0||0
18|Dose_1|TEXT|0||0
19|Volume_1|TEXT|0||0
20|Route_1|TEXT|0||0
21|Frequency_1|TEXT|0||0
22|Duration_1|REAL|0||0
23|Formulation_1|TEXT|0||0
24|Treatment_2|TEXT|0||0
25|Dose_2|TEXT|0||0
26|Volume_2|TEXT|0||0
27|Route_2|TEXT|0||0
28|Frequency_2|TEXT|0||0
29|Duration_2|REAL|0||0
30|Formulation_2|TEXT|0||0
31|Injected Cell Numbers|TEXT|0||0
32|Animal|TEXT|0||0
33|Injected / Implanted position|TEXT|0||0
34|Inoculation Date|REAL|0||0
35|Dosing Date|REAL|0||0
36|Invivo Study Ending Date|REAL|0||0
```

|Quotation No| Study ID| Tumor Cell Line| Group| Treatment| Animal ID| BW (g)| L (mm)| W (mm)| TV (mm3)| Comment| Date| Days on Study Treatment| Days from Tumor Inoculation| BW % Change| TV % Change| Objectives| Treatment_1| Dose_1| Volume_1| Route_1| Frequency_1| Duration_1| Formulation_1| Treatment_2| Dose_2| Volume_2| Route_2| Frequency_2| Duration_2| Formulation_2| Injected Cell Numbers| Animal| Injected / Implanted position| Inoculation Date| Dosing Date| Invivo Study Ending Date|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
|PH-KIN04-IVP-2030|PH-ON-KIN-BxPC-3-06132020|BxPC-3|G1|Vehicle_1 & Vehicle_2|20|22.8|10.4|7.79|315.55732||18486.0|1.0|24.0|0.0|0.0|Efficacy Safety PK|Vehicle_1||5 uL/g|PO|QD|14.0|0.5%MC/0.1 % Tween80|Vehicle_2||5 uL/g|PO|QD|14.0|1%MC/0.5 % Tween80|5x10^6 in 100ul of 1:1 medium/Matrigel|Balb/c nude♀ 6-8w AKYB|Subcutaneous|18463.0|18486.0|18500.0|

#### mapping

- `Quotation No.` == `study_id` from `FT_PHARM_STUDY`
- `Inoculation Date` == `DATE_COMPOUND_RECEIVED`
- `Dosing date` == `DATE_IN_LIFE_INITIATED` 
- `Invivo Study Ending Date` == `DATE_REPORTED`
