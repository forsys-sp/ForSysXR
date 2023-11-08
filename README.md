# ForSysXR

<!--- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/forsys_consortium_logo.png" align="right" style="height:90px!important; position:absolute; top:10px; right:10px" />

## Scenario planning for land management

ForSys is a land management planning model that explores potential outcomes
across many possible priorities, including but not limited to landscape
restoration and hazardous fuel management. The model is spatially
explicit and uses multi-criteria prioritization and optimization methods
that can rapidly process scenarios, from small to very large-scales
(\~10,000 acres to more than 180 million acres). The previous iteration
of the ForSys program was called the Landscape Treatment Designer (LTD),
and was used in several published studies. ForSys has been used in
several research and applied case studies at a range of scales
(projects, forests, states, continental United States) to prioritize
landscape-scale treatments (see case studies). ForSys is available in a
windows desktop GUI (ForSysX) and an in a modified R version (ForSysR).

For details on ForSysX and [ForSysR](https://github.com/forsys-sp/forsysr) algorithms, please refer to Day et al. (2023) <!-- INSERT LINK WHEN PUBLIC -->

## Installation

The current official version of the *forsys* package can be installed
from [GitHub](https://github.com/forsys-sp/forsysr/) using the following
code. 

We recommend updating other packages when prompted.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("forsys-sp/forsysr")
remotes::install_github("forsys-sp/patchmax")
```

After installing ForSysXR package, the user needs to download
and unzip the ForSysXDLL from
[here](https://github.com/bmaparicio/ForSysXR/raw/main/ForSysXDLLDist.zip).
This step is crucial as it downloads ForSysX executable that will later
be run from R.

## Usage

Below we demonstrate how the *ForSysXR* package can be used and highlight the flexibility of the algorithm in solving multiple problems.
For brevity, the dataset used is distributed with the package. First, we will
load the *forsys* package.

``` r
library(ForSysXR)

# In order to run the examples below, these additional libraries are required:
library(sf)
library(dplyr)
```

### Loading data

Although *forsys* can support many different types of treatment unit
data, here our treatment units are represented as polygons in a spatial
vector format. Each polygon represents a different treatment unit. 
Please note that geodatabase format is not supported by *ForSysX*. Only shapefile format is currently supported

``` r
# load treatment unit data
data(stands_data)
# show the first rows in the attribute table
head(stands_data)
```

    ## Simple feature collection with 6 features and 10 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 571829.6 ymin: 4449945 xmax: 577284.2 ymax: 4455392
    ## Projected CRS: ETRS89 / UTM zone 29N
    ##   Stand_ID  Area_ha  X_Coord Y_Coord availuse water     obj_1    obj_2
    ## 1        1 2.149784 577104.1 4455304        1     0  649.1051 2.632802
    ## 2        2 1.307183 571959.4 4450037        1     0 1092.8525 2.421031
    ## 3        3 1.313889 572198.1 4450204        1     0 1072.2005 2.397908
    ## 4        4 1.283715 576908.8 4451242        1     0  252.8004 2.378386
    ## 5        5 1.393932 577108.5 4451299        1     0  269.7420 2.388572
    ## 6        6 1.077390 574488.3 4452205        1     0  146.1724 2.339832
    ##   obj_3        threshold                  geometry
    ## 1 0.0017115583  1.750002 POLYGON ((577284.2 4455239,...
    ## 2 0.0004543819  1.627590 POLYGON ((571986 4450002, 5...
    ## 3 0.0008986479  1.996994 POLYGON ((572168.8 4450285,...
    ## 4 0.0037384391  2.315409 POLYGON ((576948.2 4451149,...
    ## 5 0.0038692370  2.815383 POLYGON ((577192.4 4451290,...
    ## 6 0.0020394307  1.976931 POLYGON ((574485.3 4452268,...

``` r
# plot the treatment units
plot(stands_data[,1])
```

<img src="man/figures/fig_1.png" width="300" />

The figure plotted shows the stands in the study area. It is composed of 1028 different stands.

It is also useful to plot the objectives, available stands, and stands that should be excluded.

``` r
# plot the objectives, availability and excluded stands
plot_1 <- stands_data  %>%
  mutate_at(c('obj_1'), ~na_if(., 0)) %>%
  ggplot() +
  geom_sf(aes(fill=obj_1),colour=NA) +
  scale_fill_viridis_c(option = "A",na.value = "grey50")+
  ggtitle("Objective 1") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))



plot_2 <- stands_data  %>%
  mutate_at(c('obj_2'), ~na_if(., 0)) %>%
  ggplot() +
  geom_sf(aes(fill=obj_2),colour=NA) +
  scale_fill_viridis_c(option = "A",na.value = "grey50")+
  ggtitle("Objective 2") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))



plot_3 <- stands_data  %>%
  mutate_at(c('obj_3'), ~na_if(., 0)) %>%
  ggplot() +
  geom_sf(aes(fill=obj_3),colour=NA) +
  scale_fill_viridis_c(option = "A",na.value = "grey50")+
  ggtitle("Objective 3") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))


plot_4 <- stands_data  %>%
  ggplot() +
  geom_sf(aes(fill=factor(availuse)),colour=NA) +
  ggtitle("Availability") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))+
  guides(fill=guide_legend(title="Availab"))


plot_5 <- stands_data  %>%
  ggplot() +
  geom_sf(aes(fill=factor(water)),colour=NA) +
  ggtitle("Exclude") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))+
  guides(fill=guide_legend(title="Exclude"))


ggarrange(plot_1,plot_2,plot_3,plot_4,plot_5,nrow=3,ncol=2)

```
<img src="man/figures/fig_2.png" width="600" />

### Running a ForSys Scenario

*Forsys* prioritizes projects by maximizing an objective given one or
more constraints. The objectives represent one or more management
priorities, while a constraint can be perceived as the condition required to stop the prioritization process. Common constraints are total area treated and/or total cost.
Thresholds are environmental or categorical conditions that
trigger the need to treat an individual treatment unit or stand (e.g., 
particular ownership or minimum forest cover). *Forsys* then builds
projects and ranks them in order of their priority. Projects can be
either predefined units (e.g., watersheds) or can be built dynamically.

The example below sets a simple *ForSysX* run. It uses the stands_data shown above to delineate the top 50 ha
within each predefined project based on ‘priority1’. This run defines a total of 10 projects of 50 ha each.

``` r
set_forsysx_run (input_shapefile = stands_data,
                 outputs_base_name = "C:/Users/ForSysXR/run_tutorial_1",
                 stand_id = "Stand_ID",
                 area = "Area_ha",
                 available = "availuse",
                 exclude_field = "water",
                 seed_stands_only_available_stands = 1,
                 x_coordinate = "Point_X",
                 y_coordinate = "Point_Y",
                 max_number_projects = 10,
                 output_adjacency_matrix ="C:/Users/ForSysXR",
                 constraints_name = "Area_ha",
                 constraints_value = "50.00",
                 constraints_slack = "1.00",
                 effect_fields = c("obj_1","obj_2","obj_3"),
                 objectives = c("obj_1","Treat","1","1","1"),
                 output_xml = "C:/Users/ForSysXR/tutorial_objective1.xml",
                 run_forsysx = 1,
                 plot_results=TRUE,
                 exe_path = "C:/Users/ForSysXR/ForSysXConsole.exe",
                 save_outputs = c("stand_csv","shapefile","image"))
```

<img src="man/figures/run_tutorial_1_1_49-50_inR.jpg" width="300" align="center"/>


Not surprisingly, the treatment rank of the projects selected
corresponds directly to those areas where obj_1 was highest, as
plotted above. Project rank \#1 (darkest red) is the highest-ranked
project.


The amount of objective targeted for treatment per project can be plotted as follows


``` r
result_table <- read.csv("C:/Users/ForSysXR/run_tutorial_1_Results.csv")

ggplot(result_table,aes(x=ProjectNumber,y=ETrt_obj_1))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)+
  xlab("Objective attainment")+
  ylab("Project number")+
  theme_classic()
```

<img src="man/figures/attainment_run1.jpg" width="300" align="center"/>


### Using threshold 

It is possible to define a treatment threshold in ForSys. For instance, a given stand may be available for treatment, but if it does not contain a certain value of biomass or any fire metric, it may be not targeted for treatment. Here, the field threshold represents the predicted flame length in meters. As an example, one could limit the treatments to be allocated only in areas with predicted flame lengths greater than 2 meters.


We run *forsys* with the following arguments. Note the use of adjacency_matrix (uses the adjacency created above instead of generating a new file) and the parameter threshold.

``` r
set_forsysx_run (input_shapefile = stands_data,
                 outputs_base_name = "C:/Users/ForSysXR/run_tutorial_threshold_2",
                 stand_id = "Stand_ID",
                 area = "Area_ha",
                 available = "availuse",
                 exclude_field = "water",
                 seed_stands_only_available_stands = 1,
                 x_coordinate = "X_Coord",
                 y_coordinate = "Y_Coord",
                 max_number_projects = 10,
                 adjacency_matrix ="C:/Users/ForSysXR/adjacency_matrix_forsys.csv",
                 constraints_name = "Area_ha",
                 constraints_value = "50.00",
                 constraints_slack = "1.00",
                 effect_fields = c("obj_1","obj_2","obj_3"),
                 objectives = c("obj_1","Treat","1","1","1"),
                 output_xml = "C:/Users/ForSysXR/test_tutorial_vs3_threshold.xml",
                 run_forsysx = 1,
                 plot_results=TRUE,
                 exe_path = "C:/Users/ForSysXR/ForSysXConsole.exe",
                 threshold=c("threshold",">",2),
                 save_outputs = c("stand_csv","shapefile","image"))
```

Not surprisingly, the treatment rank of the projects selected
corresponds directly to those areas where priority1 was highest, as
plotted below. Projeck rank \#1 (darkest blue) is the highest ranked
project.

``` r
plot_dat <- test_forest %>%
  group_by(proj_id) %>% summarize() %>%
  left_join(run_outputs$project_output %>% select(proj_id, treatment_rank))
plot(plot_dat[,'treatment_rank'], main="Project rank")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Below we plot the stands rather than the project rank and only retain
those stands that were treated.

``` r
plot_dat_2 <- test_forest %>% select(stand_id) %>%
  left_join(run_outputs$stand_output %>% mutate(stand_id = as.integer(stand_id))) %>%
  select(stand_id, priority1, proj_id) %>%
  left_join(run_outputs$project_output %>% select(proj_id, treatment_rank))
plot(plot_dat_2[,c('treatment_rank','priority1')], border=NA)
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Multiple priorities

In many prioritization studies, more than one objective is often considered.  When this is the case, it is advised that the real values are not used directly in ForSysX as the magnitude of the objectives' values are often different. Two clear examples of that are objective 1 (ranges from 0 to 2569) and objective 3 (ranges from 0 to 0.0848). If these values are used directly in ForSys, the priority areas would be defined based on objective 1 instead of on both.

ForSysX and ForSysXR allow the user to normalize the objectives using the PCP (percentage contribution concerning the total problem of all treatable units) and SPM (percentage difference from the maximum value of the objective). Hence, the sum of the PCP of all stands is 100 and the maximum SPM value of any stand is 100. Usually, the SPM is used as an objective and the PCP as an effect.

To normalize the objectives, the function normalize_objectives can be used

``` r
normalize_objectives(stands_data, fields=c("obj_1","obj_2","obj_3"), availability_txt="availuse",output_name="C:/Users/ForSysXR")

## Simple feature collection with 1028 features and 16 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 571404.6 ymin: 4448921 xmax: 579181 ymax: 4456309
## Projected CRS: ETRS89 / UTM zone 29N
## First 10 features:
##   Stand_ID  Area_ha  X_Coord Y_Coord availuse water     obj_1    obj_2        obj_3 threshold
##1         1 2.149784 577104.1 4455304        1     0  649.1051 2.632802 0.0017115583  1.750002
##2         2 1.307183 571959.4 4450037        1     0 1092.8525 2.421031 0.0004543819  1.627590
##3         3 1.313889 572198.1 4450204        1     0 1072.2005 2.397908 0.0008986479  1.996994
##4         4 1.283715 576908.8 4451242        1     0  252.8004 2.378386 0.0037384391  2.315409
##5         5 1.393932 577108.5 4451299        1     0  269.7420 2.388572 0.0038692370  2.815383
##6         6 1.077390 574488.3 4452205        1     0  146.1724 2.339832 0.0020394307  1.976931
##                         geometry obj_1_SPM obj_2_SPM obj_3_SPM  obj_1_PCP obj_2_PCP   obj_3_PCP
##1  POLYGON ((577284.2 4455239,... 25.264164  91.44424  6.847841 0.15205560 0.1149056 0.029964043
##2  POLYGON ((571986 4450002, 5... 42.535493  84.08887  1.817954 0.25600530 0.1056631 0.007954809
##3  POLYGON ((572168.8 4450285,... 41.731688  83.28573  3.595436 0.25116750 0.1046539 0.015732519
##4  POLYGON ((576948.2 4451149,...  9.839378  82.60767 14.957268 0.05921955 0.1038019 0.065448396
##5  POLYGON ((577192.4 4451290,... 10.498772  82.96146 15.480582 0.06318820 0.1042465 0.067738260
##6  POLYGON ((574485.3 4452268,...  5.689255  81.26859  8.159638 0.03424151 0.1021192 0.035704064
```

After running the normalize_objective function, a shapefile named *forsysXR_stands_normalized* is stored in the output_name specified.
This shapefile will be used to run ForSysX with multiobjectives. In the example, all three objectives will be used.


``` r
set_forsysx_run (input_shapefile = "C:/Users/ForSysXR/forsysXR_stands_normalized.shp",
                 outputs_base_name = "C:/Users/ForSysXR/run_tutorial_multiobjectives",
                 stand_id = "Stand_ID",
                 area = "Area_ha",
                 available = "availuse",
                 exclude_field = "water",
                 seed_stands_only_available_stands = 1,
                 x_coordinate = "X_Coord",
                 y_coordinate = "Y_Coord",
                 max_number_projects = 10,
                 adjacency_matrix = "C:/Users/ForSysXR/adjacency_matrix_forsys.csv",
                 constraints_name = "Area_ha",
                 constraints_value = "50",
                 constraints_slack = "1.00",
                 effect_fields = c("obj_1_PCP","obj_2_PCP","obj_3_PCP"),
                 objectives = c("obj_1_SPM","Treat","1","1","1",
                                "obj_2_SPM","Treat","1","1","1",
                                "obj_3_SPM","Treat","1","1","1"),
                 output_xml = "C:/Users/ForSysXR/test_tutorial_multiobjectives.xml",
                 run_forsysx = 1,
                 plot_results=TRUE,
                 exe_path = "C:/Users/ForSysXR/ForSysXConsole.exe",
                 save_outputs = c("stand_csv","shapefile","image"))

``` 

<img src="man/figures/run_tutorial_multicriteria.jpg" width="300" align="center"/>

One can plot the attainment for each objective and the overall attainment as follows 

``` r
result_table <- read.csv("C:/Users/aparicio/Desktop/ForSysXR/run_tutorial_multiobjectives_300_Results.csv")
head(result_table)

#area_treated <- result_table[,"Treat_Area_ha"]
obj1_PCP_treated <- result_table[,c("ProjectNumber","Treat_Area_ha","ETrt_obj_1_PCP")]
obj1_PCP_treated$objective <- 1
colnames(obj1_PCP_treated)<- c("ProjectNumber","Treat_Area_ha","PCP_treated","objective")

obj2_PCP_treated <- result_table[,c("ProjectNumber","Treat_Area_ha","ETrt_obj_2_PCP")]
obj2_PCP_treated$objective <- 2
colnames(obj2_PCP_treated)<- c("ProjectNumber","Treat_Area_ha","PCP_treated","objective")

obj3_PCP_treated <- result_table[,c("ProjectNumber","Treat_Area_ha","ETrt_obj_3_PCP")]
obj3_PCP_treated$objective <- 3
colnames(obj3_PCP_treated)<- c("ProjectNumber","Treat_Area_ha","PCP_treated","objective")

PCP_treated <- rbind(obj1_PCP_treated,obj2_PCP_treated,obj3_PCP_treated)

plot_1 <- ggplot(PCP_treated,aes(x=ProjectNumber, y=PCP_treated, color=factor(objective)))+
  geom_line(linewidth=1.2)+
  scale_x_continuous(breaks = 1:10)+
  xlab("Project number")+
  ylab("Objective attainment (PCP)")+
  guides(color=guide_legend(title="Objective"))+
  ggtitle("Individual attainment") +
  theme(plot.title=element_text(hjust=0.5))+
  theme_classic()+
  theme(text=element_text(size=14))

#alternativelty, one can also plot the overall attainment for the three objective

plot_2 <- ggplot(result_table,aes(x=ProjectNumber,y=max_value))+
  geom_line()+
  scale_x_continuous(breaks = 1:10)+
  xlab("Project number")+
  ylab("Objective attainment")+
  ggtitle("Overall attainment") +
  theme(plot.title=element_text(hjust=0.5))+
  theme_classic()+
  theme(text=element_text(size=14))



ggarrange(plot_1,plot_2,ncol=2)
```

<img src="man/figures/multiobjective_attainment.jpg" width="800" align="center"/>

Notice we will need to filter the outputs to find the scenario where
each priority is equally weighted. We do this by filtering the priority
scores. Pr_1 indicates the first priority score, and Pr_2 indicates the
second priority score.

``` r
plot_dat_3 <- test_forest %>%
  group_by(proj_id) %>% summarize() %>%
  left_join(run_outputs_3$project_output %>% filter(Pr_1_priority1 == 1 & Pr_2_priority2 == 1) %>% 
              select(proj_id, treatment_rank))

plot(plot_dat_3[,'treatment_rank'], main="Project rank for two priorities")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Alternatively we could pass *forsys* the weighted scenario alone, rather
than running through all the weights and the 21 scenario outcomes. Here
we will utilize the combine_priorities function that we call outside of
the *forsys* run function.

``` r
# Create a new combined priority variable to pass directly to our priority weightings based on priority 1 and priority 2
test_forest <- test_forest %>% forsys::combine_priorities(
  fields = c('priority1','priority2'), 
  weights = c(1,1), 
  new_field = 'combo_priority')

# Recreate our input dataset
stand_dat <- test_forest %>% st_drop_geometry()

# Rerun forsys with the same scenario, passing the combo_priority as the new priority
run_outputs_4 = forsys::run(
  return_outputs = TRUE,
  scenario_name = "test_scenario",
  stand_data = stand_dat,
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = c("combo_priority"),
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  proj_fixed_target =  TRUE,
  proj_target_field = "area_ha",
  proj_target_value = 2000
)

plot_dat_4 <- test_forest %>%
  group_by(proj_id) %>% summarize() %>%
  left_join(run_outputs_4$project_output %>% select(proj_id, treatment_rank))
plot(plot_dat_4[,'treatment_rank'], main="Project rank")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Let’s make this scenario a bit more complex by using a stand threshold
and limiting stand selection to locations where threshold2 = 1 (yellow
areas in the map below).

``` r
plot(test_forest[,c('combo_priority','threshold2')], border=NA)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Let’s run this scenario.

``` r
run_outputs_5 = forsys::run(
  return_outputs = TRUE,
  scenario_name = "test_scenario",
  stand_data = stand_dat,
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  stand_threshold = "threshold2 == 1",
  scenario_priorities = c("combo_priority"),
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  proj_fixed_target =  TRUE,
  proj_target_field = "area_ha",
  proj_target_value = 2000
)

plot_dat_5 <- test_forest %>%
  group_by(proj_id) %>% summarize() %>%
  left_join(run_outputs_5$project_output %>% select(proj_id, treatment_rank))

plot(plot_dat_5[,'treatment_rank'], main="Project rank for two priorities for threshold2")
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Exploring different project prioritization methods

*Forsys* can build projects dynamically using a package called
*patchmax*, which requires some additional arguments and a shapefile as
the input. Here we will prioritize priority2 and build five 25,000
hectare patches.

``` r
library(patchmax)

# We will set run_with_patchmax to TRUE, then in the run functions we set the search distance weight to 1 to expand the search for high objective stands. We'll limit our search to test only 10% of the stands as patch seeds to speed up our run.
run_outputs_6 = forsys::run(
  return_outputs = TRUE,
  stand_data = test_forest,
  scenario_name = "patchmax_test",
  stand_id_field = "stand_id",
  stand_area_field = "area_ha",
  stand_threshold = "threshold2 >= 1",
  scenario_priorities = "priority2",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  run_with_patchmax = TRUE,
  patchmax_proj_size = 25000,
  patchmax_proj_number = 5,
  patchmax_SDW = 1,
  patchmax_sample_frac = 0.1
)

# Plot treatment rank of patches

plot_dat_6 <- run_outputs_6$stand_output %>% filter(DoTreat == 1) %>%
  mutate(treatment_rank = proj_id, stand_id = as.integer(stand_id))
plot_dat_6 <- test_forest %>% left_join(plot_dat_6 %>% select(stand_id, treatment_rank)) %>%
  group_by(treatment_rank) %>% summarize()
plot(plot_dat_6[,'treatment_rank'], border=NA, main="Patch rank")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Running ForSysXR using predefined XML

ForSysXR can be used to run predefined XML files. This can be useful to include in R scripts or loops. This function is similar to run zones in ForSysX

``` r
run_forsysx_xml(exe_path = "C:/Users/ForSysXR/ForSysXConsole.exe",
                xml_path = "C:/Users/ForSysXR/run_tutorial_1.xml")
``` 


## Running ForSys with subunits

ForSys can also be constrained by subunits. This will allow pre-defined planning areas to be used when creating scenarios.
When using pre-defined planning areas, the “subunit_field” must be set to the field that places stands into planning areas.
The following example shows how the ForSysXR can be run with pre-defined planning areas. First, we plot the subunits in the dataset

``` r
stands_data  %>%
  ggplot() +
  geom_sf(aes(fill=factor(subunit)),colour=NA) +
  scale_fill_manual(values=c("grey80","grey40"))+
  #scale_fill_viridis_c(option = "A",na.value = "grey50")+
  ggtitle("Subunits") +
  theme_void()+
  labs(fill='Subunit number')+
  theme(plot.title=element_text(hjust=0.5))
``` 

<img src="man/figures/subunits.jpg" width="400" align="center"/>


In the following example, we set the number of projects per subunit as three, each with a total treated area of 50 hectares.

``` r
set_forsysx_run (input_shapefile = stands_data,
                 outputs_base_name = "C:/Users/ForSysXR/run_tutorial_subunits",
                 stand_id = "Stand_ID",
                 area = "Area_ha",
                 available = "availuse",
                 exclude_field = "water",
                 seed_stands_only_available_stands = 1,
                 x_coordinate = "X_Coord",
                 y_coordinate = "Y_Coord",
                 max_number_projects = 3,
                 adjacency_matrix = "C:/Users/ForSysXR/adjacency_matrix_forsys.csv",
                 constraints_name = "Area_ha",
                 constraints_value = 50,
                 constraints_slack = "1.00",
                 effect_fields = c("obj_1","obj_2","obj_3"),
                 objectives = c("obj_1","Treat","1","1","1"),
                 output_xml = "C:/Users/ForSysXR/test_tutorial_multiobjectives.xml",
                 run_forsysx = 1,
                 subunit_field = "subunit",
                 plot_results=TRUE,
                 exe_path = "C:/Users/ForSysXR/ForSysXConsole.exe",
                 save_outputs = c("stand_csv","shapefile","image"))
``` 

<img src="man/figures/three_projects_subunits.jpg" width="400" align="center"/>


## Running ForSys with zones
Running ForSys with zones allows the user to run several XML files within a single command.
To exemplify the option of running zones in ForSys, we will use another dataset included in the package. 
The new dataset is called `stands_data_FBN` and includes real stands from a study area with one objective that can be optimized, an area field that can be used as a constraint, a field representing the distance to the fuelbreak network to be used as a threshold, and a field with the order of execution of the fuelbreak network. It also contains fields with the stands' availability for treatments and stands to be excluded from the analysis.

First, one can plot the new stand dataset highlighting the predefined fuel break network projects as follows (note that stands that are not part of the fuelbreak network will be displayed as NA)

``` r
data("stands_data_FBN")
head(stands_data_FBN)

stands_data_FBN  %>%
  mutate_at(c('FBN_rank'), ~na_if(., 0)) %>%
  ggplot() +
  geom_sf(aes(fill=factor(FBN_rank))) +
  scale_fill_viridis_d(option = "A",na.value = "grey60")+
  ggtitle("Fuelbreak network projects") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))+
  guides(fill=guide_legend(title="FBN number"))
```

<img src="man/figures/FBN_projects.jpg" width="400" align="center"/>


As illustrated in the image above, there are three fuelbreak network projects. The number 1 is the most priority one and should be implemented first, while the third project has the least priority. 
To understand the use of zones, we can set the following research problem:
i) After implementing a fuelbreak network project, we want to create a restoration project in the landscape before moving to the second fuelbreak network project
ii) The restoration project must be within a given distance of the fuelbreak network project that was just implemented.


Given the objectives, we will need to create three different shapefiles, where the distance to the most recently implemented fuelbreak network project changes.

``` r
distance_to_FBN_projects <- function(my_stands,FBN_projects,output_folder){
  if(class(my_stands)[1]=="character"){
    my_stands_use <- sf::st_read(my_stands)}
  
  if(class(my_stands)[1]=="sf"){
    my_stands_use <-(my_stands)}
    
  #get the position of the variable with FBN projects
  FBN_porj_position <- grep(FBN_projects, colnames(my_stands_use))
  names(my_stands_use)[FBN_porj_position] <- "FBN_proj"
  my_linear_projs <-  subset(my_stands_use,FBN_proj!=0)

  for(i in 1:max(my_stands_use$FBN_proj,na.rm = TRUE)){
    my_linear_projs_loop <- subset(my_stands_use,FBN_proj==i)
    # create an index of the nearest feature
    index <- sf::st_nearest_feature(x = my_stands_use, y = my_linear_projs_loop)
    my_FBN_2 <- my_linear_projs_loop %>% slice(index)
    poly_dist <- as.numeric(sf::st_distance(x = my_stands_use, y= my_FBN_2, by_element = TRUE))
    my_stands_use$distance <- poly_dist
    my_stands_final <- my_stands_use
    
    #create a temp folder
    dir.create(file.path(output_folder, "temp_folder_shp"), showWarnings = FALSE)
    sf::st_write(my_stands_final,paste(output_folder, "/temp_folder_shp/","interactive_zones_",i,".shp",sep=""))}}


distance_to_FBN_projects(my_stands=stands_data_FBN,
                         FBN_projects="FBN_rank",
                         output_folder="C:/Users/ForSysXR")
```


This function will create three shapefiles (one for each fuelbreak network project) with the corresponding distance to the newest fuelbreak network project.
The shapefiles are named interactive_zones and are located in a temporary folder inside the path given by the user. This temporary folder can be deleted after running ForSys (see below).


To plot the temporary files created with the corresponding distance to each fuelbreak network project, one can do the following

``` r
for(i in 1:3){
  shp_data_i <- sf::st_read(paste("C:/Users/ForSysXR/temp_folder_shp/interactive_zones_",i,".shp",sep=""))
  
  plot_i <- shp_data_i  %>%
    ggplot() +
    geom_sf(aes(fill=distance),colour=NA) +
    ggtitle(paste("Distance to FBN project ",i,sep="")) +
    scale_fill_viridis_c(option = "inferno",na.value = "grey50",direction=-1)+
    theme_void()+
    theme(plot.title=element_text(hjust=0.5))+
    guides(fill=guide_legend(title="Distance (m)"))
    assign(paste("plot_",i,sep=""),plot_i)
    rm(shp_data_i,plot_i)
}

ggpubr::ggarrange(plot_1,plot_2,plot_3,nrow=1,ncol=3,common.legend = TRUE,legend="bottom")
```

<img src="man/figures/distance_to_FBN_projects.jpg" width="600" align="center"/>


The first step to use ForSysXR is the creation of the XML file that defines the ForSysX run. Because in this case, the only difference between the runs is the input shapefile (i.e. all run parameters should be the same), one can create the XML within a simple for loop in R. This will create the three XML files without running ForSysX

``` r
for(i in 1:3){
  set_forsysx_run (input_shapefile = paste("C:/Users/ForSysXR/temp_folder_shp/interactive_zones_",i,".shp",sep=""),
                   outputs_base_name = paste(C:/Users/ForSysXR/temp_folder_shp/run_tutorial_zones_",i,sep=""),
                   stand_id = "ID_forsys",
                   area = "area_ha",
                   available = "avail_fin",
                   exclude_field = "Exclude",
                   seed_stands_only_available_stands = 1,
                   x_coordinate = "Point_X",
                   y_coordinate = "Point_Y",
                   max_number_projects = 1,
                   output_adjacency_matrix = "C:/Users/ForSysXR/temp_folder_shp",
                   constraints_name = "area_ha",
                   constraints_value = 500,
                   constraints_slack = "1.00",
                   effect_fields = c("ob2_PCP"),
                   objectives = c("ob2_SPM","Treat","1","1","1"),
                   output_xml = paste("C:/Users/ForSysXR/temp_folder_shp/test_tutorial_zones_",i,".xml",sep=""),
                   run_forsysx = 0,
                   threshold=c("distance","<=",2500),
                   save_outputs = c("stand_csv","shapefile","image"))}
``` 


## Running ForSys with zones interactively
Running ForSys with interactive zones allows the update of fields of the input stand shapefile between runs. A simple run in Forsys will only prevent previous projects from being allocated to new projects. However, in some cases, this may not be enough. For instance, if the research objective is to allocate landscape treatments to areas close to a fuel break network at the same time it is being implemented (i.e. co-prioritization), the simple run with zones will not ensure that projects do not overlap. For the moment, this feature is only available in ForSysXR package.


First, one can plot the new stand dataset highlighting the predefined fuel break network projects as follows (note that stands that are not part of the fuelbreak network will be displayed as NA)

``` r
FBN_restoration_data  %>%
  mutate_at(c('FBN_rank'), ~na_if(., 0)) %>%
  ggplot() +
  geom_sf(aes(fill=factor(FBN_rank))) +
  scale_fill_viridis_d(option = "A",na.value = "grey60")+
  ggtitle("Fuelbreak network projects") +
  theme_void()+
  theme(plot.title=element_text(hjust=0.5))+
  guides(fill=guide_legend(title="FBN number"))
```


<img src="man/figures/FBN_projects.jpg" width="400" align="center"/>






Finally, to run ForSysXR with zones interactively one can simply use the function ```run_zones_interactive=TRUE``` as follows

``` r
run_forsysx_xml(exe_path="C:/Users/ForSysXR/ForSysXConsole.exe", 
                xml_folder="C:/Users/ForSysXR/temp_folder_shp/xml_to_use",
                run_zones_interactive=TRUE)
``` 


The same approach can be applied to other problems. For instance, if the treatments should be placed within a given distance from specific urban areas.


## Citation

Please cite the *forsys* package when using it in publications. To cite
the current official version, please use:

> Aparício BA and Ager A. (2023).
> ForSysXR: A R package for running the ForSysX scenario planning platform for modeling multi-criteria spatial
> priorities. R package version 0.9. Available at
> <https://https://github.com/bmaparicio/ForSysXR>.

## Additional resources

The [package
website](https://www.fs.usda.gov/rmrs/projects/forsys-scenario-planning-model-multi-objective-restoration-and-fuel-management-planning)
contains information on the *forsys* package.

## Getting help

If you have any questions about the *ForSysXR* package or suggestions for
improving it, please [post an issue on the code
repository](https://github.com/bmaparicio/ForSysXR/issues).
