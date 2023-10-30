# ForSysXR

<!--- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/forsys_consortium_logo.png" align="right" style="height:90px!important; position:absolute; top:10px; right:10px" />

## Scenario planning for land management

ForSys a land management planning model that explores potential outcomes
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

We run *forsys* with the following arguments. *Forsys* always writes its
outputs to csv files saved within the output folder, but we can
optionally set it to write that data out to a list which has three
elements containing the outputs.

``` r
stand_dat <- test_forest %>% st_drop_geometry()

run_outputs <- forsys::run(
  return_outputs = TRUE,
  scenario_name = "test_scenario",
  stand_data = stand_dat,
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = "priority1",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  proj_fixed_target =  TRUE,
  proj_target_field = "area_ha",
  proj_target_value = 2000
)
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

Next we look at multiple priorities. Plotting priorities 1 and 2 shows
that areas where priority 1 are highest tend to be lower for priority 2.

``` r
plot(test_forest[,c('priority1','priority2')], border=NA)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Let’s see if *forsys* can find locations where we can achieve both
objectives. We prioritize on both variables, priority1 and priority2. We
run *forsys* weighting the two objectives from 0 to 5, which results in
21 scenarios. We then filter the results to observe the outcome of the
scenario where the two objectives are equally weighted. The project rank
graph represents areas that are highest for both priorities.

``` r
run_outputs_3 = forsys::run(
  return_outputs = TRUE,
  scenario_name = "test_scenario",
  stand_data = stand_dat,
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = c("priority1","priority2"),
  scenario_weighting_values = c("0 5 1"),
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  proj_fixed_target =  TRUE,
  proj_target_field = "area_ha",
  proj_target_value = 2000
)
```

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
