#==============================#
#### Loading & Shaping data ####
#==============================#

stage_list <- c("P4", "P8", "P12", "P21", "Ad")
layer_list <- c("EGL", "ML", "IGL")

#-----------#
#### MBP ####
#-----------#

load_mbp <- function(path = config$data$mbp_path) {
  return(
    readxl::read_excel(path)
    |> tidyr::separate(Sample, sep = " ", into = c("Mouse", "c", "Slice", "Loc"), remove = T, convert = T)
    |> tidyr::extract(
      Mouse,
      into = c("Bloodline", "Pup", "Condition"),
      regex = "^([dDaA]+)(\\d{1})([hH]+|[nN]+)$",
      convert = T, remove = F
    )
    |> tidyr::unite("Sample", c(Bloodline, Pup, Condition, Slice, Loc), sep = "", remove = F)
    |> mutate(
      across(matches("_Fil"), \(.x) .x / 1e4),
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH"))
    )
    |> arrange(Condition, Pup, Slice, Loc)
    |> select(Sample, Mouse, Stage, Condition, N_DD, Length_Fil, Area_Fil, Vol_Fil, Angle_Fil)
  )
}

#---------------#
#### DAPI_ML ####
#---------------#

load_dapi_ml_ad <- function(path = config$data$dapi_ml_ad_path) {
  return(
    read_excel(path)
    |> tidyr::extract(
      Mouse,
      into = c("Bloodline", "MouseID", "Condition"),
      regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)$",
      convert = T, remove = F
    )
    |> tidyr::unite("Slice", c(Slice, Stack), sep = "", remove = F)
    |> mutate(
      Vol_ML = A_ML * 11, # Layer thickness = 11μm
      across(matches("^A_|^Vol_"), \(.x) .x / 1e4),
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Z = factor(str_to_sentence(Z))
    )
    |> arrange(Condition, Slice)
    |> select(Mouse, Condition, Z, Slice, A_ML, Vol_ML, A_DAPI, Vol_DAPI)
  )
}

#--------------------#
#### DAPI_Density ####
#--------------------#

load_dapi_density_P12 <- function(path = config$data$dapi_density_P12_path) {
  return(
    readxl::read_excel(path)
    |> filter(!Outlier)
    |> tidyr::extract(
      Mouse,
      into = c("Bloodline", "MouseID", "Condition"),
      regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)$",
      convert = T, remove = F
    )
    |> mutate(
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Mouse = factor(Mouse),
      Layer = factor(Layer, levels = layer_list)
    )
    |> arrange(Condition, Slice)
    |> select(Layer, Mouse, Condition, Slice, Nb_Area, Nb_Vol, Avg_Distance)
  )
}

#-------------#
#### VGLUT ####
#-------------#

load_vglut <- function(path = config$data$vglut_path) {
  return(
    readxl::read_excel(path)
    |> filter(!Outlier)
    |> tidyr::extract(
      Sample,
      into = c("Bloodline", "MouseID", "Condition", "Slice", "Loc"),
      regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)(\\d{1})(\\w{1})$",
      convert = T, remove = F
    )
    |> tidyr::unite("Mouse", c(Bloodline, MouseID, Condition), sep = "", remove = F)
    |> mutate(
      across(matches("^A_"), \(.x) .x / 1e4),
      A_CL_VGLUT_DD_per_cell = A_CL_VGLUT_DD / N_CC,
      A_DD_per_cell = A_DD / N_CC,
      Vol_DD_per_cell = Vol_DD / N_CC,
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Z = factor(str_to_sentence(Z))
    )
    |> arrange(Condition, Slice, Loc)
    |> select(Sample, Mouse, Stage, Condition, Z, N_CC, A_DD, A_DD_per_cell, A_VGLUT_CF, A_VGLUT_MF, Vol_DD, Vol_DD_per_cell, A_ML, Vol_ML, Thick_ML)
  )
}


#-------------#
#### GLUD2 ####
#-------------#

load_GluD2 <- function(path = config$data$GluD2_path) {
  return(
    readxl::read_excel(path)
    |> mutate(Sample = paste("DA", Sample, sep = ""))
    |> tidyr::extract(
      Sample,
      into = c("Bloodline", "MouseID", "Condition", "Slice", "Loc"),
      regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)(\\d{1})(\\w{1})$",
      convert = T, remove = F
    )
    |> tidyr::unite("Mouse", c(Bloodline, MouseID, Condition), sep = "", remove = F)
    |> mutate(
      # Areas are already 10e-5
      # A_CL_GLUD2_DD_per_cell = A_CL_GLUD2_DD / N_CC,
      A_DD_per_cell = A_DD / N_CC,
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Z = factor(str_to_sentence(Z))
    )
    |> arrange(Condition, Slice, Loc)
    |> select(Sample, Mouse, Stage, Condition, Z, N_CC, A_GLUD2, Vol_GLUD2, A_DD, A_DD_per_cell)
  )
}

#----------------#
#### Purkinje ####
#----------------#

## Combined Glur2 & Vglut data
load_purkinje <- function(vglut_path = config$data$vglut_path, GluD2_path = config$data$GluD2_path) {
  return(
    full_join(
      load_vglut(vglut_path) |> 
        filter(Experiment == "A") |> 
        select(Sample, Mouse, Condition, Z, N_CC, A_DD),
      load_GluD2(GluD2_path) 
        |> tidyr::extract(
            Sample,
            into = c("Bloodline", "MouseID", "Condition", "Slice", "Loc"),
            regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)(\\d{1})(\\w{1})$",
            convert = T, remove = T
          )
        |> mutate(Slice = Slice + 8)
        |> tidyr::unite("Sample", c(Bloodline, MouseID, Condition, Slice), sep = "", remove = F)
        |> tidyr::unite("Mouse", c(Bloodline, MouseID, Condition), sep = "", remove = F)
        |> mutate(
          Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
          Z = factor(str_to_sentence(Z))
        )
        |> arrange(Condition, Slice, Loc)
        |> select(Sample, Mouse, Condition, Z, N_CC, A_DD)
    )
    |> mutate(A_DD = A_DD / N_CC)
    |> arrange(Condition, Mouse)
    |> rowid_to_column("ID")
  )
}

#------------#
#### BrDU ####
#------------#

load_BrDU <- function(path = config$data$brDU_path) {
  return(
    readxl::excel_sheets(path)
    |> purrr::set_names()
    |> purrr::map_df(\(.x) readxl::read_excel(path = path, sheet = .x), .id = "Layer")
    |> tidyr::extract(
      Mouse,
      into = c("Bloodline", "MouseID", "Condition"),
      regex = "^(\\w{2,3})(\\d{1})([hH]+|[nN]+)$",
      convert = T, remove = F
    )
    |> tidyr::unite("Sample", c(Bloodline, MouseID, Condition, Well), sep = "", remove = F)
    |> mutate(
      Dens_BrDU = Dens_BrDU * 1e5,
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Layer = factor(Layer)
    )
    |> arrange(Layer, Condition, Sample)
    |> select(Layer, Sample, Stage, Mouse, Condition, Well, Dens_BrDU)
  )
}


#---------------#
#### Calb-P12 ####
#---------------#

load_Calb_P12 <- function(path = config$data$calb_P12_path) {
  return(
    readxl::read_excel(path)
    |> mutate(
      across(matches("^A_|^Vol_"), \(.x) as.numeric(.x) / 1e4),
      A_PC_per_cell = A_PC / N_CC,
      Vol_PC_per_cell = Vol_PC / N_CC,
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Z = factor(str_to_sentence(Z))
    )
    |> arrange(Condition, Sample, Slice)
    |> select(Sample, Mouse, Stage, Slice, Z, Condition, N_CC, A_PC, Vol_PC, A_PCL, A_ML, A_PC_per_cell, Vol_PC_per_cell)
  )
}

#---------------#
#### Calb-P21 ####
#---------------#

load_Calb_P21 <- function(path = config$data$calb_P21_path) {
  return(
    readxl::read_excel(path, sheet = "Filtered")
    |> tidyr::extract(
      Mouse,
      into = c("Bloodline", "MouseID", "Condition"),
      regex = "^(\\w{1,2})(\\d{1})([hH]+|[nN]+)$",
      convert = T, remove = F
    )
    |> tidyr::unite("Sample", c(Bloodline, MouseID, Condition, Slice, Stack), sep = "", remove = F)
    |> tidyr::unite("Slice", c(Slice, Stack), sep = "", remove = F)
    |> mutate(
      across(matches("^A_|^Vol_"), \(.x) as.numeric(.x) / 1e4),
      A_PC_per_cell = A_PC / N_CC,
      Vol_PC_per_cell = Vol_PC / N_CC,
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Z = factor(str_to_sentence(Z))
    )
    |> arrange(Condition, Sample, Slice)
    |> select(Sample, Mouse, Stage, Slice, Z, Condition, N_CC, A_ML, A_PC_per_cell, Vol_PC_per_cell)
  )
}


#------------#
#### Casp ####
#------------#

load_Casp <- function(stage = NULL) {
  if (is.null(stage) || stringr::str_to_sentence(stage) == "All") {
    paths <- str_subset(names(config$data), pattern = "^casp_P(\\d{1,2}|Ad)")
    purrr::map_df(paths, \(path) load_Casp(str_extract(path, pattern = "(P\\d{1,2})")))
  }
  else {
    path <- str_subset(names(config$data), pattern = paste0("^casp_", stage))
    
    if (length(path) == 0) {
      log.error("[DATA] No file path matches" %s+% stage)
      return(NULL)
    }
    else if (length(path) > 1) {
      log.warn("[DATA] More than one file path matches" %s+% stage)
    }
    else {
      return(
        readxl::read_excel(config$data[[path[[1]]]])
        |> tidyr::extract(
          Sample,
          into = c("Bloodline", "MouseID", "Condition"),
          regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)$",
          convert = T, remove = T
        )
        |> tidyr::unite("Sample", c(Bloodline, MouseID, Condition, Slice), sep = "", remove = F)
        |> tidyr::unite("Mouse", c(Bloodline, MouseID, Condition), sep = "", remove = F)
        |> mutate(
          across(matches("^A_|^C_"), \(.x) .x / 1e6),
          Dens_EGL = N_EGL / A_EGL,
          Dens_ML_PCL = N_ML_PCL / A_ML_PCL ,
          Dens_IGL_WM = N_IGL_WM / A_IGL_WM,
          Dens_Tot = N_Tot / A_Tot,
          Prop_C_EGL = C_EGL / A_EGL,
          Prop_C_ML_PCL = C_ML_PCL / A_ML_PCL,
          Prop_C_IGL_WM = C_IGL_WM / A_IGL_WM,
          Prop_C_Tot = C_Tot / A_Tot,
          Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
          Z = factor(str_to_sentence(Z))
        )
        |> arrange(Condition, Sample)
        |> select(Sample, Mouse, Stage, Condition, Z, matches("_EGL"), matches("_ML_PCL"), matches("_IGL_WM"), matches("_Tot"))
        |> mutate(Stage = factor(str_extract(path, pattern = "P(\\d{1,2})"), levels = stage_list), .before = Sample)
      )
    }
  }
}


#----------------#
#### Casp-Act ####
#----------------#

load_Casp_Act <- function(path = config$data$casp_Act_path) {
  return(
    readxl::read_xlsx(path) 
    |> tidyr::fill(c(Mouse, Experiment), .direction = "down") 
    |> tidyr::extract(
      Mouse,
      into = c("ID", "Pup", "Condition"),
      regex = "^(\\w{2})(\\d{1})([hH]+|[nN]+)$",
      convert = T, remove = F
    )
    |> select(Experiment, Stage, Condition, Mouse, Fluo_Norm)
    |> mutate(
      across(where(is.character), .fns = \(.x) as.factor(.x)),
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH"))
    )
  )
}

#---------------#
#### Weights ####
#---------------#

load_Weight <- function(path = config$data$weight_path, age = "All") {
  if (is.null(age) || str_to_sentence(age) == "All") {
    return(
      purrr::map_df(
        .x = readxl::excel_sheets(path) |> rlang::set_names(), 
        .f = \(.x) load_Weights(path = path, age = .x)
        # .id = "Age"
      )
    )
  } else {
    return(
      readxl::read_xlsx(path, sheet = age)
      |> tidyr::extract(
        Mouse,
        into = c("Bloodline", "MouseID", "Condition"),
        regex = "^(\\w{1,2})(\\d{1,2})([hH]+|[nN]+)$",
        convert = T, remove = F
      )
      |> mutate(
        across(c(Mouse), \(.x) as.factor(.x)),
        Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
        Stage = factor(Stage, levels = paste0("P", 2:63)),
        Day = str_extract(Stage, "(\\d{1,2})$") |> as.integer()
      )
      |> droplevels(except = c("Mouse", "Condition"))
      |> arrange(Condition, Mouse)
      |> select(Mouse, Stage, Condition, Stage, Day, Weight)
    )
  }
}

load_Weight_Nest <- function(path = config$data$weight_path) {
  return(
    readxl::read_xlsx(path, sheet = "Nest")
    |> mutate(Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")))
  )
}

load_Weight_Gain <- function(path = config$data$weight_path) {
  return(
    readxl::read_xlsx(path, sheet = "Weight_Gain_PT")
    |> tidyr::unite(
      "Mouse",
      Litter, MouseID, Condition,
      sep = "",
      remove = F
    )
    |> filter(Stage != "P2") # Removing P2 since it's the point of origin (and thus always 0)
    |> mutate(
      Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH")),
      Stage = factor(Stage, levels = c(paste0("P", 3:11), "P16", "P21")),
      Day = str_extract(Stage, "(\\d{1,2})$") |> as.integer()
    )
    |> select(Mouse, Condition, Sex, Stage, Day, Weight_Gain)
  )
}

#----------------#
#### Behavior ####
#----------------#

load_behavior_pups <- function(path = config$data$behavior_pups_path) {
  return(
    readxl::read_excel(path)
    |> tidyr::unite("Mouse", c(Experiment, Mouse, Condition), sep = "", remove = F)
    |> mutate(
      # across(matches("Time"), \(.x) as.numeric(gsub(",", ".", .x, fixed = TRUE))),
      across(c(Mouse), \(.x) as.factor(.x)),
      Condition = factor(Condition, levels = c("N", "IH")),
      Stage = factor(Stage, levels = paste0("P", 2:11)),
      Day = str_extract(Stage, "(\\d{1,2})$") |> as.integer()
      
    )
    |> arrange(Condition, Mouse, Stage)
    |> select(Mouse, Stage, Condition, Stage, Day, matches("Time"))
  )
}

load_behavior_teens <- function(path = config$data$behavior_teens_path) {
  return(
    readxl::read_excel(path)
    |> tidyr::unite("Mouse", c(Experiment, Mouse, Condition), sep = "", remove = F)
    |> mutate(
      across(c(Mouse, Measure), \(.x) as.factor(.x)),
      Condition = factor(Condition, levels = c("N", "IH"))
    )
    |> arrange(Condition, Mouse)
    |> select(Mouse, Condition, Weight, Measure, Grip_Strength)
  )
}

load_behavior_adults <- function(path = config$data$behavior_adults_path, sheet) {
  return(
    readxl::read_excel(path, sheet = sheet)
    |> tidyr::unite("Mouse", c(Experiment, Mouse, Condition), sep = "", remove = F)
    |> mutate(
      across(c(Mouse), \(.x) as.factor(.x)),
      Condition = factor(Condition, levels = c("N", "IH"))
    )
    |> arrange(Condition, Mouse)
    |> select(Mouse, Stage, Condition, everything(), -Experiment)
  )
}

#-----------------#
#### Thickness ####
#-----------------#

load_Thickness <- function(path = config$data$thickness_path, stage = "All") {
  if (is.null(stage) || str_to_sentence(stage) == "All") {
    return(
      purrr::map_df(
        .x = readxl::excel_sheets(path) |> rlang::set_names(), 
        .f = \(.x) load_Thickness(path = path, stage = .x)
      )
    )
  } else {
    return(
      readxl::read_xlsx(path, sheet = stage)
      |> filter(!Outlier)
      |> group_by(Stage, Mouse, Slice)
      |> mutate(MeasureID = seq(1:n()))
      |> ungroup()
      |> mutate(
        Slice = paste0(Mouse, Slice),
        Sample = paste0(Slice, MeasureID),
        across(c(Mouse, Slice), \(.x) as.factor(.x)),
        Condition = factor(Condition, levels = c("N", "H"), labels = c("N", "IH"))
      )
      |> arrange(Condition, Mouse)
      |> select(Sample, Stage, Slice, MeasureID, Mouse, Condition, everything(), -Outlier)
    )
  }
}

#-----------#
#### ROS ####
#-----------#

load_ROS <- function(path = config$data$ROS_path) {
  return(
    readxl::read_excel(path)
    |> mutate(
      across(c(Mouse), \(.x) as.factor(.x)),
      Condition = factor(Condition, levels = c("N", "IH"))
    )
    |> arrange(Condition, Mouse)
    |> select(Mouse, Condition, everything())
  )
}