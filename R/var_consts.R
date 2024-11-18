# plotting constants
theme_screen <- ggplot2::theme_light(base_size=28)
theme_slide <- ggplot2::theme_light(base_size=20)
label_size <- 10

# max levels to allow factoring on the fly
max_levels_for_factor <- 20

# powerpoint generation constants
loc_title <- officer::ph_location_type(type = "title")
loc_footer <- ph_location_type(type = "ftr")
loc_dt <- ph_location_type(type = "dt")
loc_slidenum <- ph_location_type(type = "sldNum")
loc_body <- ph_location_type(type = "body")
loc_body_left <- ph_location_left()
loc_body_right <- ph_location_right()
loc_ctrtitle <- ph_location_type(type="ctrTitle")
loc_subtitle <- ph_location_type(type="subTitle")
loc_fullsize <- ph_location_fullsize(width = 0.8, height=0.8,)

toad_liver_query <-
  "SELECT
    	lqi.lqi_id
    	,lqi.case_date
    	,EXTRACT(YEAR FROM lqi.case_date) AS case_year
    	,meld.creatinine AS preop_cr
    	,meld.inr AS preop_inr
    	,meld.bilirubin AS preop_bili
    	,meld.meld
    	,meld.meldna
    	,liver.f_lab_at_timepoint(lqi.lqi_id, 'preop','hemoglobin')::numeric AS preop_hb
    	,lqi.ffp::numeric as ffp
    	,lqi.prbcs::numeric as prbcs
    	,lqi.platelets::numeric as platelets
    	,CASE WHEN lqi.living_related_flag = 't' THEN TRUE ELSE FALSE END AS living_related_flag
    	,CASE WHEN lqi.diagnosis_hcc = 't' THEN TRUE ELSE FALSE END AS diagnosis_hcc
    	,star.prev_ab_surg_trr
    	,star.tipss_trr
    	,aki.postop_aki_stage_composite
    	,CASE WHEN aki.postop_aki_stage_composite >= 2 THEN TRUE ELSE FALSE END AS postop_aki_23_flag
    FROM liver.mv_case_liverqi lqi
    LEFT JOIN LATERAL liver.f_meld_by_lqi_id(lqi.lqi_id) meld ON TRUE
    LEFT JOIN LATERAL liver.f_postop_aki_by_lqi_id(lqi.lqi_id, 48,96) aki ON true
    INNER JOIN liver.mv_case_star star ON star.lqi_id = lqi.lqi_id
    WHERE lqi.case_date > '2012-06-01'
    	AND COALESCE(lqi.aborted_flag,'f') <> 't'
    	AND lqi.aborted_flag <> 't'
    	AND lqi.patient_age_years::int >= 18"
