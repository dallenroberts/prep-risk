
# Modeling Prevention-Effective PrEP Use 
Commissioning scripts for running EMOD-HIV models calibrated to Eswatini that compare PrEP use by risk group to PrEP use by partnership

## COMPS commissioning process
1. Verify `optim_script.py` settings
	a) Scale factor set to 0.05
	b) Config file specified manually
2. Update `scenarios.csv` for desired COMPS suite
3. Verify `simtools.ini` settings
4. To run on COMPS: `python run_scenarios.py -m provided -c optim_script.py --samples calibrated_parameter_sets.csv -s [SUITE_NAME] -o output/[SUITE_NAME] --table scenarios.csv --no-download`
5. Then process from **hivdtkproc** repository.

## Directories (Current)
- **Scenarios/** contains scenarios.csv files that control settings for `run_scenarios.py`
- **bin/** contains EMOD binaries
- **InputFiles/** contains EMOD campaign and configuration .json files
- **Data/** contains calibration ingest form
- **R/** contains R scripts useful for commissioning

## Files
- **calibrated_parameter_sets.csv**: 250 parameter sets from previously calibrated model
- **Data/calibration_ingest_form-Eswatini.xlsm**: Ingest form for calibration and describing parameter set mapping
- **scenarios.csv**: Read by `run_scenarios.py` for commissioning on COMPS
- **simtools.ini**: COMPS settings

## Scripts
- `optim_script.py`: Sets base population scale factor and config file
- `run_scenarios.py`: Commissions COMPS scripts (no editing needed)
- `R/create_prep_by_partnership_scenarios.R`: Creates scenarios.csv for partnership PrEP sweep
- `R/create_prep_by_risk_group_scenarios.R`: Creates scenarios.csv for risk group PrEP sweep
- 
## Directories (Deprecated)
- **R**: (*most moved over to hivdtkproc*)
- **output** :contains COMPS output (*analysis moved to SSMT, outputs moved to hivdtkproc*)
- **figures**: (*analysis moved to SSMT, outputs moved to hivdtkproc*)

## Analyses
- **calibration_check**: Compare campaigns and max concurrent clients for FSW
	- Scenario:`scenarios_calibration_check.csv`
	- Config: `config_calibration.json`
	- Commissioning script: `python run_scenarios.py -m provided -c optim_script.py --samples calibrated_parameter_sets.csv -s calibration_check -o output/calibration_check --table scenarios.csv --no-download`
- **calibration**: Final calibration results and plots
	- Scenario:`scenarios_calibration.csv`
	- Config: `config_calibration.json`
	- `python run_scenarios.py -m provided -c optim_script.py --samples calibrated_parameter_sets.csv -s calibration -o output/calibration --table scenarios.csv --no-download` 
- **relationships**: Relationships analysis for HIV-negative 15-34 cohort
	- Scenario: `scenarios_relationships.csv`
	- Config: `config_relationships.json`
	- `python run_scenarios.py -m provided -c optim_script.py --samples calibrated_parameter_sets.csv -s relationships -o output/relationships --table scenarios.csv --no-download`
- **prep_risk_group_sweep\***: Sweep over coverage levels of PrEP targeted to specific risk groups. Note that due to COMPS commissionning errors, this was broken up into several suites.
	- Scenario: `scenarios_prep_by_risk_group.csv`
	- Config: `config_prep_sweep.json`
	-`python run_scenarios.py -m provided -c optim_script.py --samples calibrated_parameter_sets.csv -s prep_risk_group_sweep -o output/prep_risk_group_sweep --table scenarios.csv --no-download`
- **prep_partnership_sweep\***: Sweep over coverage levels of PrEP targeted based on relationships. Note that due to COMPS commissionning errors, this was broken up into several suites.
	- Scenario: `scenarios_prep_by_partnership.csv`
	- Config: `config_prep_sweep.json`
	-`python run_scenarios.py -m provided -c optim_script.py --samples calibrated_parameter_sets.csv -s prep_partnership_sweep -o output/prep_partnership_sweep --table scenarios.csv --no-download`

