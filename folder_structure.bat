@echo off
chcp 65001 >nul
echo ============================================================
echo  CREATION DE LA STRUCTURE DU PROJET CNPS_TREATMENT
echo ============================================================
echo.

set "ROOT=C:\Users\e_koffie\Documents\Salaires\CNPS_TREATMENT_PROJECT"

:: Config
mkdir "%ROOT%\config" 2>nul

:: Data
mkdir "%ROOT%\data\raw" 2>nul
mkdir "%ROOT%\data\raw\excel" 2>nul
mkdir "%ROOT%\data\processed" 2>nul
mkdir "%ROOT%\data\processed\dta" 2>nul
mkdir "%ROOT%\data\processed\registry" 2>nul
mkdir "%ROOT%\data\cleaned\individual" 2>nul
mkdir "%ROOT%\data\cleaned\firm_time" 2>nul
mkdir "%ROOT%\data\cleaned\analytical" 2>nul
mkdir "%ROOT%\data\cleaned\imputations" 2>nul
mkdir "%ROOT%\data\cleaned\monthly" 2>nul
mkdir "%ROOT%\data\cleaned\final" 2>nul
mkdir "%ROOT%\data\archive" 2>nul

:: Models
mkdir "%ROOT%\models\declaration\fitted" 2>nul
mkdir "%ROOT%\models\declaration\diagnostics" 2>nul
mkdir "%ROOT%\models\declaration\summaries" 2>nul
mkdir "%ROOT%\models\imputation\fitted" 2>nul
mkdir "%ROOT%\models\imputation\diagnostics" 2>nul
mkdir "%ROOT%\models\imputation\summaries" 2>nul
mkdir "%ROOT%\models\registry" 2>nul

:: Sessions
mkdir "%ROOT%\sessions\registry" 2>nul
mkdir "%ROOT%\sessions\active" 2>nul

:: Output
mkdir "%ROOT%\output\indicators" 2>nul
mkdir "%ROOT%\output\analytical_reports" 2>nul
mkdir "%ROOT%\output\inconsistencies" 2>nul

:: Scripts
mkdir "%ROOT%\scripts\ingestion" 2>nul
mkdir "%ROOT%\scripts\preparation" 2>nul
mkdir "%ROOT%\scripts\structuring" 2>nul
mkdir "%ROOT%\scripts\modeling" 2>nul
mkdir "%ROOT%\scripts\diagnostics" 2>nul
mkdir "%ROOT%\scripts\estimation" 2>nul
mkdir "%ROOT%\scripts\pipeline" 2>nul

echo.
echo ============================================================
echo  Structure creee dans: %ROOT%
echo ============================================================
echo.

:: Afficher l'arborescence
tree "%ROOT%" /F

pause