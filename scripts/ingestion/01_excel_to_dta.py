# ============================================================
# 01_EXCEL_TO_DTA.PY
# Conversion Excel → Stata avec gestion incrémentale
# ============================================================

import pandas as pd
import os
import sys
import hashlib
import csv
from datetime import datetime

def get_paths(project_root):
    return {
        "raw_excel": os.path.join(project_root, "data", "raw", "excel"),
        "processed_dta": os.path.join(project_root, "data", "processed", "dta"),
        "registry_file": os.path.join(project_root, "data", "processed", "registry", "processed_index.csv"),
    }

def get_file_hash(filepath):
    if not os.path.exists(filepath):
        return None
    hash_md5 = hashlib.md5()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()

def read_registry(registry_file):
    if not os.path.exists(registry_file):
        return {}
    registry = {}
    try:
        with open(registry_file, "r", newline="", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            for row in reader:
                if row.get("filename"):
                    registry[row["filename"]] = row
    except Exception:
        pass
    return registry

def save_registry(registry, registry_file):
    fieldnames = [
        "filename", "source_file", "hash_md5", "processed_at",
        "step_completed", "status", "mois", "annee", "n_rows", "n_cols"
    ]
    os.makedirs(os.path.dirname(registry_file), exist_ok=True)
    with open(registry_file, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for entry in registry.values():
            row = {k: entry.get(k, "") for k in fieldnames}
            writer.writerow(row)

def update_registry(registry, registry_file, filename, source_file, 
                    step_completed, status, n_rows=None, n_cols=None, paths=None):
    name = os.path.splitext(filename)[0]
    mois, annee = "", ""
    if len(name) >= 7 and name[2] == "_":
        try:
            mois = int(name[0:2])
            annee = int(name[3:7])
        except ValueError:
            pass
    
    file_hash = ""
    if paths:
        source_path = os.path.join(paths["raw_excel"], source_file)
        file_hash = get_file_hash(source_path) or ""
    
    registry[filename] = {
        "filename": filename,
        "source_file": source_file,
        "hash_md5": file_hash,
        "processed_at": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        "step_completed": step_completed,
        "status": status,
        "mois": mois,
        "annee": annee,
        "n_rows": n_rows or "",
        "n_cols": n_cols or "",
    }
    save_registry(registry, registry_file)
    return registry

def needs_processing(source_file, registry, paths):
    source_path = os.path.join(paths["raw_excel"], source_file)
    current_hash = get_file_hash(source_path)
    dta_filename = os.path.splitext(source_file)[0] + ".dta"
    
    if dta_filename not in registry:
        return True, "new_file"
    
    entry = registry[dta_filename]
    if entry.get("hash_md5") != current_hash:
        return True, "modified"
    if entry.get("status") != "completed":
        return True, "incomplete"
    
    return False, "up_to_date"

def convert_excel_to_dta(project_root, force=False):
    paths = get_paths(project_root)
    
    print("\n" + "=" * 60)
    print("ÉTAPE 1: CONVERSION EXCEL → STATA")
    print("=" * 60)
    
    os.makedirs(paths["raw_excel"], exist_ok=True)
    os.makedirs(paths["processed_dta"], exist_ok=True)
    
    registry = read_registry(paths["registry_file"])
    
    excel_folder = paths["raw_excel"]
    if not os.path.exists(excel_folder):
        print("Dossier Excel non trouvé")
        return {"success": [], "errors": []}
    
    all_files = [f for f in os.listdir(excel_folder) 
                 if f.lower().endswith(".xlsx") and not f.startswith("~$")]
    
    if force:
        files_to_process = all_files
    else:
        files_to_process = []
        for f in all_files:
            needs, _ = needs_processing(f, registry, paths)
            if needs:
                files_to_process.append(f)
    
    print(f"Fichiers à traiter: {len(files_to_process)}")
    
    if len(files_to_process) == 0:
        print("\nAucun fichier à traiter")
        return {"success": [], "errors": []}
    
    results = {"success": [], "errors": []}
    
    for i, filename in enumerate(files_to_process, 1):
        print(f"\n[{i}/{len(files_to_process)}] → {filename}")
        
        try:
            excel_path = os.path.join(paths["raw_excel"], filename)
            
            print("    Lecture des feuilles...", end=" ", flush=True)
            sheets_dict = pd.read_excel(excel_path, sheet_name=None)
            print(f"{len(sheets_dict)} feuilles")
            
            print("    Concaténation...", end=" ", flush=True)
            df_all = pd.concat(sheets_dict.values(), ignore_index=True)
            print(f"{len(df_all):,} lignes, {len(df_all.columns)} colonnes")
            
            output_filename = os.path.splitext(filename)[0] + ".dta"
            output_path = os.path.join(paths["processed_dta"], output_filename)
            
            print("    Sauvegarde Stata...", end=" ", flush=True)
            df_all.to_stata(output_path, write_index=False, version=118)
            print("OK")
            
            registry = update_registry(
                registry=registry,
                registry_file=paths["registry_file"],
                filename=output_filename,
                source_file=filename,
                step_completed="01_excel_to_dta",
                status="in_progress",
                n_rows=len(df_all),
                n_cols=len(df_all.columns),
                paths=paths
            )
            
            print(f"    ✓ Sauvegardé: {output_filename}")
            results["success"].append(filename)
            
        except Exception as e:
            print(f"    ✖ ERREUR: {e}")
            results["errors"].append(filename)
    
    print("\n" + "-" * 40)
    print(f"Résumé: {len(results['success'])} succès, {len(results['errors'])} erreurs")
    
    return results

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python 01_excel_to_dta.py <project_root> [--force]")
        sys.exit(1)
    
    project_root = sys.argv[1]
    force = "--force" in sys.argv
    
    if not os.path.exists(project_root):
        print(f"ERREUR: Dossier non trouvé: {project_root}")
        sys.exit(1)
    
    results = convert_excel_to_dta(project_root, force=force)
    sys.exit(1 if results["errors"] else 0)