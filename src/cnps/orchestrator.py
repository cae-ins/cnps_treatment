"""
Orchestrateur en sous-processus deprecie, independant de pipeline.py.

Decouvre les etapes 01-12 en scannant src/cnps/ (fichiers NN_*.py tries
par prefixe numerique) et les execute chacune comme un sous-processus
Python separe (sys.executable src/cnps/NN_nom.py), pour un isolement
complet des logs et des erreurs entre etapes.

N'importe rien de cnps.pipeline : c'est un point d'entree alternatif,
pas un remplacement de `cnps run` / `python run.py run`.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
import time
import warnings
from pathlib import Path

from loguru import logger

from cnps.config import load_config

_DEPRECATION_MESSAGE = (
    "cnps.orchestrator est deprecie et sera supprime dans une version future; "
    "utiliser python run.py run, qui s'appuie sur cnps.pipeline."
)
warnings.warn(_DEPRECATION_MESSAGE, DeprecationWarning, stacklevel=2)

_STAGE_DIR = Path(__file__).parent
# NN_*.py, avec un suffixe alphabetique optionnel pour les sous-etapes
# inserees entre deux numeros existants (ex. 07b_ entre 07_ et 08_).
_STAGE_RE = re.compile(r"^(\d{2}[a-z]?)_.*\.py$")


def discover_stages() -> list[tuple[str, Path]]:
    """
    Retourne [(numero, chemin), ...] tries, pour les fichiers NN_*.py.

    Le tri est lexicographique sur le numero, ce qui place naturellement une
    sous-etape juste apres son etape parente ("07" < "07b" < "08").
    """
    found = []
    for f in _STAGE_DIR.iterdir():
        m = _STAGE_RE.match(f.name)
        if m:
            found.append((m.group(1), f))
    return sorted(found, key=lambda t: t[0])


def run_orchestrated(
    from_num: str,
    to_num: str,
    verbose: bool = False,
    include_hj_estimated: bool = False,
) -> list[dict]:
    logger.warning(_DEPRECATION_MESSAGE)
    stages = [(n, p) for n, p in discover_stages() if from_num <= n <= to_num]
    results = []

    for num, script in stages:
        cmd = [sys.executable, str(script)] + (["--verbose"] if verbose else [])
        # --include-hj-estimated ne concerne que l'etape 03 (03_nettoyage_donnees.py) :
        # les autres etapes n'exposent pas ce flag dans leur propre argparse et
        # echoueraient si on le leur passait aussi.
        if num == "03" and include_hj_estimated:
            cmd.append("--include-hj-estimated")

        logger.info("=" * 60)
        logger.info("SOUS-PROCESSUS {} : {}", num, script.name)
        logger.info("=" * 60)

        t0 = time.perf_counter()
        proc = subprocess.run(cmd)
        dt = time.perf_counter() - t0
        status = "ok" if proc.returncode == 0 else "error"
        results.append(
            {
                "num": num,
                "name": script.name,
                "status": status,
                "duration": dt,
                "returncode": proc.returncode,
            }
        )

        logger.info(
            "{} : {} en {:.1f}s (code retour {})",
            script.name,
            status.upper(),
            dt,
            proc.returncode,
        )

        # Meme regle que pipeline.py::run_pipeline : toute erreur arrete le
        # pipeline. Une validation invalide interdit donc toujours l'export.
        if status == "error":
            logger.error("Orchestrateur arrete a l'etape '{}'", script.name)
            break

    return results


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Execute le pipeline CNPS en sous-processus isoles, etape par etape."
    )
    parser.add_argument("--from", dest="from_num", default="01")
    parser.add_argument("--to", dest="to_num", default="12")
    parser.add_argument(
        "--include-hj-estimated",
        "--include_hj_estimated",
        dest="include_hj_estimated",
        action="store_true",
        help="ANALYSE DE SENSIBILITE UNIQUEMENT, relaye a l'etape 03 seulement : "
        "n'exclut aucun type d'employe, horaires compris. Par defaut, seuls "
        "les horaires sont exclus (69%% de DUREE_TRAVAILLEE incoherente) ; "
        "les journaliers sont conserves dans les deux cas. Les deux graphies "
        "(tiret/underscore) sont acceptees.",
    )
    parser.add_argument("--verbose", "-v", action="store_true")
    args = parser.parse_args()

    cfg = load_config()
    logger.remove()
    logger.add(
        sys.stderr,
        level="DEBUG" if args.verbose else cfg.logging.level,
        colorize=True,
        format="<green>{time:HH:mm:ss}</green> | <level>{level:<8}</level> | {message}",
    )
    logger.add(
        str(cfg.paths.logs / "orchestrator.log"),
        level="DEBUG" if args.verbose else cfg.logging.level,
        rotation=cfg.logging.rotation,
        retention=cfg.logging.retention,
        encoding="utf-8",
    )

    results = run_orchestrated(
        args.from_num.zfill(2),
        args.to_num.zfill(2),
        verbose=args.verbose,
        include_hj_estimated=args.include_hj_estimated,
    )

    logger.info("=" * 60)
    logger.info("RESUME")
    for r in results:
        logger.info(
            "  {} {:<30} {:<8} {:>8.1f}s",
            r["num"],
            r["name"],
            r["status"].upper(),
            r["duration"],
        )
    success = all(r["status"] == "ok" for r in results)
    logger.info(
        "Orchestrateur {} : {:.1f}s au total",
        "SUCCES" if success else "ECHEC",
        sum(r["duration"] for r in results),
    )
    sys.exit(0 if success else 1)
