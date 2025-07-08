import json
import subprocess
from pathlib import Path
import yaml


project_root = Path(__file__).parent.parent.parent.parent
snapshot_dir = Path(__file__).parent / 'snapshots'


def test_ruff_config_print_config(snapshot):
    process = subprocess.run(
        ['ruff', 'config', '--output-format', 'json'],
        capture_output=True,
        text=True,
        cwd=project_root,
    )
    result = json.loads(process.stdout)

    snapshot.snapshot_dir = snapshot_dir
    snapshot.assert_match(yaml.dump(result, indent=4, sort_keys=True), 'ruff.rules.txt')
