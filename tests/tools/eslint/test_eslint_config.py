import json
import subprocess
from pathlib import Path
import yaml


project_root = Path(__file__).parent.parent.parent.parent
snapshot_dir = Path(__file__).parent / 'snapshots'


def test_eslint_config_print_config(snapshot):
    process = subprocess.run(
        ['npx', 'eslint', '--print-config', 'eslint.config.mjs'],
        capture_output=True,
        text=True,
        cwd=project_root,
    )
    result = json.loads(process.stdout)
    result['plugins'] = None

    snapshot.snapshot_dir = snapshot_dir
    snapshot.assert_match(yaml.dump(result, indent=4), 'eslint.rules.txt')
