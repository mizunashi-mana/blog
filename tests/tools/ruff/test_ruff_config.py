import subprocess
from pathlib import Path

project_root = Path(__file__).parent.parent.parent.parent
snapshot_dir = Path(__file__).parent / 'snapshots'


def test_ruff_config_print_config(snapshot):
    process = subprocess.run(
        ['ruff', 'check', '--show-settings', 'pelicanconf.py'],
        capture_output=True,
        text=True,
        cwd=project_root,
    )

    result = process.stdout.replace(str(project_root), '.')

    snapshot.snapshot_dir = snapshot_dir
    snapshot.assert_match(result, 'ruff.rules.txt')
