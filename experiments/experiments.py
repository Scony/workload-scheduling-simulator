import os
import subprocess

import pytest


MAIN_WS_DIR = 'workspaces'



def collect_instances(paths):
    instances = []
    for path in paths:
        for root, _, files in os.walk(path):
            for feile in files:
                instance_name = '{}_{}'.format(os.path.basename(root), feile)
                instance_path = os.path.join(root, feile)
                instances.append((instance_name, instance_path))
    return instances


@pytest.fixture(scope='function')
def workspace(request):
    try:
        os.mkdir(MAIN_WS_DIR)
    except FileExistsError:
        pass
    concrete_workspace = os.path.join(MAIN_WS_DIR, request.node.originalname)
    try:
        os.mkdir(concrete_workspace)
    except FileExistsError:
        pass
    yield concrete_workspace


@pytest.mark.parametrize('instance_name, instance_path', collect_instances(['../instances/random/333']))
def test_various_sjs(workspace, instance_name, instance_path):
    opts = [
        ('--algorithm', 'sjlo'),
        ('--machines', '100'),
        ('--costfun', 'flow'),
        ('--output', 'jcosts'),
    ]
    metadata = [(k[2:], v) for k, v in opts]
    metadata += [
        ('jobs', instance_name.split('_')[0]),
        ('instance', instance_name),
    ]
    output_file_name = ','.join(['{}:{}'.format(k, v) for k, v in metadata])
    output_file_path = os.path.join(workspace, output_file_name)
    if os.path.isfile(output_file_path) and os.path.getsize(output_file_path) > 0:
        pass
    else:
        completed_process = subprocess.run(' '.join([
            'cat',
            instance_path,
            '|',
            '../bin/simulator',
            'online',
            ' '.join(['{} {}'.format(k, v) for k, v in opts]),
            '|',
            '../bin/stat-desc',
            '> {}'.format(output_file_path),
        ]), shell=True)
        assert completed_process.returncode == 0
