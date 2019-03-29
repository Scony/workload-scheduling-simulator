import sys
import os
import copy


if len(sys.argv) == 1:
    print('Missing directory param')
    sys.exit(1)


def files_in_path(path):  
    for feile in os.listdir(path):
        if os.path.isfile(os.path.join(path, feile)):
            yield feile


def decode_metadata(string):
    return {x.split(':')[0]: x.split(':')[1] for x in string.split(',')}


def read_results(path):
    results = []
    for feile in files_in_path(path):
        result = decode_metadata(feile)
        with open(os.path.join(path, feile), 'r') as fh:
            result['outcome'] = [float(x) for x in fh.read().split('\n')[0].split()]
            assert len(result['outcome']) == 5
            result['outcome'] = result['outcome'][0]
        results.append(result)
    return results


def make_key(result):
    key = copy.deepcopy(result)
    del(key['algorithm'])
    del(key['outcome'])
    return tuple(sorted(key.items()))


def calculate_best_results(results):
    best_results = {}
    for result in results:
        key = make_key(result)
        if key in best_results:
            best_results[key] = min(result['outcome'], best_results[key])
        else:
            best_results[key] = result['outcome']
    return best_results


def normalize_results(results):
    best_results = calculate_best_results(results)
    normalized_results = copy.deepcopy(results)
    for result in normalized_results:
        key = make_key(result)
        result['outcome'] = result['outcome'] / best_results[key]
    return normalized_results


def result_matches(result, pattern):
    if pattern == {}:
        return True
    else:
        return all(x in result.items() for x in pattern.items())


def print_report_for(results, pattern):
    algorithm_outcomes = {}
    for result in results:
        if result_matches(result, pattern):
            if result['algorithm'] not in algorithm_outcomes:
                algorithm_outcomes[result['algorithm']] = []
            algorithm_outcomes[result['algorithm']].append(result['outcome'])
    for algorithm in algorithm_outcomes:
        outcomes = algorithm_outcomes[algorithm]
        algorithm_outcomes[algorithm] = sum(outcomes) / len(outcomes)
    print('pattern: {}'.format(pattern))
    print('\n'.join(sorted([str((v,k)) for k, v in algorithm_outcomes.items()])))


results_dir = sys.argv[1]
results = read_results(results_dir)
results = normalize_results(results)
print_report_for(results, {})

job_nums = list(set([result['jobs'] for result in results]))
machine_nums = list(set([result['machines'] for result in results]))

for jobs_num in job_nums:
    print()
    print_report_for(results, {'jobs': jobs_num})

for machines_num in machine_nums:
    print()
    print_report_for(results, {'machines': machines_num})
