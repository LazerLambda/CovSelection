
import argparse
import datetime
import os
import logging

parser = argparse.ArgumentParser(
    prog='Experiment Wrapper',
    description='Call the R-experiment script.',
    epilog='')

parser.add_argument(
    '-f',
    '--csv-out',
    type=str,
    required=False,
    default=('csv-out' + str(datetime.datetime.now().strftime("%Y-%m-%d_%H:%M:%S") + '.csv'))
    )
args = parser.parse_args()

target_file: str = args.csv_out
n: str = "100"
dims: str = "100,250,500,750,1000"
graphs: list = ["hub", "cluster", "band", "scale-free"] #, "MB"]
methods: list = ["glasso"]# , "mb"]
selectors: list = ['ebic'] # ['stars', 'ebic']
seeds: list = [51, 23, 123, 42]

total: int = len(dims.split(',')) * len(graphs) * len(methods) * len(selectors) * len(seeds)
counter: int = 0
for seed in seeds:
    for graph in graphs:
        for method in methods:
            for selector in selectors:
                logging.info(f"{counter}/{total}")
                command_str: str = f"Rscript exp_script.R {n} {dims} {graph} {method} {selector} {target_file} {seed}"
                stream = os.popen(command_str)
                output = stream.read()
                counter += 1
