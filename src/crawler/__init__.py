__author__ = 'chris'

import sys
import warnings
from pathlib import Path

currdir = Path(__file__)
sys.path.append(str(currdir.parent.parent))
# warnings.warn(f'Path added: {currdir.parent.parent}')
# print(f'Path added: {currdir.parent.parent}')
