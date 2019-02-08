__author__ = 'chris'

import sys
from pathlib import Path

currdir = Path(__file__)
sys.path.append(str(currdir.parent.parent))
print('##', currdir.parent.parent)
