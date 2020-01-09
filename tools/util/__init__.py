# =========================================================================

# $$$ MODULE DOCUMENTATION BLOCK

# obs-preproc :: util/__init__.py
# Copyright (C) 2019 Henry R. Winterbottom

# Email: henry.winterbottom@noaa.gov

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

# =========================================================================

"""
MODULE:   

   util/__init__.py

AUTHOR: 

   Henry R. Winterbottom; 08 January 2020

ABSTRACT:

   This module initializes all modules within package util.

HISTORY:

   2020-01-08: Henry R. Winterbottom -- Initial implementation.  

"""

# ----

import sys

# ----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

# ----

module_list = ['date_interface',
               'parser_interface', ]
if sys.version_info < (3, 0, 0):
    for module in module_list:
        __import__('util.%s' % module)
if sys.version_info >= (3, 0, 0):
    __all__ = module_list
    from util import *
