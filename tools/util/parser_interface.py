# =========================================================================

# $$$ MODULE DOCUMENTATION BLOCK

# obs-preproc :: util/parser_interface.py
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

   util.parser_interface.py

DESCRIPTION:

   This module contains methods to perform various tasks which involve
   the parsing of dictionaries, lists, and other Python type
   comprehensions.

AUTHORS: 

   Henry R. Winterbottom; 08 January 2020

ABSTRACT:

   * dict_formatter; This method formats a Python dictionary; all
     UNICODE and data-type conversions are performed within this
     method.

   * true_or_false; This method checks whether an argument is a
     Boolean-type value; if so, this method defines the appropriate
     Python boolean-type; otherwise, this method returns NoneType.

"""

# ----

import collections
import sys

# ----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

# ----


def dict_formatter(in_dict):
    """
    DESCRIPTION:

    This method formats a Python dictionary; all UNICODE and data-type
    conversions are performed within this method.

    INPUT VARIABLES:

    * in_dict; a standalone Python dictionary to be formatted.

    OUTPUT VARIABLES:

    * out_dict; a standalone Python dictionary which has been
      formatted.

    """
    def sorted_by_keys(dct,):
        new_dct = collections.OrderedDict()
        for key, value in sorted(dct.items(), key=lambda key: key):
            if sys.version_info < (3, 0, 0):
                if isinstance(value, unicode):
                    value = value.encode('ascii', 'ignore')
                if isinstance(key, unicode):
                    key = key.encode('ascii', 'ignore')
            if isinstance(value, dict):
                new_dct[key] = sorted_by_keys(value)
            else:
                if sys.version_info < (3, 0, 0):
                    if isinstance(key, unicode):
                        key = key.encode('ascii', 'ignore')
                    if isinstance(value, unicode):
                        value = value.encode('ascii', 'ignore')
                test_value = value
                if isinstance(test_value, bool):
                    if test_value:
                        value = True
                    if not test_value:
                        value = False
                if isinstance(test_value, str):
                    try:
                        dummy = float(test_value)
                        if '.' in test_value:
                            value = float(test_value)
                        else:
                            value = int(test_value)
                    except ValueError:
                        if test_value.lower() == 'none':
                            value = None
                        elif test_value.lower() == 'true':
                            value = True
                        elif test_value.lower() == 'false':
                            value = False
                        else:
                            value = str(test_value)
                new_dct[key] = value
        return new_dct
    out_dict = sorted_by_keys(dct=in_dict)
    return out_dict

# ----


def dict_key_value(dict_in, key, force=False, max_value=False, min_value=False, index_value=None):
    """
    DESCRIPTION:

    This method ingests a Python dictionary and a dictionary key and
    return the value(s) corresponding to the respective dictionary
    key; if the optional variable 'force' is True and the dictionary
    key does not exist within the Python dictionary, the method will
    return NoneType.

    INPUT VARIABLES:

    * dict_in; a Python dictionary to be parsed.

    * key; a Python string indicating the dictionary key within the
      Python dictionary (see above).

    OPTIONAL VARIABLES:

    * force; a Python boolean variable; if True and in the absence of
      the respective dictionary key within the Python dictionary,
      NoneType is returned; otherwise, an EnvironmentError is raised.

    * max_value; a Python boolean variable; if True, and a Python list
      yielded via the Python dictionary key, the maximum value within
      the Python list will be returned; the default value is False.

    * min_value; a Python boolean variable; if True, and a Python list
      yielded via the Python dictionary key, the minimum value within
      the Python list will be returned; the default value is False.

    * index_value; a Python integer defining the index within the
      Python list (as yielded by the Python dictionary key) to return;
      the default value is NoneType.

    OUTPUT VARIABLES:

    * value; a list of values collected from the ingested Python
      dictionary and the respective dictionary key.

    """
    if max_value and min_value:
        msg = ('The user has requested both minimum and maximum list '
               'values. Please check that only one threshold value is '
               'is to be sought from the list. Aborting!!!')
        raise EnvironmentError(msg)
    if index_value is not None:
        if max_value:
            msg = ('The user has selected both a single value (as per '
                   'the specified index) and the maximum list value. '
                   'Please check which criteria to fulfill. Aborting!!!')
            raise EnvironmentError(msg)
        if min_value:
            msg = ('The user has selected both a single value (as per '
                   'the specified index) and the minimum list value. '
                   'Please check which criteria to fulfill. Aborting!!!')
            raise EnvironmentError(msg)
    try:
        value = dict_in[key]
        try:
            in_list = dict_in[key].split(',')
            value = list(string_parser(in_list=in_list))
            if max_value:
                value = max(value)
            if min_value:
                value = min(value)
            if index_value is not None:
                value = value[index_value]
        except AttributeError:
            value = dict_in[key]
    except KeyError:
        if not force:
            msg = ('Key %s could not be found in user provided dictionary. '
                   'Aborting!!!' % key)
            raise EnvironmentError(msg)
        if force:
            value = None
    return value

# ----


def true_or_false(argval):
    """
    DESCRIPTION:

    This method checks whether an argument is a Boolean-type value; if
    so, this method defines the appropriate Python boolean-type;
    otherwise, this method returns NoneType.

    INPUT VARIABLES:

    * argval; a value corresponding to an argument.

    OUTPUT VARIABLES:

    * pytype; a Python boolean-type value if the argument is a boolean
      variable; otherwise, NoneType.

    """
    ua = str(argval).upper()
    if 'TRUE'.startswith(ua):
        pytype = True
    elif 'FALSE'.startswith(ua):
        pytype = False
    else:
        pytype = None
    return pytype

# ----


def unique_list(in_list):
    """
    DESCRIPTION:

    This method ingests a list, possibly with duplicate values, and
    returns a list of only unique values.

    INPUT VARIABLES:

    * in_list; a N-dimensional Python list containing strings.

    OUTPUT VARIABLES:

    * out_list; a Python list containing only uniquely-valued strings.

    """
    out_list = list()
    out_dict = collections.OrderedDict.fromkeys(x for x in in_list if x not
                                                in out_list)
    out_list = list()
    for key in sorted(out_dict.keys()):
        out_list.append(key.replace(' ', ''))
    return out_list
