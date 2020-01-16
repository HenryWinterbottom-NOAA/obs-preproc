#    Copyright (C) 2019 Henry R. Winterbottom

#    Email: Henry.Winterbottom@noaa.gov

#    This file is part of obs-preproc.

#    obs-preproc is free software: you can redistribute it and/or
#    modify it under the terms of the GNU General Public License as
#    published by the Free Software Foundation, either version 3 of
#    the License, or (at your option) any later version.

#    obs-preproc is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with obs-preproc.  If not, see
#    <http://www.gnu.org/licenses/>.

# ----



# ----

import argparse
import os

# ----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

#----

class ObsPreProcVDM(object):
    """ """
    def __init__(self,opts_obj):
        """ """
        self.opts_obj=opts_obj

    def format_vdm(self):
        """ """
        datapath=getattr(self.opts_obj,'datapath')
        self.vdm_dict=dict()
        filenames=os.listdir(datapath)
        for filename in filenames:
            kwargs={'filename':os.path.join(datapath,filename)}
            self.vdm_dict[filename]=self.parse_vdm(**kwargs)

    def parse_vdm(self,filename):
        """ """
        vdm_dict=dict()
        with open(filename,'rt') as f:
            data=f.read()
        print(data)
        
    def run(self):
        """ """
        self.format_vdm()

# ----

class ObsPreProcVDMError(Exception):
    """
    DESCRIPTION:

    This is the base-class for all module raised exceptions.

    INPUT VARIABLES:

    * msg; a Python string to accompany the raised exception.

    """

    def __init__(self, msg):
        """
        DESCRIPTION:

        Creates a new ObsPreProcVDMError object.

        """
        super(ObsPreProcVDMError, self).__init__(msg)

#----

class ObsPreProcVDMOptions(object):
    """
    DESCRIPTION:

    This is the base-class object used to collect command line
    arguments provided by the user.

    """

    def __init__(self):
        """
        DESCRIPTION:

        Creates a new ObsPreProcVDMOptions object.

        """
        self.parser = argparse.ArgumentParser()
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument('-c', '--cycle', help='The forecast cycle timestamp; '
                                 'formatted as (assuming UNIX convention) %Y-%m-%d_%H:%M:%S.', default=None)
        self.parser.add_argument('-d', '--datapath', help='The path to the vortex data message(s) '
                                 '(VDM) for the respective forecast cycle.', default=None)
        self.opts_obj = lambda: None

    def run(self):
        """
        DESCRIPTION:

        This method collects the user-specified command-line
        arguments; the available command line arguments are as
        follows:

        -c; The forecast cycle timestamp; formatted as (assuming UNIX
            convention) %Y-%m-%d_%H:%M:%S.

        -d; The path to the vortex data message(s) (VDM) for the
            respective forecast cycle.

        OUTPUT VARIABLES:

        * opts_obj; a Python object containing the user command line
          options.

        """
        opts_obj = self.opts_obj
        args_list = ['cycle', 'datapath']
        args = self.parser.parse_args()
        for item in args_list:
            value = getattr(args, item)
            if value is None:
                msg = ('The argument %s cannot be NoneType. Aborting!!!' % item)
                raise ObsPreProcVDMError(msg=msg)
            else:
                setattr(opts_obj, item, value)
        return opts_obj

#----

def main():
    """ 
    DESCRIPTION:

    This is the driver-level method to invoke the tasks within this
    script.

    """
    options = ObsPreProcVDMOptions()
    opts_obj = options.run()
    formatvdm = ObsPreProcVDM(opts_obj=opts_obj)
    formatvdm.run()
    
# ----


if __name__ == '__main__':
    main()
