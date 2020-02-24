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

"""
SCRIPT:   

   sonde_archive.py

AUTHOR: 

   Henry R. Winterbottom; 08 January 2020

ABSTRACT:

   (1) FormatSonde: This is the base-class object to format TEMP-DROP
       messages (observations) in accordance with the expectations of
       the tempdrop_sonde executable.

   (2) SondeArchive: This is the base-class object for creating the
       AOML/HRD TEMP-DROP formatted observation archives as function
       of the user specified times.

   (3) SondeArchiveError: This is the base-class for all module raised
       exceptions; it is a sub-class of Exception.

   (4) SondeArchiveOptions: This is the base-class object used to
       collect command line arguments provided by the user.

   * main; This is the driver-level method to invoke the tasks within
     this script.

USAGE:

   python sonde_archive.py 
     
     --conffile <the UNIX conf-formatted file containing the arguments
                 to be used to construct the sonde archive>

HISTORY:

   2020-01-08: Henry R. Winterbottom -- Initial implementation.

"""

import util
import argparse
import contextlib
import logging
import os
import string
import sys
import tarfile
if sys.version_info >= (3, 0, 0):
    import configparser as ConfigParser
else:
    import ConfigParser


# ----

class FormatSonde(object):
    """ 
    DESCRIPTION:

    This is the base-class object to format TEMP-DROP messages
    (observations) in accordance with the expectations of the
    tempdrop_sonde executable.

    """

    def __init__(self):
        """ 
        DESCRIPTION:

        Creates a new FormatSonde object.

        """
        
    def formatsondes(self, input_file_dict):
        """
        DESCRIPTION:

        This method formats TEMP-DROP messages (observations) in
        accordance with the expectations of the tempdrop_sonde
        executable.

        INPUT VARIABLES:

        * input_file_dict; a Python dictionary containing key and
          value pairs for the Julian date and corresponding TEMP-DROP
          message file.

        OUTPUT VARIABLES:

        * output_file_dict; a Python dictionary containing key and
          value pairs for the Julian date and the corresponding
          modified TEMP-DROP message file.

        """
        input_files = input_file_dict.values()
        output_files = list()
        srchstrs = ['REL', 'SPG', 'SPL']
        excldstrs = ['62626', 'REL', 'SPG', 'SPL']
        for infile in input_files:
            if os.path.exists(infile):
                with open(infile, 'rb') as inf:
                    data = inf.read()
                outfile = ('%s.mod' % infile)
                datan = list()
                data.replace('\r', '')
                data = data.split('\n')
                data = filter(None, data)
                for item in data:
                    item = self.stripmeta(instr=item)
                    datan.append(item)
                data = datan
                with open(outfile, 'w') as outf:
                    for item in data:
                        if any(s in item for s in excldstrs):
                            pass
                        else:
                            outf.write('%s\n' % item)
                    outdata = list()
                    for (i, item) in enumerate(data):
                        for srchstr in srchstrs:
                            if srchstr in item:
                                try:
                                    nstr = data[i]+data[i+1]
                                    nstr = self.stripmeta(instr=nstr)
                                    indx = nstr.index(srchstr)
                                    sstr = nstr[indx:indx+23]
                                    sstr = self.stripmeta(instr=sstr)
                                    outf.write('%s\n' % sstr)
                                except IndexError:
                                    pass
                output_files.append(outfile)
        return output_files

    def stripmeta(self, instr):
        """
        DESCRIPTION:

        This method stripts meta-characters and carriage returns from
        an input string.  

        INPUT VARIABLES:

        * instr; a Python string possibly containing meta-characters.

        OUTPUT VARIABLES:

        * outstr; a Python string stripped of meta-characters and
          carriage returns.

        """
        for c in (string.ascii_lowercase+string.ascii_uppercase):
            chkstr = '^%s' % c
            outstr = instr.replace(chkstr, '')
            instr = outstr
        outstr = outstr.replace('\r', '')
        return outstr


# ----

class SondeArchive(object):
    """
    DESCRIPTION:

    This is the base-class object for creating the AOML/HRD TEMP-DROP
    formatted observation archives as function of the user specified
    times.

    INPUT VARIABLES:

    * opts_obj; a Python object containing the user command line
      options.

    """

    def __init__(self, opts_obj):
        """ 
        DESCRIPTION:

        Creates a new SondeArchive object.

        """
        self.opts_obj = opts_obj
        self.config = ConfigParser.ConfigParser()
        self.config.optionxform = str
        self.logger = SondeArchiveLog()
        self.format_sonde = FormatSonde()
        
    def archive_times(self):
        """
        DESCRIPTION:

        This method constructs a Python list of archive times in
        accordance with the user specifications.

        """
        self.archive_times_list = list()
        try:
            start_time = self.conf_dict['start_time']
        except Exception as msg:
            raise SondeArchiveError(msg=msg)
        try:
            stop_time = self.conf_dict['stop_time']
        except Exception as msg:
            raise SondeArchiveError(msg=msg)
        try:
            interval_seconds = self.conf_dict['interval_seconds']
        except Exception as msg:
            raise SondeArchiveError(msg=msg)
        kwargs = {'datestr': start_time, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                  'out_frmttyp': '%Y-%m-%d_%H:%M:%S'}
        datestr = util.date_interface.datestrfrmt(**kwargs)
        self.archive_times_list.append(datestr)
        while datestr != stop_time:
            kwargs = {'datestr': datestr, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                      'out_frmttyp': '%Y-%m-%d_%H:%M:%S', 'offset_seconds':
                      interval_seconds}
            datestr = util.date_interface.datestrfrmt(**kwargs)
            self.archive_times_list.append(datestr)

    def build_archive(self):
        """
        DESCRIPTION:

        This method constructs a tarball file for the corresponding
        user specified times which contains the files within the
        respective date/time ranges.

        """
        try:
            time_window_seconds = self.conf_dict['time_window_seconds']
        except Exception as msg:
            raise SondeArchiveError(msg=msg)
        for archive_time in self.archive_times_list:
            kwargs = {'datestr': archive_time, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                      'out_frmttyp': '%Y-%m-%d_%H:%M:%S', 'offset_seconds':
                      -1.0*time_window_seconds}
            datestr = util.date_interface.datestrfrmt(**kwargs)
            kwargs = {'datestr': datestr}
            date_comps_obj = util.date_interface.datestrcomps(**kwargs)
            julian_start = date_comps_obj.julian_day
            kwargs = {'datestr': archive_time, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                      'out_frmttyp': '%Y-%m-%d_%H:%M:%S', 'offset_seconds':
                      time_window_seconds}
            datestr = util.date_interface.datestrfrmt(**kwargs)
            kwargs = {'datestr': datestr}
            date_comps_obj = util.date_interface.datestrcomps(**kwargs)
            julian_stop = date_comps_obj.julian_day
            kwargs = {'julian_start': julian_start, 'julian_stop': julian_stop}
            input_file_dict = self.filter_input_files(**kwargs)
            kwargs = {'input_file_dict': input_file_dict}
            output_files = self.format_sonde.formatsondes(**kwargs)
            kwargs = {'output_files': output_files,'archive_time': archive_time}
            self.create_tarball(**kwargs)

    def create_tarball(self, output_files, archive_time):
        """
        DESCRIPTION:

        This method creates a tarball file for the corresponding time
        (archive_time) containing the files specified by the key and
        value pairs within the input Python dictionary
        (input_file_dict).

        INPUT VARIABLES:

        * output_file; a Python list containing only the output files,
          within the user specified Julian date range, to compose the
          respective tarball file.

        * archive_time; a Python string specifying the archive time
          for the tarball file.

        """
        archive_filename = self.conf_dict['archive_filename']
        try:
            kwargs={'datestr':archive_time,'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                    'out_frmttyp':'%Y%m%d%H'}
            analdate=util.date_interface.datestrfrmt(**kwargs)
            output_path = os.path.join(self.conf_dict['output_path'],analdate)
            if not os.path.exists(output_path):
                os.makedirs(output_path)
            kwargs = {'datestr': archive_time, 'in_frmttyp': '%Y-%m-%d_%H:%M:%S',
                      'out_frmttyp': archive_filename}
            archive_filename = util.date_interface.datestrfrmt(**kwargs)
            archive_filename = os.path.join(output_path, archive_filename)
            msg = ('Creating archive file %s.' % archive_filename)
            self.logger.info(msg=msg)
            with contextlib.closing(tarfile.open(archive_filename, 'w')) as tar:
                for filename in output_files:
                    arcname = os.path.basename(filename)                    
                    msg = ('Adding file %s to archive file %s.' % (filename,
                                                                   archive_filename))
                    self.logger.info(msg=msg)
                    tar.add(filename, arcname=arcname, recursive=False)
        except Exception as err:
            msg=('Tarball %s failed with error %s.'%(archive_filename,err))
            raise SondeArchiveError(msg=msg)

    def filter_input_files(self, julian_start, julian_stop):
        """
        DESCRIPTION:

        This function filters the base-class attribute input_file_dict
        (created via method input_files) in accordance with the start
        and stop Julian dates specified by the user.

        INPUT VARIABLES:

        * julian_start; a Python float value specifying the starting
          Julian date.

        * julian_stop; a Python float value specifying the ending
          Julian date.

        OUTPUT VARIABLES:

        * input_file_dict; a Python dictionary containing only the
          base-class attribute input_file_dict key and value pairs
          that are within the user specified Julian date range.

        """
        input_file_dict = {key: value for (key, value) in self.input_file_dict.items()
                           if (key >= julian_start and key <= julian_stop)}
        return input_file_dict

    def input_files(self):
        """
        DESCRIPTION:

        This method constructs a Python dictionary containing key and
        value pairs for the Julian date corresponding to the
        observation timestamp (key) and the filename path (value).

        """
        try:
            input_path = self.conf_dict['input_path']
            filenames = os.listdir(input_path)
        except Exception as msg:
            raise SondeArchiveError(msg=msg)
        self.input_file_dict = dict()
        for filename in filenames:
            try:
                timestamp = filename.split('.')[0]
                kwargs = {'datestr': timestamp, 'in_frmttyp': '%Y%m%d%H%M',
                          'out_frmttyp': '%Y-%m-%d_%H:%M:%S'}
                datestr = util.date_interface.datestrfrmt(**kwargs)
                kwargs = {'datestr': datestr}
                date_comps_obj = util.date_interface.datestrcomps(**kwargs)
                self.input_file_dict[date_comps_obj.julian_day] =\
                    os.path.join(input_path, filename)
            except Exception:
                pass

    def read_conf(self):
        """
        DESCRIPTION:

        This method reads the user-specified conf-formatted file and
        defines the base-class object (self.config).

        """
        conffile = getattr(self.opts_obj, 'conffile')
        self.config.readfp(open(conffile))
        for section in self.config.sections():
            in_dict = dict(self.config.items(section))
            kwargs = {'in_dict': in_dict}
            self.conf_dict = util.parser_interface.dict_formatter(**kwargs)

    def run(self):
        """
        DESCRIPTION:

        This method performs the following tasks:

        (1) Reads the user-specified UNIX conf-formatted file.

        (2) Defines a Python dictionary of input file paths and their
            respective Julian valid dates.

        (3) Defines the archive times in accordance with the user
            specifications.

        (4) Constructs the tarball archives for each user specified
            time.

        """
        self.read_conf()
        self.input_files()
        self.archive_times()
        self.build_archive()

# ----


class SondeArchiveError(Exception):
    """
    DESCRIPTION:

    This is the base-class for all module raised exceptions; it is a
    sub-class of Exception.

    INPUT VARIABLES:

    * msg; a Python string to accompany the raised exception.

    """

    def __init__(self, msg):
        """
        DESCRIPTION:

        Creates a new SondeArchiveError object.

        """
        super(SondeArchiveError, self).__init__(msg)

# ----


class SondeArchiveLog(object):
    """
    DESCRIPTION:

    This is the base-class object for all Log instances.

    """

    def __init__(self):
        """
        DESCRIPTION:

        Creates a new SondeArchiveLog object.

        """
        self.exception = SondeArchiveError

    def info(self, msg):
        """
        DESCRIPTION:

        This method writes a message to the base-class Python logger via
        the INFO level.

        INPUT VARIABLES:

        * msg; a Python string containing the user specified logger
          message.

        """
        self.log = self.setup(info=True)
        self.log.info(msg)

    def setup(self, info=False):
        """
        DESCRIPTION:

        This method defines the Python logging object.

        OUTPUT VARIABLES:

        * log; a Python object containing the user specifed/define
          Python logging object.

        """
        if info:
            format = '%(levelname)s :: %(asctime)s : %(message)s'
        if not info:
            format = '%(levelname)s :: %(asctime)s : %(pathname)s (%(lineno)s)'\
                '; %(message)s'
        datefmt = '%Y-%m-%d %H:%M:%S'
        log = logging
        log.basicConfig(stream=sys.stdout, level=logging.INFO, format=format,
                        datefmt=datefmt)
        return log

# ----


class SondeArchiveOptions(object):
    """
    DESCRIPTION:

    This is the base-class object used to collect command line
    arguments provided by the user.

    """

    def __init__(self):
        """
        DESCRIPTION:

        Creates a new SondeArchiveOptions object.

        """
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument('-cf', '--conffile', help='The UNIX conf-formatted '
                                 'file containing the arguments to be used to construct the '
                                 'sonde archive.', default=None)
        self.opts_obj = lambda: None

    def run(self):
        """
        DESCRIPTION:

        This method collects the user-specified command-line
        arguments; the available command line arguments are as
        follows:

        -cf; The UNIX conf-formatted file containing the arguments to
             be used to construct the sonde archive.

        OUTPUT VARIABLES:

        * opts_obj; a Python object containing the user command line
          options.

        """
        opts_obj = self.opts_obj
        args_list = ['conffile']
        args = self.parser.parse_args()
        for item in args_list:
            value = getattr(args, item)
            if value is None:
                msg = ('The argument %s cannot be NoneType. Aborting!!!' % item)
                raise SondeArchiveError(msg=msg)
            else:
                setattr(opts_obj, item, value)
        return opts_obj

# ----


def main():
    """
    DESCRIPTION:

    This is the driver-level method to invoke the tasks within this
    script.

    """
    options = SondeArchiveOptions()
    opts_obj = options.run()
    sondearchive = SondeArchive(opts_obj=opts_obj)
    sondearchive.run()

# ----


if __name__ == '__main__':
    main()
