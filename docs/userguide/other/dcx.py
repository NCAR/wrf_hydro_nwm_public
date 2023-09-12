# !/usr/bin/env python
# encoding: utf-8

# #### THIS GETS EXECUTED VIA GEN FILE #######
# #lns=open(__file__).readlines()
# #list(gen_head(lns))
# def gen_head(lns,**kw):
#    dl = list(rindices('^"""', lns))
#    yield from lns[dl[0]+1:dl[1]]
#    yield from lns[dl[-2]+1:dl[-1]]
# def gen_head
# #list(gen_api(lns))
# def gen_api(lns,**kw):
#    yield from doc_parts(lns, signature='py', prefix='dcx.')
# def gen_api
# #### THIS GETS EXECUTED VIA GEN FILE #######

# pylama: ignore=E402,E722,C901,W605,E101,E501,W191

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from collections import OrderedDict, defaultdict
from hashlib import sha1 as sha
from binascii import b2a_base64
import pyx
import stpl
from docutils.core import publish_file, publish_string
from argparse import Namespace
from types import GeneratorType
from itertools import tee
from functools import lru_cache, wraps, partial, reduce
from urllib import request
from threading import RLock
import tempfile
import subprocess as sp
import atexit
import contextlib
import shutil
import os, os.path
import io
import re
import sys
import codecs
import numpy as np
from rstdoc import __version__

try:
    import txdir
except:
    txdir = None

try:
    from rstdoc.retable import title_some
except:
    title_some = """=-^"'`._~+:;,"""

"""
.. _`rstdcx`:

rstdcx
======

restructuredText sources are split into two types of files:
main files considered by Sphinx, and included files.
Which of ``.rest``  or ``.rst`` is main or included is determined
by ``source_suffix`` in a ``<root>/conf.py``
or opposite to the extension of the included ``_links_sphinx.r?st`` file:

- if you have ``.. include:: /_links_sphinx.rest``,
  then the main file extension is ``.rst``

``rstdoc`` creates documentation (PDF, HTML, DOCX)
from restructuredText (``.rst``, ``.rest``) using either

- `Pandoc <https://pandoc.org>`__
- `Sphinx <http://www.sphinx-doc.org>`__
- Docutils
  `configurable <http://docutils.sourceforge.net/docs/user/config.html>`__

``rstdoc`` and ``rstdcx`` command line tools call ``dcx.py``.
which

- creates ``.tags`` to jump around with the editor

- handles `.stpl <https://bottlepy.org/docs/dev/stpl.html>`__ files

- processes ``gen`` files (see examples produced by --rest)

- creates links files (``_links_docx.r?st``, ``_links_sphinx.r?st``, ...)

- forwards known files to either Pandoc, Sphinx or Docutils

See example at the end of ``dcx.py``.
It is supposed to be used with a build tool.
``make`` and ``waf`` examples are included.

- Initialize example tree (add ``--rstrest`` to make ``.rst`` main and ``.rest`` included files):

  $ ./dcx.py --rest repo #repo/doc/{sy,ra,sr,dd,tp}.rest files OR
  $ ./dcx.py --stpl repo #repo/doc/{sy,ra,sr,dd,tp}.rest.stpl files
  $ ./dcx.py --ipdt repo #repo/pdt/AAA/{i,p,d,t}.rest.stpl files
  $ ./dcx.py --over repo #.rest all over

- Only create .tags and ``_links_xxx.r?st``::

  $ cd repo
  $ rstdoc

- Create the docs (and .tags and ``_links_xxx.r?st``) with **make**::

  $ make html #OR
  $ make epub #OR
  $ make latex #OR
  $ make docx #OR
  $ make pdf

  The latter two are done by Pandoc, the others by Sphinx.

- Create the docs (and .tags and ``_links_xxx.r?st``) with
  `waf <https://github.com/waf-project/waf>`__:

  Instead of using ``make`` one can load ``dcx.py`` (``rstdoc.dcx``) in
  `waf <https://github.com/waf-project/waf>`__.
  ``waf`` also considers all recursively included files,
  such that a change in any of them results in a rebuild.
  All files can have an additional ``.stpl`` extension to use
  `SimpleTemplate <https://bottlepy.org/docs/dev/stpl.html>`__.

  $ waf configure #also copies the latest version of waf in here
  $ waf --docs docx,sphinx_html,rst_odt
  $ #or you provide --docs during configure to always compile the docs

  - ``rst_xxx``: via
    `rst2xxx.py <http://docutils.sourceforge.net/docs/user/tools.html>`__
  - ``sphinx_xxx``: via `Sphinx <http://www.sphinx-doc.org>`__ and
  - just ``xxx``: via `Pandoc <https://pandoc.org>`__.


The following image language files should be parallel to the ``.r?st`` files.
They are automatically converted to ``.png``
and placed into ``./_images`` or ``<updir>/_images`` or else parallel to the file.

- ``.tikz`` or ``.tikz.stpl``.
  This needs LaTex.

- `.svg <http://svgpocketguide.com/book/>`__ or ``.svg.stpl``

- ``.dot`` or ``.dot.stpl``

  This needs `graphviz <https://graphviz.gitlab.io/gallery/>`__.

- `.uml <http://plantuml.com/command-line>`__ or ``.uml.stpl``

  This needs `plantuml <http://plantuml.com/command-line>`__ .
  Provide either

  - ``plantuml.bat`` with e.g. ``java -jar "%~dp0plantuml.jar" %*``  or
  - ``plantuml`` sh script with
    ``java -jar `dirname $BASH_SOURCE`/plantuml.jar "$@"``

- ``.eps`` or ``.eps.stpl`` embedded postscript files.

  This needs `inkscape <https://inkscape.org/en/>`__.

- ``.pyg`` contains python code that produces a graphic.
  If the python code defines a ``to_svg`` or a ``save_to_png`` function,
  then that is used, to create a png.
  Else the following is tried

  - ``pyx.canvas.canvas`` from the
    `pyx <http://pyx.sourceforge.net/manual/graphics.html>`__ library or
  - ``cairocffi.Surface`` from
    `cairocffi <https://cairocffi.readthedocs.io/en/stable/overview.html>`__
  - `matplotlib <https://matplotlib.org>`__.
    If ``matplotlib.pyplot.get_fignums()>1``
    the figures result in ``<name><fignum>.png``

  The same code or the file names can be used in a ``.r?st.stpl`` file
  with ``pngembed()`` or ``dcx.svgembed()`` to embed in html output.

  ::

     {{!svgembed("egpyx.pyg",outinfo)}}
     <%
     ansvg=svgembed('''
     from svgwrite import cm, mm, drawing
     d=drawing.Drawing(viewBox=('0 0 300 300'))
     d.add(d.circle(center=(2*cm, 2*cm), r='1cm', stroke='blue', stroke_width=9))
     '''.splitlines(),outinfo)
     %>
     {{!ansvg}}


Conventions
-----------

Files

  - main files and included files: ``.rest``, ``.rst`` or vice versa.
    ``.txt`` are for literally included files (use :literal: option).
  - templates separately rendered : ``*.rest.stpl`` and ``*.rst.stpl``
    template included: ``*.rst.tpl``
    Template lookup is done in
    ``.`` and ``..`` with respect to the current file.

    - with ``%include('some.rst.tpl', param="test")`` with optional parameters
    - with ``%globals().update(include('utility.rst.tpl'))``
      if it contains only definitions

Links

- ``.. _`id`:`` are *reST targets*.
  reST targets should not be template-generated.
  The template files should have a higher or equal number of targets
  than the generated file,
  in order for tags to jump to the template original.
  If one wants to generate reST targets,
  then this should better happen in a previous step,
  e.g. with ``gen`` files mentioned above.

- References use replacement
  `substitutions <http://docutils.sourceforge.net/docs/ref/rst/directives.html#replacement-text>`__:
  ``|id|``.

- If you want an overview of the linking (traceability),
  add ``.. include:: _traceability_file.rst``
  to ``index.rest`` or another ``.rest`` parallel to it.
  It is there in the example project, to include it in tests.
  ``_traceability_file.{svg,png,rst}`` are all in the same directory.

Link files are created in link roots, which are folders where the first main file
(``.rest`` or ``.rst``) is encoutered during depth-first traversal.
Non-overlapping link root paths produce separately linked file sets.

``.. include:: /_links_sphinx.r?st``, with the one initial ``/``
instead of a relative or absolute path,
will automatically search upward for the ``_links_xxx.r?st`` file
(``_sphinx`` is replaced by what is needed by the wanted target when the docs are generated).

Sphinx ``conf.py`` is augmented by configuration for Pandoc and Docutils.
It should be where the input file is, or better at the project root
to be usable with `waf <https://github.com/waf-project/waf>`__.

See the example project created with ``--rest/stpl/ipdt/over``
and the sources of the documentation of
`rstdoc <https://github.com/rpuntaie/rstdoc>`__.


"""

'''
API
---

.. code-block:: py

   import rstdoc.dcx as dcx


The functions in ``dcx.py``
are available to the ``gen_xxx(lns,**kw)`` functions (|dhy|).

'''

try:
    import svgwrite.drawing
except:
    svgwrite = None

try:
    import pyfca
except:
    pyfca = None

try:
    import cairocffi
    import cairosvg
except:
    cairocffi = None
    cairosvg = None

try:
    import sphinx_bootstrap_theme
    html_theme_path = sphinx_bootstrap_theme.get_html_theme_path()
except:
    html_theme_path = ''


class RstDocError(Exception):
    pass


_plus = '+'
_indent = '    '
_indent_text = lambda txt: '\n'.join(_indent+x for x in txt.splitlines())


class _ToolRunner:
    def svg2png(self, *args, **kwargs):
        try:
            cairosvg.svg2png(*args, **kwargs)
        except Exception as e:
            print('Trying inkscape for ',kwargs['write_to'])
            fn = normjoin(tempdir(),'svg.svg')
            with open(fn,'w',encoding='utf-8') as f:
                f.write(kwargs['bytestring'])
            run_inkscape(fn, kwargs['write_to'],
                dpi=kwargs.get('DPI', DPI))

    def run(self, *args, **kwargs):
        if 'outfile' in kwargs:
            del kwargs['outfile']
        return sp.run(*args, **kwargs)


_toolrunner = _ToolRunner()

def opnwrite(filename):
    return open(filename, 'w', encoding='utf-8', newline='\n')

def opnappend(filename):
    return open(filename, 'a', encoding='utf-8', newline='\n')

def opn(filename):
    return open(filename, encoding='utf-8')


isfile = os.path.isfile
isdir = os.path.isdir
islink = os.path.islink

def abspath(x):
    return os.path.abspath(x).replace('\\', '/')


isabs = os.path.isabs


def relpath(x, start=None):
    try:
        return os.path.relpath(x, start=start).replace('\\', '/')
    except ValueError:
        return abspath(x)

def dirname(x):
    return os.path.dirname(x).replace('\\', '/')

base = os.path.basename

def dir_base(x):
    return [e.replace('\\', '/') for e in os.path.split(x)]


def stem(x):
    return os.path.splitext(x)[0].replace('\\', '/')


def stem_ext(x):
    return [e.replace('\\', '/') for e in os.path.splitext(x)]


exists = os.path.exists


def cwd():
    return os.getcwd().replace('\\', '/')


mkdir = partial(os.makedirs, exist_ok=True)
cd = os.chdir
cp = shutil.copy


def ls(x='.'):
    return [e for e in sorted(os.listdir(x))]


def rmrf(x):
    try:
        if isdir(x):
            shutil.rmtree(x)
        else:
            os.remove(x)
    except:
        pass


def filenewer(infile, outfile):
    res = True
    try:
        res = os.path.getmtime(infile) > os.path.getmtime(outfile)
    except:
        pass
    return res


def normjoin(*x):
    return os.path.normpath(os.path.join(*x)).replace("\\", "/")


def updir(fn):
    return normjoin(dirname(fn), '..', base(fn))


# fn='x/y/../y/a.b'
# updir(fn) # x\a.b
# updir('a.b') # ..\a.b
# updir('a.b/a.b') # a.b
# normjoin(fn) # x\y\a.b

def is_project_root_file(filename):
    '''
    Identifies the root of the project by a file name contained there.

    '''
    return filename=='.git' or filename=='waf' or filename=='Makefile' or filename.lower().startswith('readme')


'''
Used for png creation.
'''
DPI = 600

# text files
_stpl = '.stpl'
_tpl = '.tpl'
_rest = '.rest'
_rst = '.rst'
_txt = '.txt'

_grepinc = lambda inc: grep('^\.\. include:: .*_links_sphinx'+inc+'$',
               exts=set(['.rst','.rest','.stpl','.tpl']))

#a_rest is the main extension (default .rest)
def _set_rstrest(a_rest):
    global _rest
    global _rst
    assert isinstance(a_rest,str)
    _rest = a_rest
    _rst = '.rest' if _rest=='.rst' else '.rst'

def _get_rstrest(config=None):
    if config is None:
        config = conf_py(cwd())
    if 'source_suffix' not in config:
        try:
            next(_grepinc('.rest'))
            _set_rstrest('.rst') #found incuded .rest, so main .rst
            if '__file__' in config:
                with opnappend(config['__file__']) as f:
                    f.write('\nsource_suffix = "%s"'%_rest)
            else:
                conf_file = up_dir(is_project_root_file)
                if conf_file:
                    conf_file = normjoin(conf_file,'conf.py')
                    if not exists(conf_file):
                        config['__file__'] = conf_file
                        with opnwrite(conf_file) as f:
                            f.write('\nsource_suffix = "%s"'%_rest)
            return _rest
        except:
            pass
    else:
        _set_rstrest(config['source_suffix'])
        return _rest
    _set_rstrest('.rest')
    return _rest

def is_rest(x):
    return x.endswith(_rest) or x.endswith(_rest + _stpl)

def is_rst(x):
    return x.endswith(_rst) or x.endswith(_rst + _stpl) or x.endswith(
        _rst + _tpl)


# graphic files
_svg = '.svg'
_tikz = '.tikz'
_tex = '.tex'
_dot = '.dot'
_uml = '.uml'
_eps = '.eps'
_pyg = '.pyg'
_png = '.png'  # target of all others


def _is_graphic(t):
    return t != '' and any(x.endswith(t) for x in graphic_extensions)


rextgt = re.compile(
    r'(?:^|^[^\.\%\w]*\s|^\s*\(?\w+[\)\.]\s)\.\. _`?(\w[^:`]*)`?:\s*$')
# no need to consider those not starting with \w,
# because rexlinksto starts with \w
rexsubtgt = re.compile(
    r'(?:^|^[^\.\%\w]*\s|^\s*\(?\w+[\)\.]\s)\.\. \|(\w[^\|]*)\|\s\w+::')

rextitle = re.compile(r'^([!"#$%&\'()*+,\-./:;<=>?@[\]^_`{|}~])\1+$')
#rextitle.match('===')
#rextitle.match('==')
#rextitle.match('=') #NO

rexitem = re.compile(r'^\s*:?\**(\w[^:\*]*)\**:\s*.*$')
#rexitem.match(":linkname: words").groups()[0]
#rexitem.match("linkname: words").groups()[0]
#rexitem.match("a linkname: words").groups()[0]
#rexitem.match("``a`` linkname: words") #NO

rexoneword = re.compile(r'^\s*(\w+)\s*$')
rexname = re.compile(r'^\s*:name:\s*(\w.*)*$')
rexlnks = re.compile(r'(?:^|[^a-zA-Z`])\|(\w+)\|(?:$|[^a-zA-Z`])')
reximg = re.compile(r'(?:image|figure):: ((?:\.|/|\\|\w).*)')
rerstinclude = re.compile(r'\.\. include::\s*([\./\w\\].*)')
restplinclude = re.compile(r"""%\s*include\s*\(\s*["']([^'"]+)['"].*\)\s*""")

#... combined:
cmmnt = r"""[\.:#%/';"-]"""
rexkw = re.compile(r'^\s*('+cmmnt+cmmnt+r' {|%?\s*{*_+\w+_*\()|^\s*:\w+:\s')
#rexkw.search("{{_A30('kw1 kw2')}}")
#rexkw.search("{{_U00___('kw1 kw2')}}")
#rexkw.search(" % __0A0('kw1 kw2')")
#rexkw.search(" % __123_('kw1 kw2')")
#rexkw.search(":A30: kw1 kw2")
#rexkw.search(" :U00: kw1 kw2")
#rexkw.search("\t:123: kw1 kw2")
#rexkw.search("   .. {kw1 kw2}")
#rexkw.search("   -- {kw1 kw2}")
#rexkw.search("   // {kw1 kw2}")
#rexkw.search("   ## {kw1 kw2}")
#rexkw.search("   '' {kw1 kw2}")
#rexkw.search("   ''' {kw1 kw2}") #NO
#rexkw.search(" %  .. {kw1 kw2}") #NO
#rexkw.search(" % fun('kw1 kw2')") #NO
rexkwsplit = re.compile(r'[\W_]+')

rextrace_target_id=re.compile("^[^-_]\w+$")
#rextrace_target_id.match("_000") #NO
#rextrace_target_id.match("-000") #NO
#rextrace_target_id.match("sd 00") #NO
#rextrace_target_id.match("a-d-00") #NO
#rextrace_target_id.match("A_d_00")
#rextrace_target_id.match("sD00")
#rextrace_target_id.match("000")

#https://sourceforge.net/p/docutils/mailman/message/36453416/
_rstlinkfixer = re.compile('#[^>]+>')
def _rst_id_fixer(matchobj):
    return matchobj.group(0).replace(' ', '-').replace('_', '-')
def _rst_id_fix(linktxt):
    return _rstlinkfixer.sub(_rst_id_fixer, linktxt, re.MULTILINE)

@lru_cache()
def _here_or_updir(fldr, file):
    filepth = normjoin(fldr, file)
    there = 1
    if not exists(filepth):
        there = 0
        filedir = up_dir(lambda x:x==file or is_project_root_file(x), start=abspath(fldr))
        if filedir:
            filepth = normjoin(filedir, file)
            if exists(filepth):
                rthere = relpath(filedir,start = fldr)
                #rthere = '../..'
                there = len(rthere.split('..'))
            else:
                there = 0
    return (filepth, there)

# master_doc and latex_documents is determined automatically
sphinx_config_keys = """
    project
    author
    copyright
    version
    release
    html_theme
    html_theme_path
    latex_elements
    html_extra_path
    source_suffix
    """.split()

latex_elements = {
    'preamble':
    r"""
\usepackage{pgfplots}
\usepackage{unicode-math}
\usepackage{tikz}
\usepackage{caption}
\captionsetup[figure]{labelformat=empty}
\usetikzlibrary{
  arrows,snakes,backgrounds,patterns,matrix,shapes,
  fit,calc,shadows,plotmarks,intersections
  }
"""
}

tex_wrap = r"""
\documentclass[12pt, tikz]{standalone}
\usepackage{amsmath}
""" + latex_elements['preamble'] + r"""
\pagestyle{empty}
\begin{document}
%s
\end{document}
"""


def target_id_group(targetid):
    return targetid[0]


target_id_color = {
    "ra": ("r", "lightblue"),
    "sr": ("s", "red"),
    "dd": ("d", "yellow"),
    "tp": ("t", "green")
}

_images = '_images'
# used for _traceability_file.rst and _traceability_file.svg
_traceability_file = '_traceability_file'
html_extra_path = [_traceability_file + '.svg']
pandoc_doc_optref = {
    'latex': '--template reference.tex',
    'html': {},  # each can also be dict of file:template
    'pdf': '--template reference.tex',
    'docx': '--reference-doc reference.docx',
    'odt': '--reference-doc reference.odt'
}
_pandoc_latex_pdf = [
    '--listings', '--number-sections', '--pdf-engine', 'xelatex', '-V',
    'titlepage', '-V', 'papersize=a4', '-V', 'toc', '-V', 'toc-depth=3', '-V',
    'geometry:margin=2.5cm'
]
pandoc_opts = {
    'pdf': _pandoc_latex_pdf,
    'latex': _pandoc_latex_pdf,
    'docx': [],
    'odt': [],
    'html': ['--mathml', '--highlight-style', 'pygments']
}
rst_opts = {  # http://docutils.sourceforge.net/docs/user/config.html
    'strip_comments': True,
    'report_level': 3,
    'raw_enabled': True
}

# ``list-table`` and ``code-block`` are converted to ``table`` and ``code``


def make_counters():
    return {".. figure": 1, ".. math": 1, ".. table": 1, ".. code": 1}


def name_from_directive(directive, count):
    return directive[0].upper() + directive[1:] + ' ' + str(count)


config_defaults = {
    'project': 'Project',
    'author': 'Project Team',
    'copyright': '2019, Project Team',
    'version': '1.0',
    'release': '1.0.0',
    'html_theme': 'bootstrap',
    'html_theme_path': html_theme_path,
    'latex_elements': latex_elements,
    'tex_wrap': tex_wrap,
    'target_id_group': target_id_group,
    'target_id_color': target_id_color,
    'rextrace_target_id': rextrace_target_id,
    'pandoc_doc_optref': pandoc_doc_optref,
    'pandoc_opts': pandoc_opts,
    'rst_opts': rst_opts,
    'name_from_directive': name_from_directive
}

sphinx_enforced = {
    'numfig': 0,
    'smartquotes': 0,
    'templates_path': [],
    'language': None,
    'highlight_language': "none",
    'default_role': 'math',
    'latex_engine': 'xelatex',
    'pygments_style': 'sphinx',
    'exclude_patterns': ['_build', 'Thumbs.db', '.DS_Store']
}


'''
``g_config`` can be used to inject a global config.
This overrides the defaults
and is overriden by an updir ``conf.py``.
'''
g_config = None

@lru_cache()
def conf_py(fldr):
    """
    ``defaults``, ``g_config``, updir ``conf.py``

    """
    config = {}
    config.update(config_defaults)
    if g_config:
        config.update(g_config)
    confpydir = up_dir(lambda x:x=='conf.py' or is_project_root_file(x),start=abspath(fldr))
    if confpydir:
        confpypath = normjoin(confpydir,'conf.py')
        if exists(confpypath):
            with opn(confpypath) as f:
                config['__file__'] = abspath(confpypath)
                eval(compile(f.read(), abspath(confpypath), 'exec'), config)
                config.update(sphinx_enforced)
                return config
    if g_include:
        for gi in g_include:
            confpypath = normjoin(gi,'conf.py')
            if exists(confpypath):
                config['__file__'] = abspath(confpypath)
                eval(compile(f.read(), abspath(confpypath), 'exec'), config)
                break
    config.update(sphinx_enforced)
    return config

def _fillwith(u, v):
    return [x or v for x in u]


def _joinlines(lns):
    if lns[0].endswith('\n'):
        tmp = ''.join(lns)
    else:
        tmp = '\n'.join(lns)
    return tmp.replace('\r\n', '\n')


# x=b'a\r\nb'
# _nbstr(x)==b'a\nb'
def _nbstr(x):
    return x and x.replace(b'\r\n', b'\n') or b''

# x=x.decode()
# _nstr(x)=='a\nb'


def _nstr(x):
    return x and x.replace('\r\n', '\n') or ''


def cmd(cmdlist, **kwargs):
    '''
    Runs ``cmdlist`` via subprocess.run and return stdout.
    In case of problems RstDocError is raised.

    :param cmdlist: command as list
    :param kwargs: arguments forwarded to ``subprocess.run()``

    '''

    cmdstr = ' '.join(cmdlist)
    try:
        for x in 'out err'.split():
            kwargs['std' + x] = sp.PIPE
        r = _toolrunner.run(cmdlist, **kwargs)
        try:
            stdout, stderr = _nstr(r.stdout), _nstr(r.stderr)
        except:
            stdout, stderr = _nbstr(r.stdout).decode('utf-8'), _nbstr(
                r.stderr).decode('utf-8')
        if r.returncode != 0:
            raise RstDocError('Error code %s returned from \n%s\nin\n%s\n' % (
                r.returncode, cmdstr,
                cwd()) + '\n[stdout]\n%s\n[stderr]\n%s' % (stdout, stderr))
        return stdout
    except OSError as err:
        raise RstDocError(
            'Error: Cannot run ' + cmdstr + ' in ' + cwd() + str(err))


def _imgout(inf):
    inp, inname = dir_base(inf)
    infn, infe = stem_ext(inname)
    if not _is_graphic(infe) and not _is_graphic(stem_ext(infn)[1]):
        raise ValueError('%s is not an image source' % inf)
    outp, there = _here_or_updir(inp, _images)
    if not there:
        outp = inp
    outname = infn + _png
    nout = normjoin(outp, outname)
    return nout


def _unioe(args):
    i, o = [None] * 2
    try:
        (i, o), a = args[:2], args[2:]
    except:
        (i,), a = args[:1], args[1:]
    return i, o, a


def png_post_process_if_any(f):
    @wraps(f)
    def png_post_processor(*args, **kwargs):
        infile, outfile, args = _unioe(args)
        if isinstance(infile, str):
            config = conf_py(dirname(infile))
            pp = config.get('png_post_processor', None)
            pngfile = f(infile, outfile, *args, **kwargs)
            if pp and exists(pngfile):
                return pp(pngfile)
            else:
                return pngfile
        return f(infile, outfile, *args, **kwargs)

    return png_post_processor


def _ext(x):
    return x[0] == '.' and x or '.' + x


_cdlock = RLock()


@contextlib.contextmanager
def new_cwd(apth):
    '''
    Use as::

        with new_cwd(dir):
            #inside that directory

    '''

    _cdlock.acquire()
    prev_cwd = cwd()
    cd(apth)
    try:
        yield
    finally:
        cd(prev_cwd)
        _cdlock.release()


def startfile(filepath):
    '''
    Extends the Python startfile to non-Windows platforms

    '''

    if sys.platform.startswith('darwin'):
        sp.call(('open', filepath))
    elif os.name == 'nt':  # For Windows
        os.startfile(filepath)
    elif os.name == 'posix':  # For Linux, Mac, etc.
        sp.call(('xdg-open', filepath))

def up_dir(match,start=None):
    '''
    Find a parent path producing a match on one of its entries.
    Without a match an empty string is returned.

    :param match: a function returning a bool on a directory entry
    :param start: absolute path or None
    :return: directory with a match on one of its entries

    >>> up_dir(lambda x: False)
    ''

    '''

    if start is None:
        start = os.getcwd()
    if any(match(x) for x in os.listdir(start)):
        return start
    parent = os.path.dirname(start)
    if start == parent:
        rootres = start.replace('\\','/').strip('/').replace(':','')
        if len(rootres)==1 and sys.platform=='win32':
            rootres = ''
        return rootres
    return up_dir(match,start=parent)

def tempdir():
    '''
    Make temporary directory and register it to be removed with ``atexit``.

    This can be used inside a ``.stpl`` file
    to create images from inlined images source,
    place them in temporary file,
    and include them in the final ``.docx`` or ``.odt``.

    '''

    atmpdir = tempfile.mkdtemp()
    atexit.register(rmrf, atmpdir)
    return atmpdir


def infile_cwd(f):
    """
    Changes into the directory of the infile if infile is a file name string.
    """

    @wraps(f)
    def infilecwder(*args, **kwargs):
        infile, outfile, args = _unioe(args)
        if isinstance(infile, str):
            ndir, inf = dir_base(infile)
        else:
            ndir, inf = '', infile
        if ndir:
            if isinstance(outfile, str) and outfile != '-':
                outfile = relpath(outfile, start=ndir)
            with new_cwd(ndir):
                return f(inf, outfile, *args, **kwargs)
        return f(infile, outfile, *args, **kwargs)

    return infilecwder


def normoutfile(f, suffix=None):
    """
    Make outfile from infile by appending suffix, or, if None,
    ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.
    The outfile is returned.
    """

    @wraps(f)
    def normoutfiler(*args, **kwargs):
        infile, outfile, args = _unioe(args)
        if isinstance(infile, str):
            if not outfile:
                if not suffix or _is_graphic(suffix):
                    outfile = _imgout(infile)
                elif suffix:
                    infn, infe = stem_ext(infile)
                    outinfo = kwargs.get('outinfo', None)
                    if outinfo.startswith('sphinx'):
                        outfile = "{1}/{0}/{2}".format(
                                  outinfo, *dir_base(infn)
                                  ) + '.' + outinfo[outinfo.find('_') + 1:]
                    else:
                        if _stpl.endswith(infe):
                            infn, infe = stem_ext(infn)
                        outfile = infn
        f(infile, outfile, *args, **kwargs)
        return outfile

    return normoutfiler

def _suffix(outinfo):
    try:
        _, suf = outinfo.split('_')
    except: #noqa
        suf = outinfo
    return suf or 'html'

def _in_2_out_name(inname,outinfo):
    instem = stem(inname)
    if instem.endswith(_rest) or instem.endswith(_rst):
        instem = stem(instem)
    res = base(instem) + '.' + _suffix(outinfo).strip('.')
    return res

def in_temp_if_list(
        f,
        suffix='stpl'
        ):
    """

    to produce a temporary directory/file for when infile is a list of strings.
    The temporary directory/file is removed via atexit.

    :param suffix: .dot, .uml, ... or rest.stpl,...
      default it will assume stpl and use outinfo

    To make this have an effect use after ``readin``

    - includes ``normoutfile``

    If outfile is None, outfile is derived from suffix,
    which can be ``rest.stpl``, ``png.svg``;
    If suffix is ``.svg``, ...,
    png is assumed and will be placed into ``_images``.

    """

    @wraps(f)
    def intmpiflister(*args, **kwargs):
        infile, outfile, args = _unioe(args)
        outinfo = None
        try:
            suf0, suf1 = suffix.split('.', 1)
        except: #noqa
            outinfo = kwargs.get('outinfo', 'rest')
            if _is_graphic(outinfo):
                suf0, suf1 = outinfo, suffix
            else:
                # see infile_outinfo
                _, outi = dir_base(outinfo)
                suf0, suf1 = _suffix(outi) + _rest, suffix
        if not isinstance(infile, str) and infile:
            if outfile and isinstance(outfile, str):
                outfile = abspath(outfile)
            atmpdir = tempdir()
            content = _joinlines(infile).encode('utf-8')
            infnfromoutinfo,outi = outinfo and dir_base(outinfo) or (None,None)
            if outfile and isinstance(outfile, str):
                infn = stem(base(outfile))
            elif infnfromoutinfo:
                kwargs['outinfo'] = outi
                infn = infnfromoutinfo
            else:
                infn = sha(content).hexdigest()
            if suf0:
                infile = normjoin(atmpdir, '.'.join([infn, suf0, suf1]))
            else:
                infile = normjoin(atmpdir, '.'.join([infn, suf1]))
            with open(infile, 'bw') as ff:
                ff.write(content)
            return normoutfile(f, suf0)(infile, outfile, *args, **kwargs)
        return normoutfile(f, suf0)(infile, outfile, *args, **kwargs)

    return intmpiflister


def readin(f):
    """
    Decorator to read in file content and pass it on to the wrapped function.

    The config is forwarded via parameters, if the file is read in.
    """

    @wraps(f)
    def readiner(*args, **kwargs):
        infile, outfile, args = _unioe(args)
        if isinstance(infile, str):
            config = conf_py(dirname(infile))
            with opn(infile) as inf:
                return f(inf.readlines(), outfile, *args, **config, **kwargs)
        return f(infile, outfile, *args, **kwargs)

    return readiner


def run_inkscape(infile,  outfile, dpi=DPI):
    '''
    Uses ``inkscape`` commandline to convert to ``.png``

    :param infile: .svg, .eps, .pdf filename string
      (for list with actual .eps or .svg data use |dcx.svgpng| or |dcx.epspng|)
    :param outfile: .png file name

    '''

    cmd([
        'inkscape', '--export-dpi=%s' % dpi, '--export-area-drawing',
        '--export-background-opacity=0', infile,
        '--export-filename='+outfile
    ],
        outfile=outfile)


@infile_cwd
def rst_sphinx(
        infile, outfile, outtype=None, **config
        ):
    '''
    Run Sphinx on infile.

    :param infile: .txt, .rst, .rest filename
    :param outfile: the path to the target file (not target directory)
    :param outtype: html, latex,... or any other sphinx writer
    :param config: keys from config_defaults

    ::

        >>> olddir = os.getcwd()
        >>> cd(dirname(__file__))
        >>> cd('../doc')

        >>> infile, outfile = ('index.rest',
        ... '../build/doc/sphinx_html/index.html')
        >>> rst_sphinx(infile, outfile) #doctest: +ELLIPSIS
        >>> exists(outfile)
        True

        >>> infile, outfile = ('dd.rest',
        ... '../build/doc/sphinx_html/dd.html')
        >>> rst_sphinx(infile, outfile) #doctest: +ELLIPSIS
        >>> exists(outfile)
        True

        >>> infile, outfile = ('dd.rest',
        ... '../build/doc/sphinx_latex/dd.tex')
        >>> rst_sphinx(infile, outfile) #doctest: +ELLIPSIS
        >>> exists(outfile)
        True

        >>> cd(olddir)

    '''

    cfgt = {}
    cfgt.update(config_defaults)
    cfgt.update(config)

    indr, infn = dir_base(infile)
    outdr, outn = dir_base(outfile)
    outnn, outne = stem_ext(outn)
    samedir = False
    if outdr == indr:
        samedir = True
    if not indr:
        indr = '.'
    cfg = {}
    cfg.update({
        k: v
        for k, v in cfgt.items() if k in sphinx_config_keys
        and 'latex' not in k and k != 'html_extra_path'
    })
    cfg.setdefault('source_suffix','.rest')
    #cfg['source_suffix'] = '.rest'
    if not outtype or outtype=='html':
        if outne == '.html':
            if infn.startswith('index.'):
                outtype = 'html'
            else:
                outtype = 'singlehtml'
        elif outne == '.tex':
            outtype = 'latex'
        else:
            outtype = outne.strip('.')
    cfg.update({k: v for k, v in sphinx_enforced.items() if 'latex' not in k})
    cfg['master_doc'] = stem(infn) #.rest.rest -> .rest (see rest_rest)
    # .rest.rest contains temporary modification and is used instead of .rest
    if outtype == 'html' and is_rest(cfg['master_doc']): #... not for index.rest
        cfg['master_doc'] = stem(cfg['master_doc'])
    if exists(cfg['master_doc']):
        cfg['master_doc'] = stem(cfg['master_doc'])
    if samedir:
        outdr = normjoin(outdr, 'sphinx_'+outtype)
        outfn = normjoin(outdr, outn)
        print('Warning: Shinx output cannot be in same directory. Using '
              + outdr)
    else:
        outfn = outfile
    latex_elements = []
    latex_documents = []
    if 'latex' in outtype:
        cfg.update({
            k: v
            for k, v in cfgt.items()
            if k in sphinx_config_keys and 'latex' in k
        })
        cfg.update({k: v for k, v in sphinx_enforced.items() if 'latex' in k})
        try:
            latex_elements = ([
                ['-D', "latex_elements.%s=%s" % (k, v.replace('\n', ''))]
                for k, v in cfg['latex_elements'].items()
            ] + [['-D', 'latex_engine=xelatex']])
        except:
            pass
        del cfg['latex_elements']
        del cfg['latex_engine']
        latex_documents = []
    extras = ['-C'] + reduce(lambda x, y: x + y, [[
        '-D', "%s=%s" % (k, (','.join(v) if isinstance(v, list) else v))
    ] for k, v in cfg.items()] + latex_elements + latex_documents)
    sphinxcmd = ['sphinx-build', '-b', outtype, indr, outdr] + extras
    cmd(sphinxcmd, outfile=outfn)
    if outtype == 'html':
        #undo duplication via temp file's see: rest_rest
        rmrf(normjoin(outdr,cfg['master_doc']+_rest+'.html'))
    if 'latex' in outtype:
        texfile = next(x for x in os.listdir(outdr) if x.endswith('.tex'))
        os.rename(normjoin(outdr, texfile), outfn)
    if 'html' in outtype and 'html_extra_path' in cfgt:
        for epth in cfgt['html_extra_path']:
            try:
                if isabs(epth):
                    epth = relpath(epth, start=indr)
                cp(epth, normjoin(outdr, epth))
            except:
                pass


def _copy_images_for(infile, outfile, with_trace):
    indr = dirname(infile)
    imgdir, there = _here_or_updir(indr, _images)
    imgdir_tgt = outfile
    while there:
        imgdir_tgt = dirname(imgdir_tgt)
        there = there - 1
    if imgdir_tgt == outfile:
        return
    imgdir_tgt = normjoin(imgdir_tgt,_images)
    outdr = dirname(outfile)
    if with_trace:
        tracesvg = normjoin(indr, _traceability_file + _svg)
        if exists(tracesvg):
            try:
                cp(tracesvg, normjoin(outdr, _traceability_file + _svg))
            except:
                pass
    if exists(imgdir) and imgdir != imgdir_tgt:
        if not exists(imgdir_tgt):
            mkdir(imgdir_tgt)
        for x in os.listdir(imgdir):
            frm, twd = normjoin(imgdir, x), normjoin(imgdir_tgt, x)
            docpy = filenewer(frm, twd)
            if docpy:
                try:
                    cp(frm, twd)
                except:
                    pass

'''
One can append paths to ``rstdoc.dcx.g_include`` for stpl expansion
or finding other files.
'''
g_include = []

@infile_cwd
def rst_pandoc(
        infile, outfile, outtype, **config
        ):
    '''
    Run Pandoc on infile.

    :param infile: .txt, .rst, .rest filename
    :param outfile: the path to the target document
    :param outtype: html,...
    :param config: keys from config_defaults

    '''

    cfg = {}
    cfg.update(config_defaults)
    cfg.update(config)
    pandoccmd = ['pandoc', '--standalone', '-f', 'rst'] + cfg.get(
        'pandoc_opts', {}).get(outtype, []) + [
            '-t', 'latex'
            if outtype == 'pdf' else outtype.replace('rest','rst'), infile, '-o', outfile
        ]
    opt_refdoc = cfg.get('pandoc_doc_optref', {}).get(outtype, '')
    if opt_refdoc:
        if isinstance(opt_refdoc, dict):
            opt_refdoc = opt_refdoc.get(base(infile), '')
        if opt_refdoc:
            refoption, refdoc = opt_refdoc.split()
            refdocfound, there = _here_or_updir('.', refdoc)
            if there:
                pandoccmd.append(refoption)
                pandoccmd.append(abspath(refdocfound))
            elif g_include:
                refdoc = dir_base(refdoc)[1]
                for gi in g_include:
                    refdoctry = normjoin(gi,refdoc)
                    if exists(refdoctry):
                        pandoccmd.append(refoption)
                        pandoccmd.append(refdoctry)
                        break
    stdout = cmd(pandoccmd, outfile=outfile)
    if outtype.endswith('html') or outtype.endswith('latex'):
        _copy_images_for(infile, outfile, outtype.endswith('html'))
    elif outtype.endswith('odt'):
        PageBreakHack(outfile)
    return stdout


def _indented_default_role_math(filelines):
    """

    .. _`x`:

    xlabel:

    hello

    """
    indent = ''
    i = 0
    try:
        while not filelines[i].strip():
            i = i+1
        indent = ' '*filelines[i].index(filelines[i].lstrip())
    except:
        pass
    return indent + '.. default-role:: math\n\n'


@infile_cwd
def rst_rst2(
        infile, outfile, outtype, **config
        ):
    '''
    Run the rst2xxx docutils fontend tool on infile.

    :param infile: .txt, .rst, .rest filename
    :param outfile: the path to the target document
    :param outtype: html,...
    :param config: keys from config_defaults

    '''

    cfg = {}
    cfg.update(config_defaults)
    cfg.update(config)
    destination_path = outfile if outfile != '-' else None
    if outtype == 'odt':
        outtype = 'odf_odt'
    stdout = None
    if isinstance(infile, str):
        publish_file(
            source_path=infile,
            destination_path=destination_path,
            writer_name=outtype,
            settings_overrides=cfg['rst_opts'])
    else:
        source = _indented_default_role_math(infile) + _joinlines(infile)
        stdout = publish_string(
            source,
            destination_path=destination_path,
            writer_name=outtype,
            settings_overrides=cfg['rst_opts'])
    if destination_path:
        if outtype.endswith('html') or outtype.endswith('latex'):
            _copy_images_for(infile, outfile, outtype.endswith('html'))
        elif outtype.endswith('odt'):
            PageBreakHack(destination_path)
    return stdout


def PageBreakHack(destination_path):
    '''
    This introduces a ``PageBreak`` style into ``content.xml``
    to allow the following raw page break of opendocument odt::

      .. raw:: odt

          <text:p text:style-name="PageBreak"/>

    This is no good solution,
    as it introduces an empty line at the top of the new page.

    Unfortunately the following does not work
    with or without ``text:use-soft-page-breaks="true"``

    ::

        .. for docutils
        .. raw:: odt

            <text:p text:style-name="PageBreak"/>

        .. for pandoc
        .. raw:: opendocument

            <text:p text:style-name="PageBreak"/>

    According to
    `C066363e.pdf <https://standards.iso.org/ittf/PubliclyAvailableStandards/c066363_ISO_IEC_26300-1_2015.zip>`__
    it should work.

    See ``utility.rst.tpl`` in the ``--stpl`` created example project tree.

    '''

    from zipfile import ZipFile
    odtzip = OrderedDict()
    with ZipFile(destination_path) as z:
        for n in z.namelist():
            with z.open(n) as f:
                content = f.read()
                if n == 'content.xml':
                    # break-after produces two page breaks
                    content = content.replace(
                        b'</office:automatic-styles>', b' '.join(
                            x.strip() for x in b"""<style:style
                      style:name="PageBreak"
                      style:family="paragraph"
                      style:master-page-name="rststyle-pagedefault"
                      style:parent-style-name="Standard">
                      <style:paragraph-properties fo:break-before="page"/>
                      </style:style>
                      </office:automatic-styles>""".splitlines()))
                    content = content.replace(
                        b'<office:text>',
                        b'<office:text text:use-soft-page-breaks="true">')
            odtzip[n] = content
    with ZipFile(destination_path, 'w') as z:
        for n, content in odtzip.items():
            with z.open(n, mode='w', force_zip64=True) as f:
                f.write(content)


# sphinx_html, rst_html, [pandoc_]html
rst_tools = {'pandoc': rst_pandoc, 'sphinx': rst_sphinx, 'rst': rst_rst2}


@png_post_process_if_any
@normoutfile
@readin
def svgpng(infile, outfile=None, *args, **kwargs):
    '''
    Converts a .svg file to a png file.

    :param infile: a .svg file name or list of lines
    :param outfile: if not provided the input file with new extension
          ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.

    '''

    _toolrunner.svg2png(
        bytestring=_joinlines(infile),
        write_to=outfile,
        dpi=kwargs.get('DPI', DPI))


@png_post_process_if_any
@partial(in_temp_if_list, suffix='.tex')
@infile_cwd
def texpng(infile, outfile=None, *args, **kwargs):
    '''
    Latex has several graphic packages, like

    - tikz
    - chemfig

    that can be converted to .png with this function.

    For ``.tikz`` file use |dcx.tikzpng|.

    :param infile: a .tex file name or list of lines
        (provide outfile in the latter case)
    :param outfile: if not provided, the input file with
                  ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.

    '''

    pdffile = stem(infile) + '.pdf'
    try:
        cmd(['xelatex', '-interaction=nonstopmode', infile], outfile=pdffile)
    except RstDocError as err:
        with opn(infile) as latex:
            raise RstDocError(str(err) + '\n[latex]\n' + latex.read())
    run_inkscape(pdffile, outfile, dpi=kwargs.get('DPI', DPI))


def _texwrap(f):
    @wraps(f)
    def _texwraper(*args, **kwargs):
        texlns, outfile, args = _unioe(args)
        content = _joinlines(texlns)
        latex = kwargs.get('tex_wrap', tex_wrap) % content
        return f(latex.splitlines(), outfile, *args, **kwargs)

    return _texwraper


def _tikzwrap(f):
    @wraps(f)
    def _tikzwraper(*args, **kwargs):
        tikzlns, outfile, args = _unioe(args)
        content = _joinlines(tikzlns).strip()
        tikzenclose = [r'\begin{tikzpicture}', '%s', r'\end{tikzpicture}']
        if not content.startswith(tikzenclose[0]):
            content = _joinlines(tikzenclose) % content
        return f(content.splitlines(), outfile, *args, **kwargs)

    return _tikzwraper


'''
Converts a .tikz file to a png file.

See |dcx.texpng|.
'''
tikzpng = normoutfile(readin(_tikzwrap(_texwrap(texpng))))


@png_post_process_if_any
@partial(in_temp_if_list, suffix='.dot')
@infile_cwd
def dotpng(
        infile,
        outfile=None,
        *args,
        **kwargs
        ):
    '''
    Converts a .dot file to a png file.

    :param infile: a .dot file name or list of lines
        (provide outfile in the latter case)
    :param outfile: if not provided the input file with new extension
        ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.

    '''

    cmd(['dot', '-Tpng', infile, '-o', outfile], outfile=outfile)


@png_post_process_if_any
@partial(in_temp_if_list, suffix='.uml')
@infile_cwd
def umlpng(
        infile,
        outfile=None,
        *args,
        **kwargs
        ):
    '''
    Converts a .uml file to a png file.

    :param infile: a .uml file name or list of lines
        (provide outfile in the latter case)
    :param outfile: if not provided the input file with new extension
        ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.

    '''

    cmd(['plantuml', '-tpng', infile, '-o' + dirname(outfile)],
        shell=sys.platform == 'win32',
        outfile=outfile)


@png_post_process_if_any
@partial(in_temp_if_list, suffix='.eps')
@infile_cwd
def epspng(
        infile,
        outfile=None,
        *args,
        **kwargs):
    '''
    Converts an .eps file to a png file using inkscape.

    :param infile: a .eps file name or list of lines
        (provide outfile in the latter case)
    :param outfile: if not provided the input file with new extension
        ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.

    '''

    run_inkscape(infile, outfile, dpi=kwargs.get('DPI', DPI))


@png_post_process_if_any
@normoutfile
@readin
@infile_cwd
def pygpng(
        infile, outfile=None, *args,
        **kwargs
        ):
    '''
    Converts a .pyg file to a png file.

    ``.pyg`` contains python code that produces a graphic.
    If the python code defines a ``to_svg`` or a ``save_to_png`` function,
    then that is used.
    Else the following is tried

    - ``pyx.canvas.canvas`` from the
      `pyx <http://pyx.sourceforge.net/manual/graphics.html>`__ library or
    - ``svgwrite.drawing.Drawing`` from the
      `svgwrite <https://svgwrite.readthedocs.io>`__ library or
    - ``cairocffi.Surface`` from
      `cairocffi <https://cairocffi.readthedocs.io/en/stable/overview.html#basic-usage>`__
    - `matplotlib <https://matplotlib.org>`__.
      If ``matplotlib.pyplot.get_fignums()>1``
      the figures result ``<name><fignum>.png``

    :param infile: a .pyg file name or list of lines
        (provide outfile in the latter case)
    :param outfile: if not provided the input file with new extension
        ``.png`` in ``./_images``, ``<updir>/_images`` or parallel to infile.

    '''

    pygcode = _joinlines(infile)
    pygvars = {}
    dpi = kwargs.get('DPI', DPI)
    eval(compile(pygcode, outfile, 'exec'), pygvars)
    if 'save_to_png' in pygvars:
        pygvars['save_to_png'](outfile)
    elif 'to_svg' in pygvars:
        _toolrunner.svg2png(bytestring=pygvars['to_svg'](),
            write_to=outfile, dpi=dpi)
    else:
        for _, v in pygvars.items():
            if hasattr(v,'_repr_svg_'):
                _toolrunner.svg2png(
                    bytestring=v._repr_svg_(), write_to=outfile, dpi=dpi)
                break
            elif cairocffi and isinstance(v, cairocffi.Surface):
                v.write_to_png(target=outfile)
                break
            elif svgwrite and isinstance(v, svgwrite.drawing.Drawing):
                _toolrunner.svg2png(bytestring=v.tostring(),
                                    write_to=outfile, dpi=dpi)
                break
            else:  # try matplotlib.pyplot
                try:
                    fignums = plt.get_fignums()
                    if len(fignums) == 0:
                        continue
                    if len(fignums) > 1:
                        # makename('a.b', 1) # a1.b
                        def makename(x, i):
                            return ('{0}%s{1}' % i).format(*stem_ext(x))
                    else:

                        def makename(x, i):
                            return x
                    for i in fignums:
                        plt.figure(i).savefig(
                            makename(outfile, i), format='png')
                    plt.close()
                    break
                except:
                    continue

@readin
@infile_cwd
def pygsvg(infile, *args, **kwargs):
    '''
    Converts a .pyg file or according python code to an svg string.

    ``.pyg`` contains python code that produces an SVG graphic.
    Either there is a ``to_svg()`` function or
    the following is tried

    - ``io.BytesIO`` containing SVG, e.g via ``cairo.SVGSurface(ioobj,width,height)``
    - ``io.StringIO`` containing SVG
    - object with attribute ``_repr_svg_``
    - ``svgwrite.drawing.Drawing`` from the
      `svgwrite <https://svgwrite.readthedocs.io>`__ library or
    - ``cairocffi.SVGSurface`` from
      `cairocffi <https://cairocffi.readthedocs.io/en/stable/overview.html#basic-usage>`__
    - `matplotlib <https://matplotlib.org>`__.

    :param infile: a .pyg file name or list of lines

    '''

    onlysvg = lambda x: '<svg'+x.split('<svg')[1]
    pygcode = _joinlines(infile)
    pygvars = {}
    eval(compile(pygcode, "pygsvg", 'exec'), pygvars)
    if 'to_svg' in pygvars:
        return onlysvg(pygvars['to_svg']())
    else:
        for _, v in pygvars.items():
            if hasattr(v,'_repr_svg_'):
                return onlysvg(v._repr_svg_())
            elif cairocffi and isinstance(v, cairocffi.SVGSurface):
                v.finish()
                break #find io.BytesIO
            elif svgwrite and isinstance(v, svgwrite.drawing.Drawing):
                return v.tostring()
            else:  # try matplotlib.pyplot
                try:
                    fignums = plt.get_fignums()
                    if len(fignums) == 0:
                        continue
                    svgsrc = ""
                    for i in fignums:
                        bio = io.BytesIO()
                        plt.figure(i).savefig(bio,format='svg')
                        bio.seek(0)
                        svgsrc += onlysvg(bio.read().decode('utf-8'))
                    plt.close()
                    return svgsrc
                except:
                    continue
        for _, v in pygvars.items():
            if isinstance(v, io.BytesIO):
                v.seek(0)
                return onlysvg(v.read().decode('utf-8'))
            elif isinstance(v, io.StringIO):
                v.seek(0)
                return onlysvg(v.read())

def svgembed(
        pyg_or_svg, outinfo, *args, **kwargs
        ):
    '''
    If ``outinfo`` ends with ``html``, SVG is embedded.
    Else the SVG is converted to a temporary image file
    and included in the DOCX or ODT zip.

    '''

    try:
        svgsrc = pygsvg(pyg_or_svg)
    except Exception:
        svgsrc = _joinlines(pyg_or_svg)
    if outinfo.endswith('html') or outinfo.endswith('rest') or outinfo.endswith('rst'):
        return '.. raw:: html\n\n'+_indent_text(svgsrc)
    else:
        svgfn = normjoin(tempdir(),'svg.png')
        svgpng(svgsrc.splitlines(),svgfn, *args, **kwargs)
        return ".. image:: {}".format(svgfn)


def _png64(pngfn):
    with open(pngfn,'rb') as f:
        b64 = b2a_base64(f.read())
        return '<img src="data:image/png;base64,{0}"/>'.format(b64.decode("utf-8"))


def pngembed(
        pyg_or_pngfile, outinfo, *args, **kwargs
        ):
    '''
    If ``outinfo`` ends with ``html``, the PNG is embedded.
    Else the PNG is included in the DOCX or ODT zip.

    '''

    pngfn = normjoin(tempdir(),'png.png')
    pygpng(pyg_or_pngfile,pngfn,*args,**kwargs)
    if outinfo.endswith('html') or outinfo.endswith('rest') or outinfo.endswith('rest'):
        return '.. raw:: html\n\n'+_indent_text(_png64(pngfn))
    else:
        return ".. image:: {}".format(pngfn)


@infile_cwd
def dostpl(
        infile,
        outfile=None,
        lookup=None,
        **kwargs
        ):
    '''
    Expands an `.stpl <https://bottlepy.org/docs/dev/stpl.html>`__ file.

    The whole ``rstdoc.dcx`` namespace is forwarded to the template code.

    ``.stpl`` is unrestrained python:

    - e.g. one can create temporary images,
      which are then included in the final .docx of .odt
      See |dcx.tempdir|.

    :param infile: a .stpl file name or list of lines
    :param outfile: if not provided the expanded is returned
    :param lookup: lookup paths can be absolute or relative to infile

    ::

        >>> infile = ['hi {{2+3}}!']
        >>> dostpl(infile)
        ['hi 5!']

    '''

    if not lookup:
        lookup = ['.', '..'] + g_include
    if isinstance(infile, str):
        lookup = [abspath(normjoin(dirname(infile), x)) for x in lookup if not isabs(x)
                  ]+[x for x in lookup if isabs(x)]
        filename = abspath(infile)
    else:
        lookup = [abspath(x) for x in lookup if not isabs(x)
                  ]+[x for x in lookup if isabs(x)]
        try:
            filename = abspath(outfile)
        except:
            filename = None
        infile = _joinlines(infile)
    variables = {}
    variables.update(globals())
    variables.update(kwargs)
    variables.update({'__file__': filename})
    if 'outinfo' not in variables and outfile:
        _, variables['outinfo'] = stem_ext(outfile)
    stpl.TEMPLATES.clear()
    st = stpl.template(
        infile,
        template_settings={'escape_func': lambda x: x},
        template_lookup=lookup,
        **variables
    )
    if outfile:
        with opnwrite(outfile) as f:
            f.write(st)
    else:
        return st.replace('\r\n', '\n').splitlines(keepends=True)


def dorst(
        infile,
        outfile=io.StringIO,
        outinfo=None,
        fn_i_ln=None,
        **kwargs
        ):
    r'''
    Default interpreted text role is set to math.
    The link lines are added to the rest file or rst lines

    :param infile: a .rest, .rst, .txt file name or list of lines

    :param outfile: None and '-' mean standard out.

        If io.StringIO, then the lines are returned.
        ``|xxx|`` substitutions for reST link targets
        in infile are appended if no ``_links_sphinx.rst`` there

    :param outinfo: specifies the tool to use.

        - ``html``, ``docx``, ``odt``,... via pandoc
        - ``sphinx_html``,... via sphinx
        - ``rst_html``,... via rst2xxx frontend tools

        General format of outinfo::

            [infile/][tgtfile.]docx[.]

        ``infile`` is used, if the function infile param are lines.

        ``tgtfile`` is target file used in links.

        ``tgtfile`` is the target file to create.
        A final dot tells not to create the target file.
        This is of use in the command line
        if piping a file to rstdoc then to pandoc.
        The doc will only be generated by pandoc,
        but links need to know the doc to link to already before that.

    :param fn_i_ln: ``(fn, i, ln)`` of the ``.stpl``
        with all stpl includes sequenced (used by |dcx.convert|)

    ::

        >>> olddir = os.getcwd()
        >>> cd(dirname(__file__))
        >>> cd('../doc')

        >>> dorst('dd.rest') #doctest: +ELLIPSIS
        ['.. default-role:: math\n', ...

        >>> dorst('ra.rest.stpl') #doctest: +ELLIPSIS
        ['.. default-role:: math\n', ...

        >>> dorst(['hi there']) #doctest: +ELLIPSIS
        ['.. default-role:: math\n', '\n', 'hi there\n', ...

        >>> dorst(['hi there'], None,'html') #doctest: +ELLIPSIS
        <!DOCTYPE html>
        ...

        >>> drst=lambda x,y: dorst(x,y,None,pandoc_doc_optref={'docx':'--reference-doc doc/reference.'+y.split('.')[1]})
        >>> dorst('ra.rest.stpl','ra.docx') #doctest: +ELLIPSIS
        >>> exists('ra.docx')
        True
        >>> rmrf('ra.docx')
        >>> exists('ra.docx')
        False
        >>> rmrf('ra.rest.stpl.rest')
        >>> exists('ra.rest.stpl.rest')
        False

        >>> dorst(['hi there'],'test.html') #doctest: +ELLIPSIS
        >>> exists('test.html')
        True
        >>> rmrf('test.html')
        >>> exists('test.html')
        False
        >>> rmrf('rest.rest.rest')
        >>> exists('rest.rest.rest')
        False

        >>> dorst(['hi there'],'test.odt','rst') #doctest: +ELLIPSIS
        >>> exists('rest.rest.rest')
        True
        >>> rmrf('rest.rest.rest')
        >>> exists('rest.rest.rest')
        False
        >>> exists('test.odt')
        True
        >>> rmrf('test.odt')
        >>> exists('test.odt')
        False
        >>> cd(olddir)


    '''

    tool = 'pandoc'
    rsttool = rst_tools[tool]
    dinfo, binfo = None, None
    if outinfo:
        # see infile_outinfo
        dinfo, binfo = dir_base(outinfo)
        outinfo = binfo
    if not isinstance(outfile, str) and outinfo in rst_tools:
        rsttool = rst_tools[outinfo]
        outinfo = 'html'
    else:
        try:
            tool, outinfo = outinfo.split('_')
            try:
                rsttool = rst_tools[tool]
            except:
                rsttool = None
        except:
            pass
    if isinstance(infile, str):
        infile = abspath(infile)
        with opn(infile) as f:
            filelines = f.readlines()
    else:
        filelines = infile
        infile = dinfo
        if not infile:
            infile = 'rest'
        infile = abspath(infile + _rest)
    sysout = None
    finalsysout = None
    try:
        if outfile is None or outfile == '-':
            if not outinfo:
                outinfo = 'rest'
            # outinfo='docx.'
            if outinfo.strip('.').find('.') < 0:
                outfile = stem(base(infile))+'.' + outinfo.strip('.')
            #=> outfile=infile.docx
            #equivalent to input params: infile.rest - docx
            else: # - - otherfile.docx
                outfile = outinfo.strip('.')
            if any(outinfo.endswith(x) for x in ['docx', 'odt', 'pdf']):
                sysout = None  # create a file in these cases
            else:
                try:
                    sys.stdout = codecs.getwriter("utf-8")(sys.stdout.detach())
                except: #noqa
                    pass
                sysout = sys.stdout
        elif callable(outfile):
            sysout = outfile()
        else:
            _, ofext = stem_ext(outfile)
            ofext = ofext.strip('.')
            if not outinfo:  # x.rst a/b/c.docx
                outinfo = ofext
            elif outinfo in rst_tools:  # x.rst a/b/c.docx pandoc
                tool = outinfo
                rsttool = rst_tools[outinfo]
                outinfo = ofext
        try:
            if outinfo.endswith('.'):  # x.rest - docx.
                rsttool = None  # ... output the rest code with links for docx
            # drop file information from outinfo
            outinfo = outinfo.strip('.')
            t, outinfo = stem_ext(outinfo)
            if not outinfo:
                outinfo = t
            outinfo = outinfo.strip('.')
        except:
            outinfo = 'rest'

        if _rest.replace('rest','rst').endswith(outinfo.replace('rest','rst')):
            rsttool = None  # no further processing wanted, sysout is final
        if not rsttool and not sysout:
            sysout = opnwrite(outfile)
        tmprestindir = None

        if rsttool:
            finalsysout = sysout
            tmprestindir = infile + _rest # .rest->rest_rest
            sysout = opnwrite(tmprestindir)
            infile = tmprestindir
            atexit.register(rmrf, tmprestindir)
        if sysout:
            sysout.write(_indented_default_role_math(filelines))
            links_done = False
            _links_re = r'^\.\. include:: (.*)(_links_sphinx)(.re?st)'
            rexincludelinks = re.compile(_links_re)
            for x in filelines:
                #x = '.. include:: _links_sphinx.rest' #1
                #x = '.. include:: ../_links_sphinx.rest' #2
                #x = '.. include:: /_links_sphinx.rst' #3
                lim = rexincludelinks.match(x)
                if lim:
                    limg0 = normjoin(lim.groups()[0])
                    limg2 = normjoin(lim.groups()[2])
                    if tool == 'sphinx':
                        links_done = True
                    else:
                        #infile='a/b/c'
                        #outinfo='docx'
                        if limg0.startswith('/'):
                            if limg0 == '/': #find linkroot
                                linkroot = up_dir(lambda x: x.startswith('_links_') or is_project_root_file(x),
                                                  abspath(dirname(infile)))
                                linksfilename = normjoin(linkroot, '_links_' + outinfo + limg2)
                            else:
                                linksfilename = normjoin(limg0, '_links_' + outinfo + limg2)
                        else:
                            linksfilename = normjoin(dirname(infile), limg0, '_links_' + outinfo + limg2)
                        #a/b/_links_docx.rst #1
                        #a/_links_docx.rst #2
                        if not exists(linksfilename):
                            linksfilename = stem(linksfilename)+(_rst if limg2==_rest else _rest)
                        if exists(linksfilename):
                            with opn(linksfilename) as f:
                                if tool == 'rst' and outinfo == 'html':
                                    sysout.write(_rst_id_fix(f.read()))
                                else:
                                    sysout.write(f.read())
                                links_done = True
                else:
                    sysout.write(x if x.endswith('\n') else x+'\n')
            if not links_done:
                sysout.write('\n')
                try:
                    reststem = stem(outfile)
                except:
                    reststem = ''
                for tgt in RstFile.make_tgts(filelines, infile,
                                             make_counters(), fn_i_ln):
                    sysout.write(
                        tgt.create_link(
                            outinfo.replace('rest', 'html'),
                            reststem, tool))

        if rsttool:
            config = conf_py(dirname(infile))
            config.update(kwargs)
            if sysout:
                sysout.close()
                sysout = None
            stdout = rsttool(infile, '-' if finalsysout else outfile,
                             outinfo, **config)
            if stdout is not None and finalsysout:
                finalsysout.write(stdout)
    finally:
        for x in [sysout, finalsysout]:
            if x is not None and x != sys.stdout and not isinstance(
                                                          x, io.StringIO):
                x.close()
        for x in [sysout, finalsysout]:
            if isinstance(x, io.StringIO):
                x.seek(0)
                return x.readlines()


converters = {
    _svg: svgpng,
    _tikz: tikzpng,
    _tex: texpng,
    _dot: dotpng,
    _uml: umlpng,
    _eps: epspng,
    _pyg: pygpng,
    _stpl: dostpl,
    _rst: dorst,
    _rest: dorst,
    _txt: dorst
}
graphic_extensions = {_svg, _tikz, _tex, _dot, _uml, _eps, _pyg}

def convert(
        infile,
        outfile=io.StringIO,
        outinfo=None,
        **kwargs
        ):
    r'''
    Converts any of the known files.

    Stpl files are forwarded to the next converter.

    The main job is to normalized the input params,
    because this is called from |dcx.main| and via Python.
    It forwards to the right converter.

    Examples::

        >>> olddir = os.getcwd()
        >>> cd(dirname(__file__))
        >>> cd('../doc')

        >>> convert([' ','   hi {{2+3}}!'], outinfo='rest')
        ['   .. default-role:: math\n', '\n', ' \n', '   hi 5!\n', '\n']

        >>> convert([' ','   hi {{2+3}}!'])  #doctest: +ELLIPSIS
        ['<!DOCTYPE html>\n', ...]
        >>> rmrf('rest.rest.rest')

        >>> infile, outfile, outinfo = ([
        ... "newpath {{' '.join(str(i)for i in range(4))}} rectstroke showpage"
        ... ],'tst.png','eps')
        >>> 'tst.png' in convert(infile, outfile, outinfo) #doctest: +ELLIPSIS
        True
        >>> exists('tst.png')
        True
        >>> rmrf('tst.png')
        >>> exists('tst.png')
        False

        >>> convert('ra.rest.stpl') #doctest: +ELLIPSIS
        ['<!DOCTYPE html>\n', ...

        >>> cnvrt=lambda x,y: convert(x,y,None,pandoc_doc_optref={'docx':'--reference-doc doc/reference.'+y.split('.')[1]})
        >>> cnvrt('ra.rest.stpl','ra.docx')
        >>> exists('ra.rest.rest')
        True
        >>> rmrf('ra.rest.rest')
        >>> exists('ra.rest.rest')
        False
        >>> exists('ra.docx')
        True
        >>> rmrf('ra.docx')
        >>> exists('ra.docx')
        False

        >>> convert('dd.rest', None,'html') #doctest: +ELLIPSIS
        <!DOCTYPE html>
        ...
        >>> exists('dd.rest.rest')
        True
        >>> rmrf('dd.rest.rest')
        >>> exists('dd.rest.rest')
        False
        >>> cd(olddir)


    :param infile:
        any of ``.tikz``, ``.svg``, ``.dot``, ``.uml``, ``.eps``, ``.pyg``
        or else stpl is assumed. Can be list of lines, too.

    :param outfile: ``-`` means standard out,
        else a file name, or None for automatic (using outinfo),
        or io.StringIO to return lines instead of stdout

    :param outinfo:
        ``html``, ``sphinx_html``, ``docx``, ``odt``, ``file.docx``,...
        interpet input as rest, else specifies graph type

    '''

    afile = False
    try:
        afile = infile and isfile(infile) or False
    except:
        pass
    if not afile and (infile == '-' or infile is None):
        try:
            sys.stdin = codecs.getreader("utf-8")(sys.stdin.detach())
        except:
            pass
        infile = sys.stdin.readlines()
    if not outinfo:
        if outfile == '-':
            outinfo = 'rest'
        elif outfile is None or callable(outfile):
            outinfo = 'html'
        else:
            _,outinfo = stem_ext(outfile)
            outinfo = outinfo.strip('.')
    fext = None
    if isinstance(infile, str):
        nextinfile, fext = stem_ext(infile)
    else:
        fext = _stpl
        if outinfo and _is_graphic(outinfo):
            soi = outinfo.strip('.')
            nextinfile = soi + '.' + soi
        else:
            nextinfile = 'rest' + _rest
    fn_i_ln = None
    while fext in converters:
        if (outfile is None or callable(outfile)) and _is_graphic(fext):
                outfile = _imgout(nextinfile + fext)
        try:
            nextinfile, fextnext = stem_ext(nextinfile)
            if fextnext not in converters:
                fextnext = None
        except:
            fextnext = None
        out_ = lambda:outfile if not fextnext else None
        thisconverter = converters[fext]
        if thisconverter == dorst:
            kwargs['fn_i_ln'] = fn_i_ln
            kwargs.pop('outfile',None)
            kwargs.pop('outinfo',None)
            infile = thisconverter(infile, out_(), outinfo, **kwargs)
        else:
            if thisconverter == dostpl:
                kwargs['outinfo'] = outinfo
                # save infile for dorst() in outinfo as "infile/outinfo"
                if fextnext in converters and converters[fextnext] == dorst:
                    if isinstance(infile, str):
                        fn_i_ln = _flatten_stpl_includes(infile)
                    else:
                        fn_i_ln = list(_flatten_stpl_includes_it(infile))
                    outinfo = nextinfile + '/' + (outinfo or '') # infile_outinfo
            infile = thisconverter(infile, out_(), **kwargs)
        if not infile:
            break
        if not fextnext:
            break
        fext = fextnext
    return infile


'''
Same as |dcx.convert|,
but creates temporary dir for a list of lines infile argument.

::

    >>> tmpfile = convert_in_tempdir("""digraph {
    ... %for i in range(3):
    ...    "From {{i}}" -> "To {{i}}";
    ... %end
    ...    }""".splitlines(), outinfo='dot')
    >>> stem_ext(tmpfile)[1]
    '.png'
    >>> tmpfile = convert_in_tempdir("""
    ... This is re{{'st'.upper()}}
    ...
    ... .. _`xx`:
    ...
    ... xx:
    ...     text
    ...
    ... """.splitlines(), outinfo='rst_html')
    >>> stem_ext(tmpfile)[1]
    '.html'

'''
convert_in_tempdir = in_temp_if_list(infile_cwd(convert))


def rindices(regex, lns):
    r'''
    Return the indices matching the regular expression ``regex``.

    :param regex: regular expression string or compiled
    :param lns: lines

    ::

        >>> lns=['a','ab','b','aa']
        >>> [lns[i] for i in rindices(r'^a\w*', lns)]==['a', 'ab', 'aa']
        True

    '''

    regex = re.compile(regex)
    for i, ln in enumerate(lns):
        if regex.search(ln):
            yield i


def rlines(regex, lns):
    '''
    Return the lines matched by ``regex``.

    :param regex: regular expression string or compiled
    :param lns: lines

    '''

    return [lns[i] for i in rindices(regex, lns)]


def intervals(nms  # list of indices
              ):
    """
    Return intervals between numbers.

    ::

        >>> intervals([1, 2, 3])==[(1, 2), (2, 3)]
        True

    """
    return list(zip(nms[:], nms[1:]))


def in2s(nms  # list of indices
         ):
    """
    Convert the list into a list of couples of two elements.

    ::

        >>> in2s([1, 2, 3, 4])==[(1, 2), (3, 4)]
        True

    """
    return list(zip(nms[::2], nms[1::2]))


# re.search(reid,'OpenDevices = None').groups()
# re.search(reid,'def OpenDevices(None)').groups()
# re.search(reid,'class OpenDevices:').groups()
# re.search(reid,'    def __init__(a, b):').groups()
# re.search(relim,"  '''prefix. ").groups()
# re.search(relim,"  '''").groups()


def doc_parts(
        lns,
        relim=r"^\s*r?'''([\w.:]*)\s*\n*$",
        reid=r"\s(\w+)[(:]|(\w+)\s\=",
        reindent=r'[^#/\s]',
        signature=None,
        prefix=''
        ):
    r'''
    ``doc_parts()`` yields doc parts delimeted by ``relim`` regular expression
    possibly with id, if ``reid`` matches

    If start and stop differ use regulare expression ``|`` in ``relim``.

    - There is no empty line between doc string
      and preceding code lines that should be included.
    - There is no empty line between doc string
      and succeeding code lines that should be included.
    - Included code lines end with an empty line.

    In case of ``__init__()`` the ID can come from the ``class`` line
    and the included lines can be those of ``__init__()``,
    if there is no empty line between the doc string
    and ``class`` above as well as ``_init__()`` below.

    If the included code comes only from one side of the doc string,
    have an empty line at the other side.

    Immediately after the initial doc string marker
    there can be a prefix, e.g. ``classname.``.

    :param lns: list of lines
    :param relim: regular expression marking lines enclosing the documentation.
        The group is a prefix.
    :param reid: extract id from preceding or succeeding non-empty lines
    :param reindent: determines start of text
    :param signature: if signature language is given the preceding
        or succeeding lines will be included
    :param prefix: prefix to make id unique, e.g. module name. Include the dot.

    ::

        >>> with open(__file__) as f:
        ...     lns = f.readlines()
        ...     docparts = list(doc_parts(lns, signature='py'))
        ...     doc_parts_line = rlines('doc_parts', docparts)
        >>> doc_parts_line[1]
        ':doc_parts:\n'

    '''

    rlim = re.compile(relim)
    rid = re.compile(reid)
    rindent = re.compile(reindent)

    def foundid(lnsi):
        if not lnsi.strip():  # empty
            return False
        id = rid.search(lnsi)
        if id and id.groups():
            ids = [x for x in id.groups() if x is not None]
            if len(ids) > 0:
                return ids[0]

    ids = []

    def checkid(rng):
        i = None
        for i in rng:
            testid = foundid(lns[i])
            if testid is False:
                break
            elif not ids and isinstance(testid, str):
                ids.append(testid)
        return i

    for a, b in in2s(list(rindices(rlim, lns))):
        try:
            thisprefix = rlim.search(lns[a]).groups()[0]
        except:
            thisprefix = ''
        ids.clear()
        i = checkid(range(a - 1, 0, -1))
        j = checkid(range(b + 1, len(lns)))
        if ids:
            yield ''
            yield '.. _`' + prefix + thisprefix + ids[0] + '`:\n'
            yield ''
            yield ':' + prefix + thisprefix + ids[0] + ':\n'
            yield ''
        if signature:
            if i is not None and i < a and i > 0:
                if not lns[i].strip():  # empty
                    i = i + 1
                if i < a:
                    yield '.. code-block:: ' + signature + '\n'
                    yield ''
                    yield from ('   ' + x for x in lns[i:a])
                    yield ''
            if j is not None and j > b + 1 and j < len(lns):
                if not lns[j].strip():  # empty
                    j = j - 1
                if j > b:
                    yield '.. code-block:: ' + signature + '\n'
                    yield ''
                    yield from ('   ' + x for x in lns[b + 1:j + 1])
                    yield ''
        indent = 0
        for ln in lns[a + 1:b]:
            lnst = rindent.search(ln)
            if lnst and lnst.span():
                indent = lnst.span()[0]
                break
        yield from (x[indent:] for x in lns[a + 1:b])


# for generator function, instead of lru_cache()
_Tee = tee([], 1)[0].__class__


def _memoized(f):
    cache = {}

    def ret(*args):
        if args not in cache:
            cache[args] = f(*args)
        if isinstance(cache[args], (GeneratorType, _Tee)):
            cache[args], r = tee(cache[args])
            return r
        return cache[args]

    return ret


@lru_cache()
def _read_lines(fn):
    lns = []
    with opn(fn) as f:
        lns = list(f.readlines())
    return lns


@_memoized
def rstincluded(
        fn,
        paths=(),
        withimg=False,
        withrest=False
        ):
    '''
    Yield the files recursively included from an RST file.

    :param fn: file name without path
    :param paths: paths where to look for fn
    :param withimg: also yield image files, not just other RST files
    :param withrest: rest files are not supposed to be included

    ::

        >>> olddir = os.getcwd()
        >>> cd(dirname(__file__))
        >>> list(rstincluded('ra.rest',('../doc',)))
        ['ra.rest.stpl', '_links_sphinx.rst']
        >>> list(rstincluded('sr.rest',('../doc',)))
        ['sr.rest', '_links_sphinx.rst']
        >>> list(rstincluded('meta.rest',('../doc',)))
        ['meta.rest', 'files.rst', '_traceability_file.rst', '_links_...']
        >>> 'dd.rest' in list(rstincluded(
        ... 'index.rest',('../doc',), False, True))
        True
        >>> cd(olddir)

    '''

    p = ''
    for p in paths:
        nfn = normjoin(p, fn)
        if exists(nfn + _stpl):  # first, because original
            nfn = nfn + _stpl
            yield fn + _stpl
            break
        elif exists(nfn):  # while this might be generated
            yield fn
            break
    else:
        nfn = fn
        yield fn
    lns = _read_lines(nfn)
    toctree = False
    if lns:
        for aln in lns:
            if toctree:
                toctreedone = False
                if aln.startswith(' '):
                    fl = aln.strip()
                    if fl.endswith(_rest) and exists(normjoin(p, fl)):
                        toctreedone = True
                        yield from rstincluded(fl, paths)
                    continue
                elif toctreedone:
                    toctree = False
            if aln.startswith('.. toctree::'):
                if withrest:
                    toctree = True
            elif aln.strip().startswith('.. '):
                # aln = '  .. include:: some.rst'
                # aln = '  .. include:: ../some.rst'
                # aln = '.. include:: some.rst'
                # aln = '.. include:: ../some.rst'
                # aln = '  .. image:: some.png'
                # aln = '.. image:: some.png'
                # aln = '  .. figure:: some.png'
                # aln = '  .. |x y| image:: some.png'
                try:
                    f, t, _ = rerstinclude.split(aln)
                    nf = not f.strip() and t
                    if nf:
                        if is_rest(nf) and not withrest:
                            continue
                        yield from rstincluded(nf.strip(), paths)
                except:
                    if withimg:
                        m = reximg.search(aln)
                        if m:
                            yield m.group(1)
            elif restplinclude.match(aln):
                # aln="%include('some.rst.tpl', v='param')"
                # aln="   %include('some.rst.tpl', v='param')"
                f, t, _ = restplinclude.split(aln)
                nf = not f.strip() and t
                if nf:
                    thisnf = normjoin(p, nf)
                    if not exists(thisnf):
                        parntnf = normjoin(p, '..', nf)
                        if exists(parntnf):
                            nf = parntnf
                        else:
                            continue
                    yield from rstincluded(nf.strip(), paths)


_traceability_instance = None


class Traceability:
    def __init__(self, tracehtmltarget):
        self.tracehtmltarget = tracehtmltarget
        self.fcaobjsets = []
        global _traceability_instance
        _traceability_instance = self
        self.counters = None

    def appendobject(self, aset):
        self.fcaobjsets.append(aset)

    def isempty(self):
        return len(self.fcaobjsets) == 0

    # returns the rst lines of _traceability_file
    def create_traceability_file(self, linkroot):
        if not pyfca:
            return []
        if not self.fcaobjsets:
            return []
        config = conf_py(linkroot)
        target_id_group = config['target_id_group']
        target_id_color = config['target_id_color']
        rextrace_target_id = re.compile(config['rextrace_target_id'])

        def _drawnode(canvas, node, parent, center, radius):
            fillcolors = []
            nodetgtgrps = {target_id_group(x) for x in node.intent}
            for _, (groupid, groupcolor) in target_id_color.items():
                if groupid in nodetgtgrps:
                    fillcolors.append(groupcolor)
            n_grps = len(fillcolors)
            for i in range(n_grps - 1, -1, -1):
                rr = int(radius * (i + 1) / n_grps)
                parent.add(
                    canvas.circle(
                        center, rr, fill=fillcolors[i], stroke='black'))

        fca = pyfca.Lattice(self.fcaobjsets,
                            lambda x: set(xe for xe in x if rextrace_target_id.match(xe)))
        tr = 'tr'

        # |trXX|, |trYY|, ...

        def reflist(x, pfx=tr):
            return (
                '|' + pfx +
                ('|, |' + pfx).join([str(e)
                                     for e in sorted(x)]) + '|') if x else ''

        fcanodes = [(".. _`" + tr + "{0}`:\n\n:" + tr +
                     "{0}:\n\n{1}\n\nUp: {2}\n\nDown: {3}\n\n").format(
                         n.index, reflist(n.intent, ''), reflist(n.up),
                         reflist(n.down)) for n in fca.nodes]
        tlines = ''.join(fcanodes).splitlines(keepends=True)
        # fig_traceability_file target
        tlines.extend([
            '.. _`fig' + _traceability_file + '`:\n', '\n',
            '.. figure:: ' + _traceability_file + '.png\n', '   :name:\n', '\n',
            '   |fig' + _traceability_file + '|: `FCA <%s>`__ %s' % (
                "https://en.wikipedia.org/wiki/Formal_concept_analysis",
                "diagram of dependencies"
                )
        ])
        if target_id_color is not None:
            legend = ', '.join(
                [fnm + " " + clr for fnm, (_, clr) in target_id_color.items()])
            tlines.extend([': ' + legend, '\n'])
        tlines.append('\n')
        with opnwrite(normjoin(linkroot, _traceability_file + _rst)) as f:
            f.write('.. raw:: html\n\n')
            f.write('    <object data="' + _traceability_file + _svg +
                    '" type="image/svg+xml"></object>\n')
            if target_id_color is not None:
                f.write(
                    '''    <p><a href="%s">FCA</a>
                  diagram of dependencies with clickable nodes: ''' % (
                      "https://en.wikipedia.org/wiki/Formal_concept_analysis"
                    )
                    + legend + '</p>\n\n')
            f.writelines(tlines)
        ld = pyfca.LatticeDiagram(fca, 4 * 297, 4 * 210)

        tracesvg = abspath(normjoin(linkroot, _traceability_file + _svg))

        def ttgt():
            return self.tracehtmltarget.endswith(_rest) and stem(
                self.tracehtmltarget) or self.tracehtmltarget

        ld.svg(
            target=ttgt() + '.html#' + tr, drawnode=_drawnode).saveas(tracesvg)
        if exists(tracesvg):
            tracepng = abspath(normjoin(linkroot, _traceability_file + _png))
            svgpng(tracesvg, tracepng)
        return tlines


def pair(alist, blist, cmp):
    '''
    pair two sorted lists
    where the second must be at least as long as the first

    :param alist: first list
    :param blist: second list longer or equal to alist
    :param cmp: compare function

    ::

        >>> alist=[1,2,4,7]
        >>> blist=[1,2,3,4,5,6,7]
        >>> cmp = lambda x,y: x==y
        >>> list(pair(alist,blist,cmp))
        [(1, 1), (2, 2), (None, 3), (4, 4), (None, 5), (None, 6), (7, 7)]

        >>> alist=[1,2,3,4,5,6,7]
        >>> blist=[1,2,3,4,5,6,7]
        >>> cmp = lambda x, y: x==y
        >>> list(pair(alist, blist, cmp))
        [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7)]

    '''

    i = 0
    for aa, bb in zip(alist, blist):
        if not cmp(aa, bb):
            break
        i = i + 1
        yield aa, bb
    alen = len(alist)
    tlen = max(alen, len(blist))
    d = 0
    for j in range(i, alen):
        for dd in range(tlen - j - d):
            bb = blist[j + d + dd]
            if not cmp(alist[j], bb):
                yield None, bb
            else:
                yield alist[j], bb
                d = d + dd
                break
        else:
            return


def gen(
        source,
        target=None,
        fun=None,
        **kw
        ):
    '''
    Take the ``gen_[fun]`` functions
    enclosed by ``#def gen_[fun](lns,**kw)`` to create a new file.

    :param source: either a list of lines or a path to the source code
    :param target: either save to this file
        or return the generated documentation
    :param fun: use ``#gen_<fun>(lns,**kw):`` to extract the documentation
    :param kw: kw arguments to the ``gen_<fun>()`` function

    ::

        >>> source=[i+'\\n' for i in """
        ...        #def gen(lns,**kw):
        ...        #  return [l.split('#@')[1] for l in rlines(r'^\s*#@', lns)]
        ...        #def gen
        ...        #@some lines
        ...        #@to extract
        ...        """.splitlines()]
        >>> [l.strip() for l in gen(source)]
        ['some lines', 'to extract']

    '''

    if isinstance(source, str):
        lns = []
        try:
            lns = _read_lines(source)
        except:
            sys.stderr.write("ERROR: {} cannot be opened\n".format(source))
            return
    else:
        lns = source
        source = ""
    if '.' not in sys.path:
        sys.path.append('.')
    if fun:
        # fun ='sdf'
        gen_regex = r'#\s*def gen_' + fun + r'(\w*(lns,\*\*kw):)*'
        # re.compile(gen_regex).search('#def gen_sdf(lns,**kw):') #begin
        # re.compile(gen_regex).search('#def gen_sdf') #end
    else:
        gen_regex = r'#\s*def gen(\w*(lns,\*\*kw):)*'
        # re.compile(gen_regex).search('# def gen(lns,**kw):') #begin
        # re.compile(gen_regex).search('# def gen') #end
    iblks = list(rindices(gen_regex, lns))
    py3 = [
        lns[k][lns[i].index('#') + 1:] for i, j in in2s(iblks)
        for k in range(i, j)
    ]
    indent = py3[0].index(py3[0].lstrip())
    py3 = '\n'.join(x[indent:] for x in py3)
    eval(compile(py3, source + r'#\s*gen', 'exec'), globals())
    if fun:
        gened = list(eval('gen_' + fun + '(lns,**kw)'))
    else:  # else eval all gen_ funtions
        gened = []
        for i in iblks[0::2]:
            gencode = re.split(r"#\s*def |:", lns[i])[1]  # gen(lns,**kw)
            gened += list(eval(gencode))
    if target:
        drn = dirname(target)
        if drn and not exists(drn):
            mkdir(drn)
        with opnwrite(target) as o:
            o.write(''.join(((x or '\n') for x in gened)))
    else:
        return gened


def parsegenfile(genpth):
    '''
    Parse the file ``genpth`` which is either 

    - python code or

    - has format ::

      sourcefile | targetfile | suffix | kw paramams or {}

    ``suffix`` refers to ``gen_<suffix>``.

    The yields are used for the |dcx.gen| function.

    :param genpth: path to gen file

    '''

    try:
        genfilelns = _read_lines(genpth)
    except: #noqa
        sys.stderr.write("ERROR: {} cannot be opened\n".format(genpth))
        return

    try: #python code return [[from,to,fun,kw],...]?
        genconfig= {'__file__':abspath(genpth)}
        gencode = '\n'.join(genfilelns)
        eval(compile(gencode, genpth, 'exec'), genconfig)
        for f,t,d,kw in genconfig['from_to_fun_kw']:
            yield f,t,d,kw # if python code last entry is not a string as blow
    except:
        for ln in genfilelns:
          if ln[0] != '#':
              try:
                  f, t, d, a = [x.strip() for x in ln.split('|')]
                  kw = eval(a)
                  yield f, t, d, kw
              except:
                  pass


def _flatten_stpl_includes_it(fn):
    """
    This flattens the .stpl includes
    to have all targets align to those in the RST file.
    Targets must be *explicit* in all ``.stpl`` and ``.tpl``,
    i.e. they must not be created by stpl code.
    This is needed to make the .tags jump to the original
    and not the generated file.
    """
    flns = []
    if isinstance(fn, str):
        if exists(fn):
            flns = _read_lines(fn)
        else:
            parnt = updir(fn)
            if exists(parnt):
                flns = _read_lines(parnt)
    else:
        flns = fn
        fn = '-'
    for i, ln in enumerate(flns):
        # ln = '% include("../test.rst.stpl", v="aparam")'
        m = restplinclude.match(ln)
        if m:
            includedtpl = m.group(1)
            yield from _flatten_stpl_includes(
                normjoin(dirname(fn), includedtpl))
        else:
            yield fn, i, ln


@lru_cache()
def _flatten_stpl_includes(fn):
    return list(_flatten_stpl_includes_it(fn))


class Tgt:

    line_search_range = 8

    def __init__(
            self,
            lnidx,  # line index
            target  # target name
    ):
        self.lnidx = lnidx
        self.target = target
        self.tagentry = None  # (path, line index)
        self.lnkname = None  # link name

    def is_inside_literal(self, lns):
        try:  # skip literal blocks
            indentation = re.search(r'\w', lns[self.lnidx]).span()[0] - 3
            if indentation > 0:
                for iprev in range(self.lnidx - 1, 0, -1):
                    prev = lns[iprev]
                    if prev:
                        newspc, _ = next((ich, ch)
                                         for ich, ch in enumerate(prev)
                                         if ch != ' ' and ch != '\t')
                        if newspc < indentation:
                            prev = prev.strip()
                            if prev:
                                if not prev.startswith(
                                        '.. ') and prev.endswith('::'):
                                    return True
                                return False
        except:
            pass

    def find_lnkname(self,
                     lns,
                     counters
                     ):
        """Tgt.

        Determines the link name for this target.
        It searches the following lines for either

        - a title
        - ``:name:`` immediately below a directive
          (a counter is used if no name is given)
        - a ``:xxx:`` or ``xxx:`` or
        - a single word ``xxx``

        The link name must not contain formatting,
        e.g. "``link name``:" is not possible.

        :param lns: the rest lines
        :param counters: the counters for the directives (see make_counters())

        """
        lenlns = len(lns)
        lnkname = self.target
        for j in range(self.lnidx + 2, self.lnidx + self.line_search_range):
            # j=i+2
            if j > lenlns - 1:
                break
            lnj = lns[j]
            if rextitle.match(lnj):
                lnkname = lns[j - 1].strip()
                if not lnkname:
                    lnkname = lns[j + 1].strip()
                break
            # j, lns=1,".. figure::\n  :name: linkname".splitlines();lnj=lns[j]
            # j, lns=1,".. figure::\n  :name:".splitlines();lnj=lns[j]
            # j, lns=1,".. math::\n  :name: linkname".splitlines();lnj=lns[j]
            itm = rexname.match(lnj)
            if itm:
                lnkname, = itm.groups()
                lnj1 = lns[j - 1].split('::')[0].replace(
                    'list-table', 'table').replace('code-block',
                                                   'code').strip()
                if counters and not lnkname and lnj1 in counters:
                    lnkname = name_from_directive(
                        lnj1.strip('. '), counters[lnj1])
                    counters[lnj1] += 1
                    break
                elif lnkname:
                    lnkname = lnkname.strip()
                    break
            itm = rexitem.match(lnj)
            if itm:
                lnkname, = itm.groups()
                break
            itm = rexoneword.match(lnj)
            if itm:
                lnkname, = itm.groups()
                break
            lnkname = self.target
        self.lnkname = lnkname

    def create_link(self,
                    linktype,
                    reststem,
                    tool
                    ):
        """Tgt.

        Creates a link.
        If both linktype and reststem are empty,
        then this is an internal link.

        :param linktype: file extension:
            one of rest, html, docx, odt, latex, pdf
        :param reststem:  the file name without extension
            (not used for linktype='sphinx' or 'rest')
        :param tool: pandoc, sphinx or rst

        """
        if reststem and linktype:
            targetfile = reststem + '.' + linktype
        else:
            targetfile = ''
        id = self.target
        if linktype == 'latex':
            linktype = 'pdf'
        if tool == 'sphinx':
            tgte = ".. |{0}| replace:: :ref:`{1}<{2}>`\n".format(
                self.target, self.lnkname, id)
        else:
            if linktype == 'odt':
                # https://github.com/jgm/pandoc/issues/3524
                tgte = ".. |{0}| replace:: `{1} <file:../{2}#{3}>`__\n".format(
                    self.target, self.lnkname, targetfile, id)
            else:
                # https://sourceforge.net/p/docutils/bugs/378/
                tgte = ".. |{0}| replace:: `{1} <file:{2}#{3}>`__\n".format(
                    self.target, self.lnkname, targetfile, id)
        if tool == 'rst' and linktype == 'html':
            return _rst_id_fix(tgte)
        else:
            return tgte

    def create_tag(self):
        return r'{0}	{1}	/\.\. _`\?{0}`\?:/;"		line:{2}'.format(
            self.target, self.tagentry[0], self.tagentry[1])


class RstFile:
    def __init__(self, reststem, doc, tgts, lnks, nlns):
        '''RstFile.

        Contains the targets for a ``.rst`` or ``.rest`` file.

        :param reststem: rest file this doc belongs to (without extension)
        :param doc: doc belonging to reststem,
            either included or itself (.rest, .rst, .stpl)
        :param tgts: list of Tgt objects yielded by |dcx.RstFile.make_tgts|.
        :param lnks: list of (line index, target name (``|target|``)) tuples
        :param nlns: number of lines of the doc

        '''

        self.reststem = reststem
        self.doc = doc
        self.tgts = tgts
        self.lnks = lnks
        self.nlns = nlns

    def __str__(self):
        return str((self.doc, self.reststem))

    def add_links_and_tags(self, add_tgt, add_linksto):
        iterlnks = iter(self.lnks)
        prevtgt = None
        # unknowntgts = []
        tgt = None
        for tgt in self.tgts:
            if tgt.lnidx is not None:
                add_linksto(prevtgt, tgt.lnidx, iterlnks)  # , unknowntgts)
                add_tgt(tgt, self.reststem)
                prevtgt = tgt
        if tgt:
            add_linksto(prevtgt, tgt.lnidx, iterlnks)  # , unknowntgts)

    @staticmethod
    def make_lnks(lns  # lines of the document
                  ):
        """RestFile.

        Yields (index, link name) for ``lns``.

        """

        for i, ln in enumerate(lns):
            mo = rexlnks.findall(ln)
            for g in mo:
                yield i, g

    @staticmethod
    def make_tgts(
            lns,
            doc,
            counters=None,
            fn_i_ln=None
            ):
        '''RstFile.

        Yields ``((line index, tag address), target, link name)``
        of ``lns`` of a restructureText file.
        For a .stpl file the linkname comes from the generated RST file.

        :param lns: lines of the document
        :param doc: the rst/rest document for tags
        :param counters: if None, the starts with
            {".. figure":1,".. math":1,".. table":1,".. code":1}
        :fn_i_ln: (fn, i, ln) of the .stpl with all stpl includes sequenced

        '''

        if counters is None:
            counters = make_counters()
        itgts = list(rindices(rextgt, lns))
        if fn_i_ln:
            lns1 = [x[2] for x in fn_i_ln]
            itgts1 = list(rindices(rextgt, lns1))
        else:
            lns1 = lns
            itgts1 = itgts
        if len(itgts) < len(itgts1):
            paired_itgts_itgts1 = pair(itgts, itgts1,
                                       lambda x, y: lns[x] == lns1[y])
        elif len(itgts) > len(itgts1):
            paired_itgts_itgts1 = ((i, j) for (
                j, i) in pair(itgts1, itgts, lambda x, y: lns1[x] == lns[y]))
        else:
            paired_itgts_itgts1 = zip(itgts, itgts1)
        lenlns = len(lns)
        lenlns1 = len(lns1)
        for i, i1 in paired_itgts_itgts1:
            ii, iis, _ = (i, lns, lenlns) if i else (i1, lns1, lenlns1)
            cur = iis[ii]
            tgt = Tgt(ii, rextgt.search(cur).group(1))
            if tgt.is_inside_literal(iis):
                continue
            tgt.find_lnkname(iis, counters)
            tgt.lnkidx = i
            if i1:
                if fn_i_ln:
                    tgt.tagentry = fn_i_ln[i1][:2]
                else:
                    tgt.tagentry = (doc, ii)
            else:
                tgt.tagentry = (doc.replace(_stpl, ''), ii)
            yield tgt

    @staticmethod
    def substs(lns  # lines of the rst document
               ):
        """RestFile.

        Return all substitution targets in the rst lns

        ::

            >>> list(RstFile.substs('''
            ...   .. |sub| image:: xx
            ...   .. |s-b| date::
            ...   '''.splitlines()))
            ['sub', 's-b']

        """

        for i, ln in enumerate(lns):
            asub = rexsubtgt.search(ln)
            if asub:
                yield asub.group(1)


g_links_types = "sphinx latex html pdf docx odt".split()
class Fldr(OrderedDict):
    def __init__(
            self,
            folder,
            linkroot,
            scanroot='.'
            ):
        """
        Represents a directory.

        It is an ordered list of {rest file: RstFile object}.

        :self.folder: is the directory path
        :self.linkroot: is the directory relative to which links are made
        :self.scanroot: is the directory where the scan began
        :self.allfiles: set of all files in the directory
        :self.alltgts: set of all targets in the directory
        :self.allsubsts: set of all substitutions in the directory
        :self.counters: the counters for each rest file

        """

        self.folder = folder
        self.linkroot = linkroot
        self.scanroot = scanroot
        self.allfiles = set()
        self.alltgts = set()
        self.allsubsts = set()
        self.rest_counters = defaultdict(dict)

    def __str__(self):
        return str(list(sorted(self.keys())))

    def scanfiles(
            self,
            fs
            ):
        """Fldr.

        Scans the directory for rest files.
        All files (.rest and included .rst)
        are added if there is at least one ``.rest[.stpl]``.

        :param fs:  all files in the directory as returned by ``os.walk()``

        Sphinx index.rest is processed last.

        ``allfiles``, ``alltgts`` and ``allsubsts`` get filled.

        """

        sofar = set([])
        sphinx_index = None
        # reversed puts the rest.stpl before the .rest
        for afs in reversed(sorted(fs)):
            fullpth = normjoin(self.folder, afs).replace("\\", "/")
            if is_rest(afs):
                if afs.startswith('index'+_rest):
                    sphinx_index = afs
                    continue
                fullpth_nostpl = fullpth.replace(_stpl, '')
                if fullpth_nostpl in sofar:
                    continue
                sofar.add(fullpth_nostpl)
                self.add_rest(afs)
        if sphinx_index:
            self.add_rest(sphinx_index)

    def add_rest(self,
                 restfile,
                 exclude_paths_substrings=['_links_', _traceability_file]):
        """Fldr.

        Scans a rest file for included files and constructs all the targets.

        """

        pths = []
        has_traceability = False
        for restinc in rstincluded(restfile, (self.folder, )):
            pth = normjoin(self.folder, restinc).replace("\\", "/")
            if _traceability_file + _rst in restinc:
                if pyfca and _traceability_instance is None:
                    Traceability(stem(restfile))
                    has_traceability = True
                    continue
            if any(x in pth for x in exclude_paths_substrings):
                continue
            pths.append(pth)

        assert pths, "No file for "+restfile+" due to excluded " + str(exclude_paths_substrings)
        reststem = pths[0]
        reststem = stem(stem(reststem))
        if reststem not in self.rest_counters:
            self.rest_counters[reststem] = make_counters()
        counters = self.rest_counters[reststem]
        if has_traceability:
            _traceability_instance.counters = counters

        self.allfiles |= set(pths)

        for doc in pths:
            rstpath = doc.replace(_stpl, '')
            if doc.endswith(_stpl) and exists(rstpath):
                lns = _read_lines(doc.replace(_stpl, ''))
                fn_i_ln = _flatten_stpl_includes(doc)
                tgts = list(RstFile.make_tgts(lns, doc, counters, fn_i_ln))
            elif not doc.endswith(_tpl) and not doc.endswith(_txt) and exists(
                    doc):
                lns = _read_lines(doc)
                tgts = list(RstFile.make_tgts(lns, doc, counters))
            else:
                continue
            lnks = list(RstFile.make_lnks(lns))
            relp = relpath(reststem,start=self.linkroot)
            rstfile = RstFile(relp, doc, tgts, lnks, len(lns))
            self[doc] = rstfile
            self.alltgts |= set([t.target for t in rstfile.tgts])
            self.allsubsts |= set(RstFile.substs(lns))

    def create_links_and_tags(self):
        """Fldr.

        Appends to links_xxx.rst and .tags in linkroot for files in this folder.

        The target IDs are grouped using target_id_group().
        To every group a color is associated. See ``conf.py``.
        This is used to color an FCA lattice diagram
        in "_traceability_file.rst".
        The diagram nodes are clickable in HTML.

        """

        tagentries = []
        lnkrelfolder = ''
        if self.folder.strip():
            lnkrelfolder = relpath(self.linkroot, start=self.folder)
        linkfiles = [(linktype, []) for linktype in g_links_types]

        def add_tgt(tgt, reststem):
            for linktype, linklines in linkfiles:
                linklines.append(
                    tgt.create_link(
                        linktype, normjoin(lnkrelfolder,reststem),
                        linktype if linktype == 'sphinx' else 'pandoc'))
            if isabs(tgt.tagentry[0]):
                tgt.tagentry = (relpath(tgt.tagentry[0], start=self.scanroot),
                                tgt.tagentry[1])
            tgt.tagentry = (tgt.tagentry[0], tgt.tagentry[1])
            newtag = tgt.create_tag()
            tagentries.append(newtag)

        def add_links_comments(comment):
            for _, linklines in linkfiles:
                linklines.append(comment)

        def add_linksto(prevtgt, lnidx, iterlnks, ojlnk=[0, None]):
            # all the links from the block following prevtgt up to this tgt
            linksto = []

            def chkappend(x):
                if not prevtgt or x != prevtgt.target:
                    linksto.append(x)

            if ojlnk[1] and ojlnk[0] < lnidx:  # first link in the new prevtgt
                if ojlnk[1] in self.alltgts:
                    chkappend(ojlnk[1])
                elif ojlnk[1] not in self.allsubsts:
                    linksto.append('-' + ojlnk[1])
                    # unknowntgts.append(ojlnk[1])
                ojlnk[1] = None
            if ojlnk[1] is None:  # remaining links in prevtgt up to this tgt
                for j, lnk in iterlnks:
                    if j > lnidx:  # links up to to this target
                        ojlnk[:] = j, lnk
                        break
                    else:
                        if lnk in self.alltgts:
                            chkappend(lnk)
                        elif lnk not in self.allsubsts:
                            linksto.append('-' + lnk)
                            # unknowntgts.append(lnk)
            if _traceability_instance:
                if prevtgt and linksto:
                    _traceability_instance.appendobject(linksto+[prevtgt.target])
            if linksto:
                linksto = '.. .. ' + ','.join(linksto) + '\n\n'
                add_links_comments(linksto)

        for rstfile in self.values():
            add_links_comments('\n.. .. {0}\n\n'.format(rstfile.doc))
            rstfile.add_links_and_tags(add_tgt, add_linksto)
        if _traceability_instance and self.linkroot==self.folder:
            tlines = _traceability_instance.create_traceability_file(self.linkroot)
            trcrst = normjoin(self.linkroot, _traceability_file + _rst)
            if tlines:
                for tgt in RstFile.make_tgts(tlines, trcrst,
                                             _traceability_instance.counters):
                    add_tgt(tgt, _traceability_instance.tracehtmltarget)
        for linktype, linklines in linkfiles:
            with opnappend(normjoin(self.linkroot,
                                   '_links_'+linktype+_rst)) as f:
                f.write('\n'.join(linklines))
        ctags_python = ""
        try:
            ctags_python = cmd(
                [
                    'ctags', '-R', '--sort=0', '--fields=+n',
                    '--languages=python', '--python-kinds=-i', '-f', '-', '*'
                ],
                cwd=self.scanroot)
        finally:
            with opnappend(normjoin(self.scanroot, '.tags')) as f:
                if ctags_python: f.write(ctags_python)
                if tagentries: f.write('\n'.join(tagentries)+'\n')


class Fldrs(OrderedDict):
    def __init__(
            self,
            scanroot='.'
            ):
        """
        Represents a directory hierarchy below ``scanroot``.

        :param scanroot: root path to start scanning
            for independent doc directories

        .tags: paths are relative to ``scanroot``.

        _links_xxx.rst: paths are relative to the first directory with a ``.rest``.
        Place e.g. index.rest in a folder above, to link between folders.

        """

        self.scanroot = scanroot

    def __str__(self):
        return super().__str__()

    def scandirs(self):
        #_images, and dot files excluded
        notexcluded = lambda d: not d.startswith('_') and not (len(d)>1 and d[0]=='.' and d[1]!='.')
        linkroot = None
        for p, ds, fs in os.walk(self.scanroot):
            ds[:] = [d for d in ds if notexcluded(d)]
            if notexcluded(base(p)):
                njp = normjoin(p)
                fldr = Fldr(njp,linkroot or njp,self.scanroot)
                fldr.scanfiles(fs)
                if len(fldr):
                    self[njp] = fldr
                    if not linkroot or not abspath(njp).startswith(abspath(linkroot)):
                        linkroot = njp
                        for linktype in g_links_types:
                            with opnwrite(normjoin(linkroot,
                                   '_links_'+linktype+_rst)) as f:
                                f.write('.. .. .. %s'%linkroot)
                            rmrf(normjoin(self.scanroot, '.tags'))

def links_and_tags(
    scanroot='.'
    ):
    '''
    Creates ``_links_xxx.rst`` files and a ``.tags``.

    :param scanroot: directory for which to create links and tags

    ::

        >>> olddir = os.getcwd()
        >>> cd(dirname(__file__))
        >>> rmrf('../doc/_links_sphinx.rst')
        >>> '_links_sphinx.rst' in ls('../doc')
        False

        >>> links_and_tags('../doc')
        >>> '_links_sphinx.rst' in ls('../doc')
        True
        >>> cd(olddir)

    '''

    fldrs = Fldrs(scanroot)
    fldrs.scandirs()
    #reversed to do create_traceability_file at the end
    for folder,fldr in reversed(fldrs.items()):
        fldr.create_links_and_tags()

def _kw_from_path(kwpth,rexkwsplit=rexkwsplit):
    """use names of path up to project root as keywords

    ::

        >>> kwpth="/projects/me_about-this-1.rst"
        >>> _kw_from_path(kwpth)==frozenset({'me', 'this', '1', 'about'})
        True

    """
    fr = kwpth
    fn = None
    while True:
        fr,fn = dir_base(fr)
        if not fn:
            break
        if exists(fr):
            ipr = any(is_project_root_file(x) for x in os.listdir(fr))
            if ipr:
                break
    if fn:
        fn = relpath(kwpth,fr)
    else:
        fn = base(kwpth)
    fpth = stem(fn)
    if fpth.endswith(_rst) or fpth.endswith(_rest):
        fpth = stem(fpth)
    res = re.split(rexkwsplit,fpth)
    return frozenset(res)

def _kw_from_line(ln,rexkwsplit=rexkwsplit):
    """make  a frozenset out of keyword line

    ::

        >>> ln='.. {kw1,kw2-kw3.kw4}'
        >>> _kw_from_line(ln) == frozenset({'kw1','kw2','kw3','kw4'})
        True
        >>> ln='   .. {kw1,trag}'
        >>> _kw_from_line(ln) == frozenset({'kw1', 'trag'})
        True

    """
    return frozenset(x for x in re.split(rexkwsplit,ln.lower()) if x)

def grep(
      regexp=rexkw,
      dir=None,
      exts=set(['.rst','.rest','.stpl','.tpl','.adoc','.md','.wiki','.py','.jl','.lua','.tex',
                '.js', '.h','.c','.hpp','.cpp','.java','.cs','.vb','.r','.sh','.vim','.el',
                '.php','.sql','.swift','.go','.rb','.m','.pl','.rs','.f90','.dart',
                '.yml','.mm','.d','.lsp','.kt','.hs','.lhs','.ex','.scala','.clj']),
      **kwargs
):
    '''
    .. {grep}

    Uses python re to find ``regexp`` and return
    ``[(file,1-based index,line),...]``
    in *dir* (default: os.getcwd()) for ``exts`` files

    :param regexp: default is '^\s*\.\. {'
    :param dir: default is current dir
    :param exts: the extension of files searched


    '''

    if dir is None:
        dir = os.getcwd()
    regexp = re.compile(regexp)
    for root, dirs, files in os.walk(dir):
        for name in files:
            if any(name.endswith(ext) for ext in exts):
                f = normjoin(root,name)
                if not f.endswith('.py') and not f.endswith(_stpl) and exists(f+_stpl):
                    continue
                with open(f,encoding="utf-8") as fb:
                    lines=[l.strip() for l in fb.readlines()]
                    res = [(i,lines[i]) for i in rindices(regexp, lines)]
                    for (i,l) in res:
                        yield (f,i+1,l)

def yield_with_kw (kws, fn_ln_kw=None, **kwargs):
    '''
    Find keyword lines in ``fn_ln_kw`` list or using grep(),
    that contain the keywords in kws.

    Keyword line are either of::

        .. {{{kw1,kw2
        .. {kw1,kw2}
        {{_ID3('kw1 kw2')}}
        %__ID3('kw1 kw2')
        :ID3: kw1 kw2

    ``..`` can also be two comment chars of popular programming languages.
    This is due to ``dcx.rexkw``, which you can change.
    See also ``dcx.grep()`` for the keyword parameters.

    :param kws: string will be split by non-chars
    :param fn_ln_kw: list of (file, line, keywords) tuples
                     or ``regexp`` for grep()

    ::

        >>> list(yield_with_kw('a',[('a/b',1,'a b'),('c/d',1,'c d')]))
        [(0, ['a/b', 1, 'a b'])]
        >>> list(yield_with_kw('a c',[('a/b',1,'a b'),('c/d',1,'c d')]))
        []
        >>> list(yield_with_kw('a',[('a/b',1,'a b'),('c/d',1,'a c d')]))
        [(0, ['a/b', 1, 'a b']), (1, ['c/d', 1, 'a c d'])]
        >>> kwargs={'dir':normjoin(dirname(__file__),'../test/fixtures')}
        >>> kws = 'svg'
        >>> len(list(yield_with_kw(kws,**kwargs)))
        6
        >>> kws = 'png'
        >>> len(list(yield_with_kw(kws,**kwargs)))
        7

    '''

    if fn_ln_kw is None:
        fn_ln_kw = grep(**kwargs)
    elif isinstance(fn_ln_kw,str) or isinstance(fn_ln_kw,re.Pattern):
        fn_ln_kw = grep(fn_ln_kw, **kwargs)
    arexkwsplit=kwargs.get('rexkwsplit',rexkwsplit)
    oldfn = None
    qset = _kw_from_line(kws,rexkwsplit=arexkwsplit)
    for i,(fn,ln,kw) in enumerate(fn_ln_kw):
        #i,(fn,ln,kw) = next(enumerate(fn_ln_kw))
        if fn != oldfn:
            fnkw = _kw_from_path(fn,rexkwsplit=arexkwsplit)
            oldfn = fn
        kws = _kw_from_line(kw,rexkwsplit=arexkwsplit)|fnkw
        if kws and qset<=kws:
            yield i,[fn,ln,kw]


# ==============> pdt

class Counter:
    def __init__(self, before_first=0):
        '''Counter.

        Counter object.

        :param before_first: first-1 value

        ::

            >>> myc = Counter()
            >>> myc()
            1
            >>> myc()
            2

        '''

        self.cnt = before_first
    def __call__(self):
        self.cnt += 1
        return self.cnt

class PdtItem(Counter):
    def __init__(self, AAA
                 ,level=0
                 ):
        """
        Used in pdtAAA

        ``PdtItem`` numbers items in a ``pdt`` document.

        :param AAA: A ``pdt`` is identified by a base36 number (AAA).
        :param level: level=0 is a content item with separate ID = AAABB,
                      level>0 are headers: AAA text

        ::

            >>> pdt=PdtItem('032')
            >>> pdt()
            '\\n03201:'
            >>> pdt('kw1 kw2','kw3')
            '\\n03202: **kw1 kw2 kw3**'
            >>> hdr2=PdtItem('032',level=2)
            >>> hdr2('header text')
            '\\n032 header text\\n---------------'

        """

        super().__init__()
        self.AAA = AAA
        self.level = level
    def __call__(self, *args):
        super().__call__()
        if self.level==0:
            BB = np.base_repr(self.cnt,36)
            Id = "{}{:0>2}".format(self.AAA,BB)
            if args:
                lines = ['\n{}: **{}**'.format(Id,' '.join(args))]
            else:
                lines = ['\n{}:'.format(Id)]
        else:
            c = title_some[self.level-1]
            assert args, "a header cannot be empty"
            lin = ' '.join([self.AAA]+list(args))
            lenlin = len(lin)
            lines = ['',lin,c*lenlin]
        return "\n".join(lines)

def _pdtok(fid):
    assert fid.upper() == fid
    assert len(fid) == 3
    assert int(fid,base=36) < 36**3

def pdtid(pdtfile,pdtok=_pdtok):
    """
    ``pdtid`` takes the path of the current file and extracts an ID

    - either from the directory or
    - from the file name

    depending on ``pdtok``, which raises if not OK.

    ::

        >>> pdtid('/a/b/3A2/0sA.rest.stpl')
        '3A2'
        >>> pdtid('/a/b/3A2/0SA.rest.stpl')
        '0SA'
        >>> pdtid('/a/b/3A2/AS-A.rest.stpl')
        '3A2'

    """

    fid = base(pdtfile)
    while True:
        fido = fid
        fid = stem(fid)
        if fid == fido:
            break
    try:
        pdtok(fid)
    except:
        fid = stem(stem(base(dirname(pdtfile))))
        pdtok(fid)
    return fid

gpdtid = pdtid
def pdtAAA(pdtfile,dct,pdtid=pdtid,
            pdtfileid=lambda x:'ipdt'[int(x[0])]):
    '''
    ``pdtAAA`` is for use in an ``.stpl`` document::

        % pdtAAA(__main_file__,globals())

    See the example generated with::

        rstdoc --ipdt

    :param pdtfile: file path of pdt
    :param dct: dict to take up the generated defines
    :param pdtid: function returning the ID for the ``pdt`` cycle
        or regular expression with group for full path
        or regular expression for just the base name without extension (``pdtok``)
    :param pdtfileid: extracts/maps a file base name to one of the letters ipdt.
                      E.g. to have the files in order one could name them {0,1,2,3}.rest.stpl,
                      and map each to one of 'ipdt'.

    A ``pdt`` is a project enhancement cycle with its own documentation.
    ``pdt`` stands for

    - plan: why
    - do: specification
    - test: tests according specification

    Additionally there should be an

    - inform: non-technical purpose for or from external people.

    There can also be *only* the ``inform`` document, if the ``pdt`` item is only informative.

    The repo looks like this (preferred)::

        project repo
            pdt
                ...
                AAA
                    i*.rest.stpl
                    p*.rest.stpl
                    d*.rest.stpl
                    t*.rest.stpl

    or::

        project repo
            pdt
                ...
                AAA.rst.stpl

    In the first case, the ``UID`` starts with ``{i,p,d,t}AAA``.
    This is useful to trace related items by their plan-do-test-aspect.

    Further reading: `pdt <https://github.com/rpuntaie/pdt>`__

    ``pdtAAA`` makes these Python defines:

    - ``_[x]AAA`` returns next item number as AAABB. Use: ``{{_[x]AAA('kw1')}}``
    - ``_[x]AAA_``, ``_[x]AAA__``, ``_[x]AAA___``, ... returns headers. Use: ``{{_[x]AAA_('header text')}}``
    - ``__[x]AAA``, same as ``_[x]AAA``, but use: ``%__[x]AAA('kw1')`` (needs _printlist in dct)
    - ``__[x]AAA_``, ``__[x]AAA__``, ``__[x]AAA___``, ... Use: ``%__[x]AAA_('header text')``

    A, B are base36 letters and x is the initial of the file.
    The generated macros do not work for indented text, as they produce line breaks in RST text.

    ::

        >>> dct={'_printlist':str}
        >>> pdtfile = "a/b/a.rest.stpl"
        >>> pdtAAA(pdtfile,dct,pdtid=r'.*/(.)\.rest\.stpl')
        >>> dct['_a']('x y').strip()
        'a01: **x y**'
        >>> dct['__a']('x y').strip() #needs _printlist
        "['\\\\na02: **x y**', '\\\\n']"
        >>> dct={}
        >>> pdtfile = "pdt/000/d.rest.stpl"
        >>> pdtAAA(pdtfile,dct)
        >>> dct['_d000']('x y').strip()
        'd00001: **x y**'
        >>> dct={}
        >>> pdtfile = "a/b/003.rest.stpl"
        >>> pdtAAA(pdtfile,dct)
        >>> dct['_003']('x y').strip()
        '00301: **x y**'
        >>> dct['_003_']('x y')
        '\\n003 x y\\n======='
        >>> pdtfile="a/b/003/d.rest.stpl"
        >>> pdtAAA(pdtfile,dct)
        >>> dct['_003']('x y').strip()
        '00301: **x y**'
        >>> dct['_d003']('x y').strip()
        'd00301: **x y**'
        >>> dct['_003_']('x y')
        '\\n003 x y\\n======='
        >>> dct['_d003_']('x y')
        '\\nd003 x y\\n========'

    '''

    try:
        AAA = pdtid(pdtfile)
    except TypeError:
        try:
            AAA = re.match(pdtid,pdtfile).group(1)
        except AttributeError:
            def repdtok(fid):
                assert re.match(pdtid,fid)
            try:
                AAA = gpdtid(pdtfile,pdtok=repdtok)
            except:
                return
    except:
        return
    pdtfn = base(pdtfile)
    x = ''
    dct['AAA']=AAA
    if not pdtfn.startswith(AAA):
        try:
            x = pdtfileid(pdtfn)
        except:
            x = pdtfn[0]
        dct[x+'AAA']=x+AAA
        dct['xAAA']=x+AAA
    dct['PdtItem']=PdtItem
    dfns = "\n".join("_{0}"+"_"*i+"=PdtItem('{0}',"+str(i)+")" for i in range(10))
    if '_printlist' in dct: #define __AAA for direct output
        #_printlist should come from the separate pkg stpl/stpl.py
        dfns += "\n"
        dfns += "\n".join("__{0}"+"_"*i+"=lambda *args: _printlist([_{0}"+"_"*i+"(*args),'\\n'])" for i in range(10))
    eval(compile(dfns.format(AAA), "<pdtAAA>", "exec"),dct)
    if x:
        eval(compile(dfns.format(x+AAA), "<pdtAAA>", "exec"),dct)

def index_toctree(index_file):
    '''
    Construct::

        .. toctree::
            file1
            file2

    for the sphinx index file,
    i.e. ``index.rest.stpl`` or ``index.rst.stpl``.
    Use like::

        {{! index_toctree(__file__) }}

    '''

    from pathlib import Path
    thisdir = Path(index_file).parent
    indexlines = open(index_file).readlines()
    alreadyi = lambda x: rlines(r'\.\. include::.*'+stem(stem(x)),indexlines)
    toctree = ['.. toctree::']
    totoctree = lambda x: alreadyi(x) or toctree.append('      '+x)
    _get_rstrest()
    toglob = '*'+_rest+"*"
    pdtdirs = list(sorted(set(y.parent for y in thisdir.rglob(toglob) if
                                not y.name.startswith('index'+_rest) and
                                not y.name.endswith(_tpl) and
                                not any(x.endswith('build') for x in str(y).split(os.sep))
                                )))
    for apdtd in pdtdirs:
        fs = [f for f in sorted(Path(apdtd).glob(toglob)) if
                  not f.name.startswith('index'+_rest) and
                  not exists(str(f.absolute())+_stpl) and
                  not f.name.endswith(_tpl)]
        fsdict = dict((f.name[0],f) for f in fs)
        fsdone = set()
        for i in "0i1p2d3t":
            if i in fsdict:
                fsi = fsdict[i]
                fsi0 = fsi.name.split('.')[0]
                ipdtf = any(x.startswith(fsi0) for x in 'inform plan do test'.split())
                if ipdtf or '0123'.find(fsi0)>=0:
                    relpth = stem(fsi.relative_to(thisdir))
                    totoctree(relpth)
                    fsdone.add(fsi)
        for f in fs:
            if f not in fsdone:
                relpth = stem(f.relative_to(thisdir))
                totoctree(relpth)
    return '\n'.join(toctree)


# ==============> for building with WAF

try:
    from waflib import TaskGen, Task

    gensrc = {}

    @TaskGen.feature('gen_files')
    @TaskGen.before('process_rule')
    def gen_files_now(tskgen):
        global gensrc
        gensrc = {}
        rootpth = tskgen.bld.path.abspath()
        if rootpth not in sys.path:
            sys.path.append(rootpth)
        for gen in tskgen.path.ant_glob("**/gen"):
            genpth = gen.abspath()
            relgen = relpath(gen.parent.abspath(),start=tskgen.path.abspath())
            if gen.exists():
                for f, t, fun, kw in parsegenfile(genpth):
                    gensrc[normjoin(relgen,t)] = normjoin(relgen,f)
                    genfrom = gen.parent.find_resource(f)
                    assert genfrom, "%s rel to %s not found"%(f,genpth)
                    gento = gen.parent.make_node(t)
                    assert gento, "%s rel to %s not found"%(t,genpth)
                    tskgen.create_task('GENTSK', genfrom, gento, fun=fun, kw=kw)

    class GENTSK(Task.Task):
        def run(self):
            genfrom = self.inputs[0]
            gento = self.outputs[0]
            gento.parent.mkdir()
            gen(genfrom.abspath(), gento.abspath(), fun=self.fun, **self.kw)

    def get_docs_param(bld):
        docs = [x.lower() for x in bld.options.docs]
        if not docs:
            docs = [x.lower() for x in bld.env.docs]
        return docs

    @lru_cache()
    def get_files_in_doc(path, node):
        srcpath = node.parent.get_src()
        orgd = node.parent.abspath()
        d = srcpath.abspath()
        n = node.name
        nod = None
        if node.is_bld(
                        ) and not node.name.endswith(
                            _stpl
                            ) and not node.name.endswith(_tpl):
            nod = srcpath.find_node(node.name + _stpl)
        if not nod:
            nod = node
        ch = rstincluded(n, (d, orgd), True, True)
        deps = []
        nodeitself = True
        for x in ch:
            if nodeitself:
                nodeitself = False
                continue
            isrst = is_rst(x)
            # else cyclic dependency for _links_xxx.rst
            if isrst and x.startswith('_links_'):
                continue
            nd = srcpath.find_node(x)
            if not nd:
                if isrst and not x.endswith(_stpl) and not x.endswith(_tpl):
                    nd = srcpath.find_node(x + _stpl)
            deps.append(nd)
        depsgensrc = [
            path.find_node(gensrc[x]) for x in deps if x and x in gensrc
        ]
        rs = [x for x in deps if x] + depsgensrc
        return (list(sorted(set(rs), key=lambda a: a.name)), [])

    @TaskGen.feature('gen_links')
    @TaskGen.after('gen_files')
    def gen_links_now(tskgen):
        docs = get_docs_param(tskgen.bld)
        if docs:
            for so in tskgen.path.ant_glob('**/*.stpl'):
                tsk = Namespace()
                tsk.inputs = (so, )
                tsk.env = tskgen.env
                tsk.generator = tskgen
                render_stpl(tsk, tskgen.bld)
            links_and_tags(tskgen.path.abspath())

    @TaskGen.feature('gen_docs')
    @TaskGen.after('gen_links')
    def gen_docs_now(tskgen):
        docs = get_docs_param(tskgen.bld)
        if docs:
            bldpth=relpath(tskgen.bld.bldnode.abspath(),start=tskgen.path.abspath())
            for anext in 'tikz svg dot uml pyg eps'.split():
                source=tskgen.path.ant_glob('**/*.'+anext,excl=bldpth+"/**")
                tskgen.bld(name='build '+anext, source=[x for x in source if _traceability_file not in x.abspath()])
            foundfiles = list(tskgen.path.ant_glob('**/*'+_rest,excl=bldpth+"/**"))
            tskgen.bld(name='build all rest', source=foundfiles)

    def render_stpl(tsk, bld):
        bldpath = bld.path.get_bld().abspath()
        ps = tsk.inputs[0].abspath()
        try:
            pt = tsk.outputs[0].abspath()
        except:
            if ps.endswith(_stpl):
                pt = stem(ps)
            else:
                raise RstDocError('No target for %s' % ps)
        env = dict(tsk.env)
        env.update(tsk.generator.__dict__)
        env['bldpath'] = bldpath
        dostpl(ps, pt, **env)

    class STPL(Task.Task):
        always_run = True

        def run(self):
            render_stpl(self, self.generator.bld)

    @TaskGen.extension(_stpl)
    def stpl_taskgen(tskgen, node):  # expand into same directory
        nn = node.parent.make_node(stem(node.name))
        tskgen.create_task('STPL', node, nn)
        try:
            tskgen.get_hook(nn)(tskgen, nn)
        except:
            pass

    def gen_ext_tsk(tskgen, node,
                    ext):  # into _images or <updir>/_images in source path
        srcfldr = node.parent.get_src()
        _imgpath, there = _here_or_updir(srcfldr.abspath(), _images)
        if not there:
            _imgpath = normjoin(srcfldr.abspath(), _images)
            mkdir(_imgpath)
        imgpath = relpath(_imgpath, start=srcfldr.abspath())
        outnode = srcfldr.make_node(
            normjoin(imgpath,
                     stem(node.name) + '.png'))
        tskgen.create_task(ext[1:].upper(), node, outnode)

    @TaskGen.extension(_tikz)
    def tikz_to_png_taskgen(tskgen, node):
        gen_ext_tsk(tskgen, node, _tikz)

    class TIKZ(Task.Task):
        def run(self):
            tikzpng(self.inputs[0].abspath(), self.outputs[0].abspath())

    @TaskGen.extension(_svg)
    def svg_to_png_taskgen(tskgen, node):
        gen_ext_tsk(tskgen, node, _svg)

    class SVG(Task.Task):
        def run(self):
            svgpng(self.inputs[0].abspath(), self.outputs[0].abspath())

    @TaskGen.extension('.dot')
    def dot_to_png_taskgen(tskgen, node):
        gen_ext_tsk(tskgen, node, '.dot')

    class DOT(Task.Task):
        run_str = "${dot} -Tpng ${SRC} -o${TGT}"

    @TaskGen.extension('.uml')
    def uml_to_png_taskgen(tskgen, node):
        gen_ext_tsk(tskgen, node, '.uml')

    class UML(Task.Task):
        run_str = "${plantuml} ${SRC} -o${TGT[0].parent.abspath()}"

    @TaskGen.extension('.eps')
    def eps_to_png_taskgen(tskgen, node):
        gen_ext_tsk(tskgen, node, '.eps')

    class EPS(Task.Task):
        run_str = ("${inkscape} --export-dpi=${DPI} --export-area-drawing" +
                   " --export-background-opacity=0 ${SRC} " +
                   " --export-filename=${TGT}")

    @TaskGen.extension('.pyg')
    def pyg_to_png_taskgen(tskgen, node):
        gen_ext_tsk(tskgen, node, '.pyg')

    class PYG(Task.Task):
        def run(self):
            pygpng(self.inputs[0].abspath(), self.outputs[0].abspath())

    @TaskGen.extension(_get_rstrest())
    def docs_taskgen(tskgen, node):
        docs = get_docs_param(tskgen.bld)
        d = get_files_in_doc(tskgen.path, node)

        def rstscan():
            return d

        def out_node(doctgt,doctype):
            relnode = relpath(stem(node.abspath())+'.'+doctype,start=tskgen.path.abspath())
            bldpath = tskgen.path.get_bld()
            return bldpath.find_or_declare(normjoin(doctgt,relnode))

        if node.name != "index"+_rest:
            for doctgt in docs:
                if doctgt.startswith('sphinx_'):
                    continue
                doctype = _suffix(doctgt)
                tskgen.create_task(
                    'NonSphinxTask', [node],
                    out_node(doctgt,doctype),
                    scan=rstscan,
                    doctgt=doctgt)
        else:
            for doctgt in docs:
                if not doctgt.startswith('sphinx_'):
                    continue
                doctype = _suffix(doctgt.replace('_tex','_latex'))
                tskgen.create_task(
                    'SphinxTask', [node],
                    out_node(doctgt,doctype.replace('latex','tex')),
                    scan=rstscan,
                    doctype=doctype)

    class NonSphinxTask(Task.Task):
        def run(self):
            dorst(self.inputs[0].abspath(), self.outputs[0].abspath(),
                  self.doctgt)

    class SphinxTask(Task.Task):
        always_run = True

        def run(self):
            inpth = self.inputs[0].abspath()
            confpypath, _ = _here_or_updir(dirname(inpth), 'conf.py')
            config = conf_py(dirname(confpypath))
            # rst_sphinx needs it relative to infile
            if 'html_extra_path' in config:
                config['html_extra_path'] = [
                    normjoin(dirname(confpypath), x)
                    for x in config['html_extra_path']
                ]
            else:
                config['html_extra_path'] = html_extra_path
            rst_sphinx(inpth, self.outputs[0].abspath(),
                       self.doctype, **config)

    def options(opt):
        def docscb(option, opt, value, parser):
            setattr(parser.values, option.dest, value.split(','))

        opt.add_option(
            "--docs",
            type='string',
            action="callback",
            callback=docscb,
            dest='docs',
            default=[],
            help="""Comma-separated list of
html, docx, pdf, sphinx_html (default)
or any other of http://www.sphinx-doc.org/en/master/usage/builders"""
        )

    def configure(cfg):
        cfg.env['docs'] = cfg.options.docs
        for x in 'plantuml dot inkscape'.split():
            try:
                cfg.env[x] = cfg.find_program(x)
            except cfg.errors.ConfigurationError:
                cfg.to_log(x + ' was not found (ignoring)')
        root=cfg.path.abspath()
        config = conf_py(root)
        cfg.env['DPI'] = str(config.get('DPI', DPI))
        cfg.env['rstrest'] = _get_rstrest()
        assert isinstance(cfg.env['rstrest'],str)
        #VERSION
        try: # repo?
            from git import Repo
            repo = Repo(root)
            tags = repo.tags
            taglast = tags and tags[-1].name or '0.0.0'
            tagint = int(taglast.replace('.',''),10)
            tagfix = tagint%10
            tagminor = tagint//10 % 10
            tagmajor = tagint//100 % 10
            tagnew = f'{tagmajor}.{tagminor}.{tagfix+1}'
            cfg.env['VERSION'] = tagnew
        except: # no repo
            try:
                # VERSION file must exist when no git repo available
                cfg.env['VERSION'] = next(filter(
                    lambda x:x.strip(),opn(normjoin(root,'VERSION')).readlines()))
            except FileNotFoundError:
                cfg.env['VERSION'] = '0.0.0'

    def build(bld):
        _set_rstrest(bld.env['rstrest'])

        bld.src2bld = lambda f: bld(
            features='subst', source=f, target=f, is_copy=True)

        def gen_files():
            bld(name="process gen file", features="gen_files")
        bld.gen_files = gen_files

        def gen_links():
            bld(name="create links and .tags", features="gen_links")
        bld.gen_links = gen_links

        def gen_docs():
            bld(name="create docs", features="gen_docs")
        bld.gen_docs = gen_docs

        # use like bld(rule=bld.stpl, source='x.h.stpl')
        # to compile stpl only, else do without rule
        bld.stpl = lambda tsk: render_stpl(tsk, bld)

        global g_config
        if exists(normjoin(bld.srcnode.abspath(), 'conf.py')):
            g_config = conf_py(bld.srcnode.abspath())

        def build_docs():
            bld.gen_files()
            bld.gen_links()
            bld.gen_docs()
            bld.add_group()
        #call bld.build_docs in root wscript to have .tags there
        bld.build_docs = build_docs

except:
    pass

# ==============< for building with WAF

# pandoc --print-default-data-file reference.docx > reference.docx
# pandoc --print-default-data-file reference.odt > reference.odt
# pandoc --print-default-template=latex
# then modified in format and not to use figure labels
# this is for mktree(): first line of file content must not be empty!
example_rest_tree = r'''
       build/
       dcx.py << file:///__dcx__
       reference.tex << file:///__tex_ref__
       reference.docx << file:///__docx_ref__
       reference.odt << file:///__odt_ref__
       wafw.py << file:///__wafw__
       waf
         #!/usr/bin/env sh
         shift
         ./wafw.py "$@"
       waf.bat
         @setlocal
         @set PYEXE=python
         @where %PYEXE% 1>NUL 2>NUL
         @if %ERRORLEVEL% neq 0 set PYEXE=py
         @%PYEXE% -x "%~dp0wafw.py" %*
         @exit /b %ERRORLEVEL%
       wscript
         #vim: ft=python
         from waflib import Logs
         Logs.colors_lst['BLUE']='\x1b[01;36m'
         top='.'
         out='build'
         def options(opt):
             opt.load('dcx', tooldir='.')
         def configure(cfg):
             cfg.load('dcx', tooldir='.')
         def build(bld):
             bld.load('dcx', tooldir='.')
             bld.build_docs()
       docutils.conf
         [general]
         halt_level: severe
         report_level: error
       conf.py
         project = 'sample'
         author = project+' Project Team'
         copyright = '2019, '+author
         version = '1.0'
         release = '1.0.0'
         try:
             import sphinx_bootstrap_theme
             html_theme_path = sphinx_bootstrap_theme.get_html_theme_path()
             html_theme = 'bootstrap'
         except:
             pass
         #these are enforced by rstdoc, but keep them for sphinx-build
         numfig = 0
         smartquotes = 0
         source_suffix = '.rest'
         templates_path = []
         language = None
         highlight_language = "none"
         default_role = 'math'
         pygments_style = 'sphinx'
         exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
         master_doc = 'index'
         html_extra_path=['doc/_traceability_file.svg'] #relative to conf.py
         import os
         on_rtd = os.environ.get('READTHEDOCS') == 'True'
         if not on_rtd:
             latex_engine = 'xelatex'
             #You can postprocess pngs.default: png_post_processor = None
             def png_post_processor(filename):
                 from PIL import Image, ImageChops
                 def trim(im):
                     bg = Image.new(im.mode, im.size, im.getpixel((0, 0)))
                     diff = ImageChops.difference(im, bg)
                     diff = ImageChops.add(diff, diff, 2.0, -100)
                     bbox = diff.getbbox()
                     if bbox:
                         return im.crop(bbox)
                     return im
                 im = Image.open(filename)
                 im = trim(im)
                 im.save(filename)
                 return filename
             #the following are default and can be omitted
             latex_elements = {'preamble':r"""
             \usepackage{pgfplots}
             \usepackage{unicode-math}
             \usepackage{tikz}
             \usepackage{caption}
             \captionsetup[figure]{labelformat=empty}
             \usetikzlibrary{arrows,snakes,backgrounds,patterns,matrix,shapes,fit,calc,shadows,plotmarks,intersections}
             """
             }
             #new in rstdcx/dcx/py
             tex_wrap = r"""
             \documentclass[12pt,tikz]{standalone}
             \usepackage{amsmath}
             """+latex_elements['preamble']+r"""
             \pagestyle{empty}
             \begin{document}
             %s
             \end{document}
             """
             DPI = 600
             # |targetid| refs are grouped by the first letter (determining whether r(iskanalysis), s(pecification), d(esign) or t(est))
             target_id_group = lambda targetid: targetid[0]
             target_id_color={"ra":("r","lightblue"), "sr":("s","red"),
                "dd":("d","yellow"), "tp":("t","green")}
             pandoc_doc_optref={'latex': '--template reference.tex',
                              'html': {},#each can also be dict of file:template
                              'pdf': '--template reference.tex',
                              'docx': '--reference-doc reference.docx',
                              'odt': '--reference-doc reference.odt'
                              }
             _pandoc_latex_pdf = ['--listings','--number-sections','--pdf-engine',
                'xelatex','-V','titlepage','-V','papersize=a4',
                '-V','toc','-V','toc-depth=3','-V','geometry:margin=2.5cm']
             pandoc_opts = {'pdf':_pandoc_latex_pdf,'latex':_pandoc_latex_pdf,
                'docx':[],'odt':[],
                'html':['--mathml','--highlight-style','pygments']}
             rst_opts = { #http://docutils.sourceforge.net/docs/user/config.html
                         'strip_comments':True
                         ,'report_level':3
                         ,'raw_enabled':True
                         }
             def name_from_directive(directive,count):
                 return directive[0].upper() + directive[1:] + ' ' + str(count)
       Makefile
         SPHINXOPTS  = -c .
         SPHINXBLD   = sphinx-build
         SPHINXPROJ  = sample
         DOCDIR      = doc/
         DOCBACK     = ../
         DCXFROMDOC  = ../
         BLDDIR      = build/doc/
         STPLS       = $(wildcard $(DOCDIR)*.stpl)
         STPLTGTS    = $(STPLS:%.stpl=%)
         SRCS        = $(filter-out $(DOCDIR)index.rest,$(wildcard $(DOCDIR)*.rest))
         SRCSTPL     = $(wildcard $(DOCDIR)*.rest.stpl)
         IMGS        = \
         	$(wildcard $(DOCDIR)*.pyg)\
         	$(wildcard $(DOCDIR)*.eps)\
         	$(wildcard $(DOCDIR)*.tikz)\
         	$(wildcard $(DOCDIR)*.svg)\
         	$(wildcard $(DOCDIR)*.uml)\
         	$(wildcard $(DOCDIR)*.dot)\
         	$(wildcard $(DOCDIR)*.eps.stpl)\
         	$(wildcard $(DOCDIR)*.tikz.stpl)\
         	$(wildcard $(DOCDIR)*.svg.stpl)\
         	$(wildcard $(DOCDIR)*.uml.stpl)\
         	$(wildcard $(DOCDIR)*.dot.stpl)
         PNGS=$(subst $(DOCDIR),$(DOCDIR)_images/,\
         	$(patsubst %.eps,%.png,\
         	$(patsubst %.pyg,%.png,\
         	$(patsubst %.tikz,%.png,\
         	$(patsubst %.svg,%.png,\
         	$(patsubst %.uml,%.png,\
         	$(patsubst %.dot,%.png,\
         	$(patsubst %.eps.stpl,%.png,\
         	$(patsubst %.dot.stpl,%.png,\
         	$(patsubst %.tikz.stpl,%.png,\
         	$(patsubst %.svg.stpl,%.png,\
         	$(patsubst %.uml.stpl,%.png,$(IMGS)))))))))))))
         DOCXS = $(subst $(DOCDIR),$(BLDDIR)docx/,$(SRCS:%.rest=%.docx))\
         	$(subst $(DOCDIR),$(BLDDIR)docx/,$(SRCSTPL:%.rest.stpl=%.docx))
         PDFS  = $(subst $(DOCDIR),$(BLDDIR)pdf/,$(SRCS:%.rest=%.pdf))\
         	$(subst $(DOCDIR),$(BLDDIR)pdf/,$(SRCSTPL:%.rest.stpl=%.pdf))
         .PHONY: docx help Makefile docxdir pdfdir stpl index imgs
         stpl: $(STPLTGTS)
         %:%.stpl
         	@cd $(DOCDIR) && stpl "$(<F)" "$(@F)"
         imgs: $(PNGS)
         $(DOCDIR)_images/%.png:$(DOCDIR)%.pyg
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
         $(DOCDIR)_images/%.png:$(DOCDIR)%.eps
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
         $(DOCDIR)_images/%.png:$(DOCDIR)%.tikz
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
         $(DOCDIR)_images/%.png:$(DOCDIR)%.svg
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
         $(DOCDIR)_images/%.png:$(DOCDIR)%.uml
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
         $(DOCDIR)_images/%.png:$(DOCDIR)%.dot
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py $(<F)
         docxdir: ${BLDDIR}docx
         pdfdir: ${BLDDIR}pdf
         MKDIR_P = mkdir -p
         ${BLDDIR}docx:
         	@${MKDIR_P} ${BLDDIR}docx
         ${BLDDIR}pdf:
         	@${MKDIR_P} ${BLDDIR}pdf
         index:
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py
         help:
         	@$(SPHINXBLD) -M help "$(DOCDIR)" "$(BLDDIR)" $(SPHINXOPTS) $(O)
         	@echo "  docx        to docx"
         	@echo "  pdf         to pdf"
         #http://www.sphinx-doc.org/en/stable/usage/builders/
         html dirhtml singlehtml htmlhelp qthelp applehelp devhelp epub latex text man texinfo pickle json xml pseudoxml: Makefile index stpl imgs
         	@$(SPHINXBLD) -M $@ "$(DOCDIR)" "$(BLDDIR)" $(SPHINXOPTS) $(O)
         docx:  docxdir index stpl imgs $(DOCXS)
         $(BLDDIR)docx/%.docx:$(DOCDIR)%.rest
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py "$(<F)" "$(DOCBACK)$@"
         pdf: pdfdir index stpl imgs $(PDFS)
         $(BLDDIR)pdf/%.pdf:$(DOCDIR)%.rest
         	@cd $(DOCDIR) && python $(DCXFROMDOC)dcx.py "$(<F)" "$(DOCBACK)$@"
       __code__
         └ some.h
             /*
             #def gen_tst(lns,**kw):
             #  return [l.split('@')[1] for l in rlines(r'^\s*@',lns)]
             #def gen_tst
             #def gen_tstdoc(lns,**kw):
             #  return ['#) '+l.split('**')[1] for l in rlines(r'^/\*\*',lns)]
             #def gen_tstdoc

             @//generated from some.h
             @#include <assert.h>
             @#include "some.h"
             @int main()
             @{
             */

             /**Test add1()
             @assert(add1(1)==2);
             */
             int add1(int a)
             {
               return a+1;
             }

             /**Test add2()
             @assert(add2(1)==3);
             */
             int add2(int a)
             {
               return a+2;
             }

             /*
             @}
             */
       doc
        ├ _images/
        ├ index.rest
            ============
            Project Name
            ============

            .. toctree::
               ra.rest
               sr.rest
               dd.rest
               tp.rest

            One can also have a

            - issues.rest for issues

            - pp.rest for the project plan
              (with backlog, epics, stories, tasks)

            .. REMOVE THIS IF NO LINKING OVERVIEW WANTED
            .. include:: _traceability_file.rst

            .. include:: _links_sphinx.rst

        ├ ra.rest
            Risk Analysis
            =============

            .. _`rz7`:

            :rz7: risk calculations

            Risk calculations are done with python in the ``.stpl`` file.

            .. include:: _links_sphinx.rst

        ├ sr.rest
            Software/System Requirements
            ============================

            .. _`s97`:

            Requirements are testable (see |t9a|).

            .. _`su7`:

            ``dcx.py`` produces its own labeling
            consistent across DOCX, PDF, HTML.

            .. _`sy7`:

            A Requirement Group
            -------------------

            .. _`s3a`:

            :s3a: brief description

            Don't count the ID, since the order will change.
            The IDs have the first letter of the file
            and 2 or more random letters of ``[0-9a-z]``.
            Use an editor macro to generate IDs.

            A link: |s3a|

            If one prefers ordered IDs, one can use templates::

              %id = lambda x=[0]: x.append(x[-1]+1) or "s{:0>2}".format(x[-1])

              .. _`soi`:

              :{{id()}}: auto numbered.

            The disadvantage is that the id will differ
            between rst and final doc.
            When this is needed in an included file
            use template include: ``%include('x.rst.tpl`)``
            See the the ``test/stpl`` directory.

            Every ``.rest`` has this line at the end::

               .. include:: _links_sphinx.rst

            .. include:: _links_sphinx.rst

        ├ dd.rest
            Design Description
            ==================

            .. _`d97`:

            :d97: Independent DD IDs

              The relation with RS IDs is m-n.
              Links like |s3a| can be scattered over more DD entries.

            .. _`dx3`:

            .. figure:: _images/egtikz1.png
               :name:
               :width: 30%

               |dx3|: Create from egtikz1.tikz

            .. _`dz3`:

            .. figure:: _images/egtikz.png
               :name:
               :width: 50%

               |dz3|: Create from egtikz.tikz

               The usage of ``:name:`` produces:
                 ``WARNING: Duplicate explicit target name: ""``. Ignore.

            Reference via |dz3|.

            ``.tikz``, ``.svg``, ``.dot``,  ``.uml``, ``.eps`` or ``.stpl``
            thereof and ``.pyg``, are converted to ``.png``.

            .. _`dz4`:

            .. figure:: _images/egsvg.png
               :name:

               |dz4|: Created from egsvg.svg.stpl

            .. _`dz5`:

            .. figure:: _images/egdot.png
               :name:

               |dz5|: Created from egdot.dot.stpl

            .. _`dz6`:

            .. figure:: _images/eguml.png
               :name:

               |dz6|: Created from eguml.uml

            .. _`dz7`:

            .. figure:: _images/egplt.png
               :name:
               :width: 30%

               |dz7|: Created from egplt.pyg

            .. _`dz8`:

            .. figure:: _images/egpyx.png
               :name:

               |dz8|: Created from egpyx.pyg

            .. _`dr8`:

            .. figure:: _images/egcairo.png
               :name:

               |dr8|: Created from egcairo.pyg

            .. _`ds8`:

            .. figure:: _images/egpygal.png
               :name:
               :width: 30%

               |ds8|: Created from egpygal.pyg

            .. _`dsx`:

            .. figure:: _images/egother.png
               :name:

               |dsx|: Created from egother.pyg

            .. _`d98`:

            .. figure:: _images/egeps.png
               :name:

               |d98|: Created from egeps.eps

            .. _`dua`:

            |dua|: Table legend

            .. table::
               :name:

               +--------+--------+
               | A      | B      |
               +========+========+
               | |eps|  | |eps|  |
               +--------+--------+

            .. _`dta`:

            |dta|: Table legend

            .. list-table::
               :name:
               :widths: 20 80
               :header-rows: 1

               * - Bit
                 - Function

               * - 0
                 - afun

            Reference |dta| does not show ``dta``.

            .. _`dyi`:

            |dyi|: Listing showing struct.

            .. code-block:: cpp
               :name:

               struct astruct{
                  int afield; //afield description
               }

            .. _`d9x`:

            .. math::
               :name:

               V = \frac{K}{r^2}

            ``:math:`` is the default inline role: `mc^2`

            .. _`d99`:

            :OtherName: Keep names the same all over.

            Here instead of ``d99:`` we use ``:OtherName:``,
            but now we have two synonyms for the same item.
            This is no good. If possible, keep ``d99`` in the source
            and in the final docs.

            The item target must be in the same file as the item content.
            The following would not work::

              .. _`dh5`:

              .. include:: somefile.rst

            .. |eps| image:: _images/egeps.png

            .. include:: _links_sphinx.rst

        ├ tp.rest
            Test Plan
            =========

            .. _`t9a`:

            Requirement Tests
            -----------------

            No duplication. Only reference the requirements to be tested.

            - |s97|
            - |su7|
            - |s3a|
            - |rz7|

            Or better: reference the according SR chapter,
            else changes there would need an update here.

            - Test |sy7|

            Unit Tests
            ----------

            Use ``.rst`` for included files
            and start the file with ``_`` if generated.

            - |d97|
            - |dx3|
            - |dz4|
            - |dz5|
            - |dz6|
            - |dz7|
            - |dz8|
            - |dsx|
            - |d98|
            - |dua|
            - |dta|
            - |dyi|
            - |d9x|
            - |d99|

            .. include:: _sometst.rst

            .. include:: _links_sphinx.rst
        ├ egtikz.tikz
            [thick,red]
            \draw (0,0) grid (3,3);
            \foreach \c in {(0,0), (1,0), (2,0), (2,1), (1,2)}
                \fill \c + (0.5,0.5) circle (0.42);
        ├ egtikz1.tikz
            \begin{scope}[blend group = soft light]
            \fill[red!30!white]   ( 90:1.2) circle (2);
            \fill[green!30!white] (210:1.2) circle (2);
            \fill[blue!30!white]  (330:1.2) circle (2);
            \end{scope}
            \node at ( 90:2)    {Typography};
            \node at ( 210:2)   {Design};
            \node at ( 330:2)   {Coding};
            \node [font=\Large] {\LaTeX};
        ├ egsvg.svg.stpl
            <?xml version="1.0" encoding="utf-8"?>
            <svg xmlns="http://www.w3.org/2000/svg"
                fill="none" version="1.1" width="110pt" height="60pt"
                stroke-width="0.566929" stroke-miterlimit="10.000000">
            %for i in range(10):
              <path fill="none" stroke="#f00" stroke-width="1"
                  d="M10,55 C15,5 100,5 100,{{i*5}}" />
            %end
            %for i in range(10):
              <path fill="none" stroke="#f40" stroke-width="1" d="M10,
                  {{i*5}} C15,5 100,5 100,55" />
            %end
            <text x="50" y="50" fill="red">Hi!</text>
            </svg>
        ├ egdot.dot.stpl
            digraph {
            %for i in range(3):
                "From {{i}}" -> "To {{i}}";
            %end
                }
        ├ eguml.uml
            @startuml
            'style options
            skinparam monochrome true
            skinparam circledCharacterRadius 0
            skinparam circledCharacterFontSize 0
            skinparam classAttributeIconSize 0
            hide empty members
            Class01 <|-- Class02
            Class03 *-- Class04
            Class05 o-- Class06
            Class07 .. Class08
            Class09 -- Class10
            @enduml
        ├ egplt.pyg
            #vim: ft=python
            import matplotlib.pyplot as plt
            import numpy as np
            x = np.random.randn(1000)
            plt.hist( x, 20)
            plt.grid()
            plt.title(r'Normal: $\mu=%.2f, \sigma=%.2f$'%(x.mean(), x.std()))
            plt.show()
        ├ egpyx.pyg
            import pyx
            c = pyx.canvas.canvas()
            c.stroke(pyx.path.circle(0,0,2),
                [pyx.style.linewidth.Thick,pyx.color.rgb.red])
            c.text(1, 1, 'Hi', [pyx.color.rgb.red])
        ├ egpygal.pyg
            import pygal
            diagram=pygal.Bar()(1, 3, 3, 7)(1, 6, 6, 4)
            def to_svg():
                return diagram.render().decode('utf-8')
        ├ egother.pyg
            from PIL import Image, ImageDraw, ImageFont
            im = Image.new("RGBA",size=(50,50),color=(155,0,100))
            draw = ImageDraw.Draw(im)
            draw.rectangle(((0, 0), (40, 40)), fill="red")
            draw.text((20, 20), "123")
            save_to_png = lambda out_file: im.save(out_file, "PNG")
        ├ egeps.eps
            newpath 6 2 36 54 rectstroke
            showpage
        ├ egcairo.pyg
            import cairocffi as cairo
            surface = cairo.SVGSurface(None, 200, 200)
            context = cairo.Context(surface)
            x, y, x1, y1 = 0.1, 0.5, 0.4, 0.9
            x2, y2, x3, y3 = 0.6, 0.1, 0.9, 0.5
            context.set_source_rgba(1, 0.2, 0.2, 0.6)
            context.scale(200, 200)
            context.set_line_width(0.04)
            context.move_to(x, y)
            context.curve_to(x1, y1, x2, y2, x3, y3)
            context.stroke()
            context.set_line_width(0.02)
            context.move_to(x, y)
            context.line_to(x1, y1)
            context.move_to(x2, y2)
            context.line_to(x3, y3)
            context.stroke()
        ├ gen
            ##from|to|gen_xxx|kwargs
            #../__code__/some.h | _sometst.rst                | tstdoc | {}
            #../__code__/some.h | ../build/__code__/some_tst.c | tst    | {}
            #or
            from_to_fun_kw = [ #fun and gen_fun() or gen()
            ['../__code__/some.h','_sometst.rst','tstdoc',{}],
            ['../__code__/some.h','../build/__code__/some_tst.c','tst',{}]
            ]'''

# replaces from '├ index.rest' to '├ egtikz.tikz'
example_stpl_subtree = r'''
        ├ model.py
            """
            Contains definitions used in
            - template files (``.rst.tpl`` or standalone ``.rest.stpl``)
            - test programs
            """
            from pint import UnitRegistry
            u = UnitRegistry()
            u.define('percent = 0.01*count = %')
            def U(*k,sep=", "):
                """
                Returns string of quantity, with units if possible.
                """
                try:
                    return sep.join(["{:~P}"]*len(k)).format(*k)
                except:
                    res = sep.join(["{}"]*len(k)).format(*k)
                    if res == 'None':
                        res = '-'
                    return res
            # Definitions e.g. x_some = 3.5*u.hour #see |x_some_doc|
        ├ utility.rst.tpl
            % import sys
            % import os
            % sys.path.append(os.path.dirname(__file__))
            % from model import *
            % cntr = lambda alist0,prefix='',width=2: alist0.append(
            %        alist0[-1]+1) or ("{}{:0>%s}"%width).format(prefix,alist0[-1])
            % II = lambda prefix,alist0,short:':{}: **{}**'.format(
            %      cntr(alist0,prefix),short)
            % #define in file e.g.
            % #SR=lambda short,alist0=[0]:II('SR',alist0,short)
            % #and use like {{SR('Item Title')}}
            %def pagebreak():
            .. raw:: openxml

                <w:p>
                  <w:r>
                    <w:br w:type="page"/>
                  </w:r>
                </w:p>

            .. raw:: html

                <p style="page-break-before: always;">&nbsp;</p>

            .. raw:: latex

                \pagebreak

            .. for docutils
            .. raw:: odt

                <text:p text:style-name="PageBreak"/>

            .. for pandoc
            .. raw:: opendocument

                <text:p text:style-name="PageBreak"/>

            %end
        ├ index.rest
            .. encoding: utf-8
            .. vim: ft=rst

            ============
            Project Name
            ============

            .. toctree::
               sy.rest
               ra.rest
               sr.rest
               dd.rest
               tp.rest

            One can also have a

            - issues.rest for issues

            - pp.rest for the project plan
              (with backlog, epics, stories, tasks)

            .. REMOVE THIS IF NO LINKING OVERVIEW WANTED
            .. include:: _traceability_file.rst

            .. include:: _links_sphinx.rst

        ├ sy.rest.stpl
            .. encoding: utf-8
            .. vim: ft=rst

            % globals().update(include('utility.rst.tpl'))
            % SY=lambda short,alist0=[0]:II('SY',alist0,short)

            .. _`sy_system_scope`:

            ############
            System Scope
            ############

            .. _`sy_general_idea`:

            {{SY('General Idea')}}

              Source code is text done in a good editor.
              Use the same editor also for documentation.
              Jump around like in hypertext.

            .. include:: _links_sphinx.rst
        ├ ra.rest.stpl
            .. encoding: utf-8
            .. vim: ft=rst

            % globals().update(include('utility.rst.tpl'))
            % RA=lambda short,alist0=[0]:II('RA',alist0,short)

            .. _`ra_risk_analysis`:

            #############
            Risk Analysis
            #############

            .. _`r_restructured_text`:

            Advantages
            ==========

            {{RA('Restructured Text')}}

              We use
              `restructuredText <http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html>`_
              together with
              `SimpleTemplate <https://bottlepy.org/docs/dev/stpl.html#simpletemplate-syntax>`_.

              This is very flexible:

              - it allows to generate boilerplate text with python
              - it allows to link the text across documents
              - it allows to have many final formats (html, docx, pdf, odt, ...)
              - ...

            .. _`ra_risks`:

            Risks
            =====

            .. _`r_editor`:

            {{RA('Wrong Editor')}}

              This is not for people that only know how to edit in MS Words.
              Users should have embraced a good text editor.
              Software guys normally have.

              One needs a good text editor that supports ctags.
              These have been tested

              - `atom <https://atom.io/>`_
              - `vim <https://www.vim.org/>`_

            .. include:: _links_sphinx.rst
        ├ sr.rest.stpl
            .. encoding: utf-8
            .. vim: ft=rst

            % globals().update(include('utility.rst.tpl'))
            % SR=lambda short,alist0=[0]:II('SR',alist0,short)

            .. _`sr_software_system_requirements`:

            ############################
            Software/System Requirements
            ############################

            .. _`sr_general`:

            General
            =======

            .. _`sr_testable`:

            {{SR('Testable')}}

              Requirements are testable (see |tp_requirement_tests|).

            .. _`sr_style`:

            {{SR('Style')}}

              In a restructuredText file, have one sentence in one line.

              Make long sentences into

              - lists

                - with sub items, if needed

              - or simply make more sentences out of it

              A link: |sr_style|.

            .. _`sr_a_requirement_group`:

            A Requirement Group
            ===================

            .. _`sr_id`:

            {{SR('ID')}}

              The ID seen in the final document is numbered
              by a python function.
              In the restructuredText files there is no numbering.
              The targets use keywords instead.
              This way one can rearrange the items
              keeping the items sorted and still referentially consistent.

              The ID shall not contain any hyphens
              or dots or other non-identifier characters,
              as some final formats, like DOCX, demand that.

            .. include:: _links_sphinx.rst
        ├ dd.rest.stpl
            .. encoding: utf-8
            .. vim: ft=rst

            % globals().update(include('utility.rst.tpl'))
            % DD=lambda short,alist0=[0]:II('DD',alist0,short)

            .. _`dd_design_description`:

            ##################
            Design Description
            ##################

            .. _`dd_traceability`:

            {{DD('Traceability')}}

              ``dcx.py`` associates all links between two targets
              to the first target.
              This can be used as traceability.

              Warnings issued during conversion to final documents
              help to keep the documents consistent.

            .. _`dd_name`:

            {{DD('Name')}}

              For targeted

              - ``.. table::``
              - ``.. list-table::``
              - ``.. figure::``
              - ``.. code-block::``
              - ``.. math::``

              use ``:name:``.
              In the legend use the same ID as in the target definition.

              .. _`dd_figure`:

              .. figure:: _images/egtikz.png
                 :name:
                 :width: 50%

                 |dd_figure|: Caption here.
                 Reference this via ``|dd_figure|``.

            For testing purpose the following is rendered via include files.

            Include normal .rst way, where the .rst might be gnerated by a ``.rst.stpl``

            .. include:: dd_included.rst

            Include the stpl way

            %include('dd_diagrams.tpl',DD=DD) # you optionally can provide python definitions

            Pandoc does not know about `definitions in included files <https://github.com/jgm/pandoc/issues/4160>`__.

            .. |eps| image:: _images/egeps.png

            .. include:: _links_sphinx.rst

        ├ dd_included.rst.stpl
            .. encoding: utf-8
            .. vim: ft=rst

            .. _`dd_code`:

            |dd_code|: Listing showing struct.

            .. code-block:: cpp
               :name:

               struct xxx{
                  int yyy; //yyy for zzz
               }

            Include normal ``.rst``.

            .. include:: dd_tables.rst

            Again include the stpl way.

            %include('dd_math.tpl')

        ├ dd_tables.rst
            .. encoding: utf-8
            .. vim: ft=rst

            .. _`dd_table`:

            |dd_table|: Table legend

            .. table::
               :name:

               +--------+--------+
               | A      | B      |
               +========+========+
               | |eps|  | |eps|  |
               +--------+--------+

            .. _`dd_list_table`:

            |dd_list_table|: Table legend

            .. list-table::
               :name:
               :widths: 20 80
               :header-rows: 1

               * - Bit
                 - Function

               * - 0
                 - xxx

            Reference |dd_table| or |dd_list_table| does not show
            ``dd_table`` or ``dd_list_table``.

        ├ dd_math.tpl
            .. encoding: utf-8
            .. vim: ft=rst

            .. _`dd_math`:

            .. math::
               :name:

               V = \frac{K}{r^2}

            ``:math:`` is the default inline role: `mc^2`

            With `sympy <www.sympy.org>`_ one can have formulas in ``some.py``
            that are usable for calculation.
            The formulas can be converted to latex
            in the ``.stpl`` or ``.tpl`` file.

            %def hyp(a,b):
            %    return a**2+b**2
            %end

            The long side of a rectangular triangle with legs
            {{3}} and {{4}} is {{hyp(3,4)**0.5}}. See |hyp|.

            .. _`hyp`:

            .. math::
                :name:

                %import sympy
                %from sympy.abc import a,b,c
                {{sympy.latex(sympy.Eq(c,hyp(a,b)))}}

        ├ dd_diagrams.tpl
            .. encoding: utf-8
            .. vim: ft=rst

            .. _`dd_diagrams`:

            {{DD('Diagrams')}}

              .. _`exampletikz1`:

              .. figure:: _images/egtikz1.png
                 :name:
                 :width: 30%

                 |exampletikz1|: Create from egtikz1.tikz

                 The usage of ``:name:`` produces: ``WARNING:
                 Duplicate explicit target name: ""``. Ignore.

              Reference via |exampletikz1|.

              ``.tikz``, ``.svg``, ``.dot``,  ``.uml``, ``.eps`` or ``.stpl``
              thereof and ``.pyg``, are converted to ``.png``.

              .. _`examplesvg`:

              .. figure:: _images/egsvg.png
                 :name:

                 |examplesvg|: Created from egsvg.svg.stpl

              .. _`exampledot`:

              .. figure:: _images/egdot.png
                 :name:

                 |exampledot|: Created from egdot.dot.stpl

              .. _`exampleuml`:

              .. figure:: _images/eguml.png
                 :name:

                 |exampleuml|: Created from eguml.uml

              .. _`exampleplt`:

              .. figure:: _images/egplt.png
                 :name:
                 :width: 30%

                 |exampleplt|: Created from egplt.pyg

              .. _`examplepyx`:

              .. figure:: _images/egpyx.png
                 :name:

                 |examplepyx|: Created from egpyx.pyg

              .. _`examplecairo`:

              .. figure:: _images/egcairo.png
                 :name:

                 |examplecairo|: Created from egcairo.pyg

              .. _`examplepygal`:

              .. figure:: _images/egpygal.png
                 :name:
                 :width: 30%

                 |examplepygal|: Created from egpygal.pyg

              .. _`exampleother`:

              .. figure:: _images/egother.png
                 :name:

                 |exampleother|: Created from egother.pyg

              .. _`exampleeps`:

              .. figure:: _images/egeps.png
                 :name:

                 |exampleeps|: Created from egeps.eps

              %if False:
              .. _`target_more_than_in_rest`:

                 It is OK to have more targets in the .stpl file.
              %end

        ├ tp.rest.stpl
            .. encoding: utf-8
            .. vim: ft=rst

            % globals().update(include('utility.rst.tpl'))
            % TP=lambda short,alist0=[0]:II('TP',alist0,short)

            .. _`tp_test_plan`:

            #########
            Test Plan
            #########

            .. _`tp_requirement_tests`:

            Requirement Tests
            =================

            .. _`tp_test_types`:

            {{TP('Test Types')}}

              Performance tests are only one kind of tests.

            .. _`tp_no_duplication`:

            {{TP('No duplication')}}

              Since items in other documents are phrased as tests,
              there is no need to repeat the text here.

              - |sr_id|

              Or better: Reference the according chapter:

              - Test |sr_a_requirement_group|

            .. _`tp_unit_tests`:

            Unit Tests
            ==========

            .. _`tp_gen_file`:

            {{TP('gen file')}}

              Use ``.rst`` for included files
              and start the file with ``_`` if generated.
              How test documentation files are generated
              from test source code can be specified in the ``gen`` file.

            .. include:: _links_sphinx.rst'''


example_ipdt_tree = r'''
       wafw.py << file:///__wafw__
       waf
         #!/usr/bin/env sh
         shift
         ./wafw.py "$@"
       waf.bat
         @setlocal
         @set PYEXE=python
         @where %PYEXE% 1>NUL 2>NUL
         @if %ERRORLEVEL% neq 0 set PYEXE=py
         @%PYEXE% -x "%~dp0wafw.py" %*
         @exit /b %ERRORLEVEL%
       wscript
         #vim: ft=python
         from waflib import Logs
         Logs.colors_lst['BLUE']='\x1b[01;36m'
         top='.'
         out='build'
         def options(opt):
             opt.load('rstdoc.dcx')
         def configure(cfg):
             cfg.load('rstdoc.dcx')
         def build(bld):
             bld.load('rstdoc.dcx')
             bld.build_docs()
       c
       └ some.h
           /*
           #def gen_tst(lns,**kw):
           #  return [l.split('@')[1] for l in rlines(r'^\s*@',lns)]
           #def gen_tst
           #def gen_tstdoc(lns,**kw):
           #  return ['#) '+l.split('**')[1] for l in rlines(r'^/\*\*',lns)]
           #def gen_tstdoc
           
           @//generated from some.h
           @#include <assert.h>
           @#include "some.h"
           @int main()
           @{
           */
           
           /**Test add1()
           @assert(add1(1)==2);
           */
           int add1(int a)
           {
             return a+1;
           }
           
           /**Test add2()
           @assert(add2(1)==3);
           */
           int add2(int a)
           {
             return a+2;
           }
           
           /*
           @}
           */
       pdt
         ├ conf.py
             project = 'PDT'
             author = project+' Project Team'
             copyright = '2019, '+author
             version = '0.0.0'
             release = version
             try:
                 import sphinx_bootstrap_theme
                 html_theme_path = sphinx_bootstrap_theme.get_html_theme_path()
                 html_theme = 'bootstrap'
             except:
                 pass
             #these are enforced by rstdoc, but keep them for sphinx-build
             numfig = 0
             smartquotes = 0
             source_suffix = '.rest'
             templates_path = []
             language = None
             highlight_language = "none"
             default_role = 'math'
             pygments_style = 'sphinx'
             exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
             master_doc = 'index'
             html_extra_path=['_traceability_file.svg'] #relative to conf.py
             import os
             on_rtd = os.environ.get('READTHEDOCS') == 'True'
             if not on_rtd:
                 latex_engine = 'xelatex'
                 #You can postprocess pngs.default: png_post_processor = None
                 def png_post_processor(filename):
                     from PIL import Image, ImageChops
                     def trim(im):
                         bg = Image.new(im.mode, im.size, im.getpixel((0, 0)))
                         diff = ImageChops.difference(im, bg)
                         diff = ImageChops.add(diff, diff, 2.0, -100)
                         bbox = diff.getbbox()
                         if bbox:
                             return im.crop(bbox)
                         return im
                     im = Image.open(filename)
                     im = trim(im)
                     im.save(filename)
                     return filename
                 #the following are default and can be omitted
                 latex_elements = {'preamble':r"""
                 \usepackage{pgfplots}
                 \usepackage{unicode-math}
                 \usepackage{tikz}
                 \usepackage{caption}
                 \captionsetup[figure]{labelformat=empty}
                 \usetikzlibrary{arrows,snakes,backgrounds,patterns,matrix,shapes,fit,calc,shadows,plotmarks,intersections}
                 """
                 }
                 #new in rstdcx/dcx/py
                 tex_wrap = r"""
                 \documentclass[12pt,tikz]{standalone}
                 \usepackage{amsmath}
                 """+latex_elements['preamble']+r"""
                 \pagestyle{empty}
                 \begin{document}
                 %s
                 \end{document}
                 """
                 DPI = 600
                 #rst-internal target IDs to include in trace
                 rextrace_target_id="^[ipdt][0-9A-Z]{3}[a-z]+$"
                 target_id_color={"inform":("i","lightblue"), "plan":("p","red"),
                    "do":("d","yellow"), "test":("t","green")}
                 pandoc_doc_optref={'latex': '--template reference.tex',
                                  'html': {},#each can also be dict of file:template
                                  'pdf': '--template reference.tex',
                                  'docx': '--reference-doc reference.docx',
                                  'odt': '--reference-doc reference.odt'
                                  }
                 _pandoc_latex_pdf = ['--listings','--number-sections','--pdf-engine',
                    'xelatex','-V','titlepage','-V','papersize=a4',
                    '-V','toc','-V','toc-depth=3','-V','geometry:margin=2.5cm']
                 pandoc_opts = {'pdf':_pandoc_latex_pdf,'latex':_pandoc_latex_pdf,
                    'docx':[],'odt':[],
                    'html':['--mathml','--highlight-style','pygments']}
                 rst_opts = { #http://docutils.sourceforge.net/docs/user/config.html
                             'strip_comments':True
                             ,'report_level':3
                             ,'raw_enabled':True
                             }
                 def name_from_directive(directive,count):
                     return directive[0].upper() + directive[1:] + ' ' + str(count)
         ├ docutils.conf
             [general]
             halt_level: severe
             report_level: error
         ├ 000
            ├ gen
                # vim: ft=python
                from os.path import relpath, dirname
                from_to_fun_kw = [ #fun and gen_fun() or gen()
                ['../../c/some.h','_sometst.rst','tstdoc',{}],
                ['../../c/some.h','../../build/c/some_tst.c','tst',{}]
                ]
            ├ i.rest.stpl
                .. vim: ft=rst
                
                .. _`i000`:
                
                % globals().update(include('pdt.rst.tpl',
                % Title="pdt inform"
                % ))
                
                .. _`i000inform`:
                %__i000('inform')
                
                Purpose is non-technical, from or for externals.
                
                The purpose of the ``__i000`` python function is to count the entries.
                If you don't care about counted items, you can use normal RST, or provide your own ``__i000``.
                
                plan-do-test: `pdt <https://github.com/rpuntaie/pdt>`__.
                
                %epilog()
            ├ p.rest.stpl
                .. vim: ft=rst
                
                .. _`p000`:
                
                % globals().update(include('pdt.rst.tpl',
                % Title="pdt plan"
                % ))
                
                
                .. _`p000grouping`:
                %__p000_('Grouping') ############################################################
                
                .. _`p000headers`:
                %__p000('headers')
                |i000inform|
                
                Headers are groupings of content items,
                but are not as important as content items.
                
                .. _`p000subproject`:
                %__p000('sub-project')
                |i000inform|
                
                For a sub-project prefer a new ``pdt`` over headers.
                
                
                %epilog()
            ├ d.rest.stpl
                .. vim: ft=rst
                
                .. _`d000`:
                
                % globals().update(include('pdt.rst.tpl',
                % Title="pdt do"
                % ))
                
                
                .. _`d000repo`:
                %__d000('repo')
                |p000headers| is an example of a link to another item.
                
                - ``pdt`` documents the development
                - ``doc`` documents the SW
                
                .. _`000repofigure`:
                
                .. figure:: _images/repo.png
                   :name:
                   :width: 50%
                
                   |000repofigure|: Example project repo.
                
                .. _`d000notrace`:
                %__d000('notrace')
                |p000subproject|
                
                The figure target above does not start with 'd'.
                ``rextrace_target_id`` is set to ignore such targets.
                
                %epilog()
            ├ t.rest.stpl
                .. vim: ft=rst
                
                .. _`t000`:
                
                % globals().update(include('pdt.rst.tpl',
                % Title="pdt test"
                % ))
                
                
                .. _`t000testitem1`:
                %__t000('test item 1')
                
                Link *plan* and *do* items that are tested here, e.g.
                
                - |p000headers| fits to |d000repo|
                
                .. _`t000testitem2`:
                %__t000('test item 2')
                |d000notrace|
                
                Tests manually.
                
                .. _`t000codegeneratedtestitems`:
                %__t000('code generated test items')
                
                .. include:: _sometst.rst
                
                %epilog()
                
                
            └ repo.pyg
                # vim: ft=python ts=4
                import drawSvg
                d = drawSvg.Drawing(400, 800, origin=(0,0))
                draw = lambda what,*args,**kwargs: d.append(getattr(drawSvg,what)(*args,**kwargs))
                for e in "Image Text Rectangle Circle ArcLine Path Lines Line Arc".split():
                    eval(compile("{}=lambda *args, **kwargs: draw('{}',*args,**kwargs)".format(e.lower(),e),"repo","exec"),globals())
                p=dict(fill='red', stroke_width=2, stroke='black')
                th=20
                y=[d.height]
                dx,dy=2*th,-2*th
                x=lambda : len(y)*dx
                indent=lambda: y.append(0)
                def back(n=1):
                    yy = sum(y[:-n])
                    y[-n-1]+=sum(y[-n:])
                    del y[-n:]
                    line(x(),yy,x(),sum(y),**p)
                def entry(t):
                    yy = sum(y)
                    line(x(),yy,x(),yy+dy,**p)
                    y[-1]=y[-1]+dy
                    yy += dy
                    line(x(),yy,x()+dx,yy,**p)
                    text(t,th,x()+dx,yy)
                entry("sw")
                indent()
                entry("pdt")
                indent()
                entry("000")
                indent()
                entry("{i,p,d,t}.rest.stpl")
                back()
                entry("001")
                indent()
                entry("{i,p,d,t}.rest.stpl")
                back(2)
                entry('doc')
                indent()
                entry('sw_{x,y,z}.rest.stpl')
                back()
                entry('c')
                indent()
                entry('sw_{x,y,z}.c')
                back()
                entry('python')
                indent()
                entry('sw')
                indent()
                entry('__init__.py')
                back(2)
                entry('test')
                indent()
                entry('test_{x,y,z}.py')
                back()
                entry('waf')
                entry('wscript')
         ├ 001
            __imgs__
            ├ i.rest.stpl
                .. vim: ft=rst
                
                .. _`i001`:
                
                %globals().update(include('pdt.rst.tpl',
                %Title="Information on Diagrams",
                %Type="inform"
                %))
                
                .. _`i001figure`:
                %__i001('figure')
                
                An item is not included in the traceabilty diagram unless with links to other items.
                Ref to |i000inform| to test inclusion.
                
                .. _`001fig1`:
                
                .. figure:: _images/egtikz.png
                   :name:
                   :width: 50%
                
                   |001fig1|: Caption here.
                   Reference this via ``|001fig1|``.
                
                .. _`i001rstinclude`:
                %__i001('rst include')
                |i000inform|
                
                .. include:: i_included.rst
                
                .. _`i001stplincludetpl`:
                %__i001('stpl include (tpl)')
                |i000inform|
                
                %include('i_diagrams.tpl',_i001=_i001)
                
                Following definitions here, as
                Pandoc does accept
                `definitions in included files <https://github.com/jgm/pandoc/issues/4160>`__.
                
                .. |eps| image:: _images/egeps.png
                
                %epilog()
                
            ├ i_diagrams.tpl
                .. vim: ft=rst
                
                .. _`i001diagrams`:
                %__i001('diagrams')
                |i000inform|
                
                .. _`exampletikz1`:
                
                .. figure:: _images/egtikz1.png
                   :name:
                   :width: 30%
                
                   |exampletikz1|: Create from egtikz1.tikz
                
                ``.tikz``, ``.svg``, ``.dot``,  ``.uml``, ``.eps`` or ``.stpl``
                thereof and ``.pyg``, are converted to ``.png``.
                
                .. _`examplesvg`:
                
                .. figure:: _images/egsvg.png
                   :name:
                
                   |examplesvg|: Created from egsvg.svg.stpl
                
                .. _`exampledot`:
                
                .. figure:: _images/egdot.png
                   :name:
                
                   |exampledot|: Created from egdot.dot.stpl
                
                .. _`exampleuml`:
                
                .. figure:: _images/eguml.png
                   :name:
                
                   |exampleuml|: Created from eguml.uml
                
                .. _`exampleplt`:
                
                .. figure:: _images/egplt.png
                   :name:
                   :width: 30%
                
                   |exampleplt|: Created from egplt.pyg
                
                .. _`examplepyx`:
                
                .. figure:: _images/egpyx.png
                   :name:
                
                   |examplepyx|: Created from egpyx.pyg
                
                .. _`examplecairo`:
                
                .. figure:: _images/egcairo.png
                   :name:
                
                   |examplecairo|: Created from egcairo.pyg
                
                .. _`examplepygal`:
                
                .. figure:: _images/egpygal.png
                   :name:
                   :width: 30%
                
                   |examplepygal|: Created from egpygal.pyg
                
                .. _`exampleother`:
                
                .. figure:: _images/egother.png
                   :name:
                
                   |exampleother|: Created from egother.pyg
                
                .. _`exampleeps`:
                
                .. figure:: _images/egeps.png
                   :name:
                
                   |exampleeps|: Created from egeps.eps
                
                %if False:
                .. _`target_more_than_in_rest`:
                
                   It is OK to have more targets in the .stpl file.
                %end
                
                Make a reference to |exampletikz1|.
                
            ├ i_included.rst
                .. vim: ft=rst
                
                
                
                .. _`i001code`:
                
                |i001code|: Listing showing struct.
                
                .. code-block:: cpp
                   :name:
                
                   struct xxx{
                      int yyy; //yyy for zzz
                   }
                
                .. _`i001table`:
                
                i00101: **table**
                
                Include normal ``.rst``.
                
                .. include:: i_tables.rst
                
                .. _`i001math`:
                
                i00102: **math**
                
                Again include the stpl way.
                
                .. vim: ft=rst
                
                .. _`i001math1`:
                
                .. math::
                   :name:
                
                   V = \frac{K}{r^2}
                
                ``:math:`` is the default inline role: `mc^2`
                
                
            ├ i_included.rst.stpl
                .. vim: ft=rst
                
                %globals().update(include('pdt.rst.tpl'))
                
                .. _`i001code`:
                
                |i001code|: Listing showing struct.
                
                .. code-block:: cpp
                   :name:
                
                   struct xxx{
                      int yyy; //yyy for zzz
                   }
                
                .. _`i001table`:
                %__001('table')
                |i000inform|
                
                Include normal ``.rst``.
                
                .. include:: i_tables.rst
                
                .. _`i001math`:
                %__i001('math')
                |i000inform|
                
                Again include the stpl way.
                
                %include('i_math.tpl')
                
            ├ i_math.tpl
                .. vim: ft=rst
                
                .. _`i001math1`:
                
                .. math::
                   :name:
                
                   V = \frac{K}{r^2}
                
                ``:math:`` is the default inline role: `mc^2`
                
            └ i_tables.rst
                .. vim: ft=rst
                
                .. _`i001table1`:
                
                |i001table1|: Table legend
                
                .. table::
                   :name:
                
                   +--------+--------+
                   | A      | B      |
                   +========+========+
                   | |eps|  | |eps|  |
                   +--------+--------+
                
                .. _`i001table2`:
                
                |i001table2|: Table legend
                
                .. list-table::
                   :name:
                   :widths: 20 80
                   :header-rows: 1
                
                   * - Bit
                     - Function
                
                   * - 0
                     - xxx
                
                Reference |i001table1| or |i001table2| does not show
                ``i001table1`` or ``i001table2``.
         ├ index.rest.stpl
             .. vim: ft=rst
             
             %globals().update(include('pdt.rst.tpl'
             %,Title="rstdoc - pdt example"
             %,Type="inform"
             %))
             
             %from pathlib import Path
             %thisdir=Path(__file__).parent
             %stem = lambda x:os.path.splitext(x)[0].replace('\\', '/')
             %from os.path import dirname, basename
             
             .. toctree::
             
             %for x in sorted(set(y.parent for y in thisdir.rglob("*.rest*") if not y.name.startswith('index.rest'))):
             %  fs = dict((f.name[0],f) for f in Path(x).rglob("*.rest*"))
             %  for i in "ipdt":
             %     if i in fs:
                   {{stem(fs[i].relative_to(thisdir))}}
             %         del fs[i]
             %     end
             %  end
             %  for i in fs:
                   {{stem(fs[i].relative_to(thisdir))}}
             %  end
             %  end
             
             .. REMOVE THIS IF NO LINKING OVERVIEW WANTED
             .. include:: _traceability_file.rst
             
             .. include:: _links_sphinx.rst
             
         ├ pdt.rst.tpl
             % #expect Title and optionally
             % setdefault('Contact','roland.puntaier@gmail.com')
             % setdefault('Type','pdt')
             % setdefault('Status','draft')
             % assert Type in "pdt inform".split(), "Wrong PDT Type"
             % assert Status in "draft final replaced deferred rejected withdrawn".split(), "Wrong PDT Status"
             %
             % #see pdtAAA (__main_file__ is the main stpl file)
             % from rstdoc.dcx import pdtAAA
             % pdtAAA(__main_file__,globals()) #,pdtid=".*/(.).\w+.stpl")
             %
             %if defined('Title'):
             %ttl=AAA+" - "+Title if defined('AAA') else Title
             {{'#'*len(ttl)}}
             {{ttl}}
             {{'#'*len(ttl)}}
             %end
             
             %if defined('iAAA'):
             :PDT: {{AAA}}
             :Contact: {{!Contact}}
             :Type: {{!Type}}
             :Status: {{Status}}
             %end
             %def pagebreak():
             .. raw:: openxml
             
                 <w:p>
                   <w:r>
                     <w:br w:type="page"/>
                   </w:r>
                 </w:p>
             
             .. raw:: html
             
                 <p style="page-break-before: always;">&nbsp;</p>
             
             .. raw:: latex
             
                 \pagebreak
             
             .. for docutils
             .. raw:: odt
             
                 <text:p text:style-name="PageBreak"/>
             
             .. for pandoc
             .. raw:: opendocument
             
                 <text:p text:style-name="PageBreak"/>
             
             %end
             %def epilog():
             .. include:: /_links_sphinx.rst
             %end #epilog'''

example_over_tree = r'''
  wafw.py << file:///__wafw__
  waf
    #!/usr/bin/env sh
    shift
    ./wafw.py "$@"
  waf.bat
    @setlocal
    @set PYEXE=python
    @where %PYEXE% 1>NUL 2>NUL
    @if %ERRORLEVEL% neq 0 set PYEXE=py
    @%PYEXE% -x "%~dp0wafw.py" %*
    @exit /b %ERRORLEVEL%
  wscript
    #vim: ft=python
    from waflib import Logs
    Logs.colors_lst['BLUE']='\x1b[01;36m'
    top='.'
    out='build'
    def options(opt):
        opt.load('rstdoc.dcx')
    def configure(cfg):
        cfg.load('rstdoc.dcx')
    def build(bld):
        bld.load('rstdoc.dcx')
        bld.build_docs()
  org
    ├ process
    │  └ SOP
    │      └ purchase.rest
    │             Purchase
    │             ========
    │
    │             .. _`sop_purchase`:
    │
    │             :sop_purchase:
    │
    │             A contributor places a link under the purchase folder.
    │
    │
    │             .. include:: /_links_sphinx.rst
    ├ discussion
    │  └ topic1.rest
    │     Topic1
    │     ======
    │
    │     .. _`topic_merge_delay`:
    │
    │     :topic_merge_delay:
    │
    │     Can someone take over review of |000|, as I'm busy with ...
    │
    │     .. include:: /_links_sphinx.rst
    ├ mediation
    │  └ conflict1.rest
    │     Conflict1
    │     =========
    │
    │     .. _`conflict_000`:
    │
    │     :conflict_000:
    │
    │     Conflicting view on |000| between ...
    │
    │     .. include:: /_links_sphinx.rst
    ├ contributor
        └ c1
           ├ assigned.rest
           │    Assigned for c1
           │    ===============
           │
           │    Planning and coordinating |000|
           │
           │    Implementation of |fw_000|
           │
           │    .. include:: /_links_sphinx.rst
           ├ log
               └ 2019.rest
                   2019
                   ====
            
                   .. _`c1_20191101`
            
                   |issue1|
            
                   .. _`c1_20191102`
            
                   |issue1|
                   It was necessary to refactor ...
            
                   .. include:: /_links_sphinx.rst
  doc
    ├ index.rest
    │    Documentation
    │    =============
    │
    │    .. toc::
    │
    │       tutorial.rest
    │
    │    .. include:: /_links_sphinx.rst
    └ tutorial.rest
    │    Tutorial
    │    ========
    │
    │    Example API usage
    │
    │    .. include:: /_links_sphinx.rst
  pdt
    └ 000
        ├ info.rest
        │   .. _`000`:
        │
        │   Feature Info
        │   ============
        │
        │   .. _`i000a`:
        │
        │   :i000a: info
        │
        │   .. include:: /_links_sphinx.rst
        ├ plan.rest
        │   Feature Plan
        │   ============
        │
        │   .. _`p000a`:
        │
        │   :p000a: plan
        │
        │   .. include:: /_links_sphinx.rst
        ├ do.rest
        │   Feature Spec
        │   ============
        │
        │   .. _`d000a`:
        │
        │   :d000a: spec
        │
        │   .. include:: /_links_sphinx.rst
        └ test.rest
        │   Feature Test
        │   ============
        │
        │   .. _`t000a`:
        │
        │   :t000a: test
        │
        │   .. include:: /_links_sphinx.rst
  dev
    ├ issues
    │  └ issue1.rest
    │      Issue1 Title
    │      ============
    │
    │      .. _`issue1`:
    │
    │      :issue1:
    │
    │      SW does not link to device, if ...
    │
    │      .. include:: /_links_sphinx.rst
    │  └ issue2.rest
    │      Issue2 Title
    │      ============
    │
    │      .. _`issue2`:
    │
    │      :issue2:
    │
    │      Test xyz fails.
    │
    │      .. include:: /_links_sphinx.rst
    ├ hw
    │  ├ casing
    │  │   ├ plan.rest
    │          .. _`case001`:
    │
    │          :case001:
    │
    │          According |d000a| ...
    │
    │          .. include:: /_links_sphinx.rst
    │  │   ├ scad/
    │  │   └ test
    │  │       └ stability.rest
    │              Casing Stability Tests
    │              ======================
    │
    │              .. _`fall_test`:
    │
    │              :fall_test:
    │
    │              The casing is pushed from a table.
    │
    │              .. include:: /_links_sphinx.rst
    │  ├ pcb1
    │  │   ├ plan.rest
    │          PCB1 Implementation
    │          ===================
    │
    │          .. _`pcb1_000`:
    │
    │          :pcb1_000:
    │
               Overview of functional units of pcb1.
    │
    │          .. include:: /_links_sphinx.rst
    │  │   ├ pcb1.sch
    │  │   └ test/
    │  └ test/
    ├ sw
    │  ├ fw
    │  │   ├ plan.rest
    │          Firmware
    │          ========
    │
    │          .. _`fw_000`:
    │
    │          :fw_000:
    │
    │          To satisfy |000| these steps need to be taken.
    │
    │          .. include:: /_links_sphinx.rst
    │  │   ├ controller1/
    │  │       └ C
    │  │         └ init.c
                    // just an example
    │  │   ├ test/
    │  ├ android/
    │  │   ├ plan.rest
    │          Android App
    │          ===========
    │
    │          .. _`appplan`:
    │
    │          :appplan:
    │
    │          Implementation plan satisfying |000|.
    │
    │          .. include:: /_links_sphinx.rst
    │  │   ├ app/
    │  │   ├ testapp/
    │  └ test/
    └ test/
  contribution.rest
     Contributing
     ============

     .. _`how_to_contribute`:

     :how_to_contribute:

     - |general_goal|
     - pdt about plans

     .. _`unassigned_issues`:

     :unassigned_issues:

     These issues are still unassigned and need new contributors.

     |issue2|

     .. include:: /_links_sphinx.rst
  readme.rest
     Project Entry Point
     ===================

     .. _`general_goal`:

     :general_goal:

     Overview of goal and links to further information.

     .. include:: /_links_sphinx.rst
  index.rest
     .. toctree::

     .. vim: ft=rst

     ############
     Example Tree
     ############

     .. toctree::

           readme.rest
           contribution.rest
           org/discussion/topic1.rest
           org/mediation/conflict1.rest
           org/process/SOP/purchase.rest
           org/contributor/c1/assigned.rest
           org/contributor/c1/log/2019.rest
           pdt/000/info.rest
           pdt/000/do.rest
           pdt/000/plan.rest
           pdt/000/test.rest
           doc/tutorial.rest
           dev/hw/casing/plan.rest
           dev/hw/casing/test/stability.rest
           dev/hw/pcb1/plan.rest
           dev/sw/android/plan.rest
           dev/sw/fw/plan.rest
           dev/issues/issue1.rest
           dev/issues/issue2.rest

     .. include:: /_links_sphinx.rst'''


def initroot(
        rootfldr
        ,sampletype
        ):
    '''
    Creates a sample tree in the file system.

    :param rootfldr: directory name that becomes root of the sample tree
    :param sampletype: either 'ipdt' or 'stpl' for templated sample trees, or 'rest' or 'over' for non-templated

    See ``example_rest_tree``, ``example_stpl_subtree``, ``example_ipdt_tree``, ``example_over_tree`` in dcx.py.

    '''

    if txdir is None:
        return

    def rR(instr):
        if _rest == '.rst':
            #>instr='x.rst y.rest z.rst'
            instr = instr.replace('.rst','.rrrr')
            instr = instr.replace('.rest','.rst')
            instr = instr.replace('.rrrr','.rest')
            #>instr == 'x.rest y.rst z.rest'
        return instr

    thisfile = __file__.replace('\\', '/')
    thisdir = dirname(thisfile)
    tex_ref = normjoin(thisdir, 'reference.tex')
    docx_ref = normjoin(thisdir, 'reference.docx')
    odt_ref = normjoin(thisdir, 'reference.odt')
    wafw = normjoin(thisdir, 'wafw.py')
    if sampletype == 'ipdt':
        imglines = example_rest_tree.splitlines()
        imglines = imglines[
            list(rindices('├ egtikz.tikz',imglines))[0]:
            list(rindices('├ gen',imglines))[0]]
        imglines = [' '*4+x for x in imglines]
        example_tree = example_ipdt_tree.replace('__imgs__',('\n'.join(imglines)+'\n').lstrip())
    elif sampletype == 'over':
        example_tree=example_over_tree
    else:
        example_tree=example_rest_tree
    example_tree = rR(example_tree)
    inittree = [
        l for l in example_tree.replace(
            '__dcx__', thisfile).replace(
            '__tex_ref__', tex_ref).replace(
            '__docx_ref__', docx_ref).replace(
            '__odt_ref__', odt_ref).replace(
            '__wafw__', wafw).replace(
            '__code__', rootfldr.strip()=='.' and base(cwd()) or rootfldr
            ).splitlines()
    ]
    if sampletype == 'stpl':
        def _replace_lines(origlns, start, stop, insertlns):
            return origlns[:list(rindices(start, origlns))
                           [0]] + insertlns + origlns[list(
                               rindices(stop, origlns))[0]:]
        inittree = _replace_lines(inittree, rR('├ index.rest'), '├ egtikz.tikz',
                                  rR(example_stpl_subtree).lstrip('\n').splitlines())
    mkdir(rootfldr)
    with new_cwd(rootfldr):
        txdir.view_to_tree(inittree)

def index_dir(
    root='.'
    ):
    '''
    Index a directory.

    :param root: All subdirectories of ``root`` that contain a ``.rest`` or ``.rest.stpl`` file are indexed.

    - expands the .stpl files
    - generates the files as defined in the ``gen`` file (see example in dcx.py)
    - generates ``_links_xxx.rst`` for xxx = {sphinx latex html pdf docx odt}
    - generates ``.tags`` with jumps to reST targets

    '''

    # do all stpl's upfront. ``create_links_and_tags()`` needs them
    from pathlib import Path
    # reversed evaluates deeper stpls first
    for f in reversed(sorted([str(x) for x in Path(root).rglob('*.stpl')])):
        dpth = normjoin(root, f)
        if isfile(dpth):
            outpth = stem(dpth)
            try:
                dostpl(dpth, outpth)
            except Exception as err:
                print('Error expanding %s: %s' % (dpth, str(err)))
    # link, gen and tags per directory
    fldrs = Fldrs(root)
    fldrs.scandirs()
    #reversed to do create_traceability_file at the end
    for folder, fldr in reversed(fldrs.items()):
        # generated files need to be there to be indexed
        genpth = normjoin(folder, 'gen')
        if exists(genpth):
            try:
                for f, t, d, kw in parsegenfile(genpth):
                    gen(normjoin(folder, f),
                        target=normjoin(folder, t),
                        fun=d,
                        **kw)
            except Exception as err:
                print('Generating files in %s seems not meant to be done: %s' %
                      (genpth, str(err)))
        fldr.create_links_and_tags()


description = (

"""
``rstdcx`` CLI
--------------

Without parameters: creates ``|substitution|`` links and .tags ctags for reST targets.

With two or three parameters: process file or dir to out file or dir
through Pandoc, Sphinx, Docutils (third parameter):

- ``html``, ``docx``, ``odt``, ``pdf``, ... uses  Pandoc.

- ``rst_html``, ``rst_odt``, ``rst_pdf``, ...  uses
  `rst2html <http://docutils.sourceforge.net/0.6/docs/user/tools.html>`__, ...

- ``sphinx_html``, ``sphinx_pdf``, ...  uses Sphinx.
  Sphinx provides a nice entry point via the
  `sphinx bootstrap theme <https://github.com/ryan-roemer/sphinx-bootstrap-theme>`__.

4th parameter onward become python defines usable in ``.stpl`` files.

Pdf output needs latex. Else you can make odt or docx and use

- win: ``swriter.exe --headless --convert-to pdf Untitled1.odt``
- linux: ``lowriter --headless --convert-to pdf Untitled1.odt``

Inkscape (.eps, .svg), Dot (.dot), Planuml (.uml), latex (.tex,.tikz)
are converted to .png into ``./_images`` or ``<updir>/_images`` or '.'.
Any of the files can be a SimpleTemplate template (xxx.yyy.stpl).

Configuration is in ``conf.py`` or ``../conf.py``.

``rstdoc --stpl|--rest|--ipdt|-over`` create sample project trees.

``--stpl`` with ``.rest.stpl`` template files,
``--rest`` with only a doc folder with ``.rest`` files,
``--ipdt`` with inform-plan-do-test enhancement cycles
``--over`` with ``.rest`` files all over the project tree including symbolic links

Examples
--------

Example folders (see wscript and Makefile there)::

    rstdoc --rest <folder> [--rstrest]
    rstdoc --stpl <folder> [--rstrest]
    rstdoc --ipdt <folder> [--rstrest]
    rstdoc --over <folder> [--rstrest]

Use ``--rstrest`` to produce ``.rst`` for the main file,
as ``.rest`` is not recognized by github/gitlab,
who also don't support file inclusion,
so no need for two extension anyway.

Examples usages with the files generated by ``rstdoc --stpl tmp``:

.. code-block:: sh

    cd tmp/doc
    rstdcx   #expand .stpl and produce .tag and _links_xxx files

    #expand stpl and append substitutions (for simple expansion use ``stpl <file> .``)
    rstdcx dd.rest.stpl - rest           # expand to stdout, appending dd.html substitutions, to pipe to Pandoc
    rstdcx dd.rest.stpl - html.          # as before
    rstdcx dd.rest.stpl - docx.          # expand to stdout, appending dd.docx substitutions, to pipe to Pandoc
    rstdcx dd.rest.stpl - newname.docx.  # expand template, appending substitutions for target newname.docx
    rstdcx dd.rest.stpl - html           # expand to stdout, already process through Pandoc to produce html on stdout
    rstdcx dd.rest.stpl                  # as before
    rstdcx sy.rest.stpl - rst_html       # expand template, already process through Docutils to produce html on stdout
    stpl sy.rest.stpl | rstdcx - - sy.html. # appending sy.html substitutions, e.g. to pipe to Pandoc
    stpl dd.rest.stpl | rstdcx - - dd.html  # appending tp.html substitutions and produce html on stdout via Pandoc
    rstdcx dd.rest.stpl dd.rest          # expand into dd.rest, appending substitutions for target dd.html
    rstdcx dd.rest.stpl dd.html html     # expand template, process through Pandoc to produce dd.html
    rstdcx dd.rest.stpl dd.html          # as before
    rstdcx dd.rest.stpl dd.html rst_html # expand template, already process through Docutils to produce dd.html
    rstdcx dd.rest.stpl dd.docx          # expand template, process through Pandoc to produce dd.docx
    rstdcx dd.rest.stpl dd.odt pandoc    # expand template, process through Pandoc to produce dd.odt
    rstdcx dd.rest.stpl dd.odt           # as before
    rstdcx dd.rest.stpl dd.odt rst_odt   # expand template, process through Docutils to produce dd.odt
    rstdcx dd.rest.stpl dd.odt rst       # as before
    rstdcx . build html                  # convert current dir to build output dir using pandoc
    rstdcx . build sphinx_html           # ... using sphinx (if no index.rest, every file separately)

    #Sphinx is not file-oriented
    #but with rstdcx you need to provide the files to give Sphinx ``master_doc`` (normally: index.rest)
    #Directly from ``.stpl`` does not work with Sphinx
    rstdcx index.rest ../build/index.html sphinx_html   # via Sphinx the output directory must be different

    #convert the graphics and place the into _images or <updir>/_images
    #if no _images directory exists they will be placed into the same directory
    rstdcx egcairo.pyg
    rstdcx egdot.dot.stpl
    rstdcx egeps.eps
    rstdcx egother.pyg
    rstdcx egplt.pyg
    rstdcx egpygal.pyg
    rstdcx egpyx.pyg
    rstdcx egsvg.svg.stpl
    rstdcx egtikz.tikz
    rstdcx egtikz1.tikz
    rstdcx eguml.uml

    #Convert graphics to a png (even if _images directory exists):
    rstdcx eguml.uml eguml.png

    #Files to other files:

    rstdoc dd.rest.stpl dd.rest
    rstdoc dd.rest.stpl dd.html html
    rstdoc dd.rest.stpl dd.html
    rstdoc sr.rest.stpl sr.html rst_html
    rstdoc dd.rest.stpl dd.docx
    rstdoc dd.rest.stpl dd.odt pandoc
    rstdoc dd.rest.stpl dd.odt
    rstdoc sr.rest.stpl sr.odt rst_odt
    rstdoc sr.rest.stpl sr.odt rst
    rstdoc index.rest build/index.html sphinx_html

    #Directories to other directories with out info:

    rstdoc . build html
    rstdoc . build sphinx_html

Grep with python re in .py, .rst, .rest, .stpl, .tpl::

    rstdoc --pygrep inline

Grep for keyword lines containing 'png'::

    rstdoc --kw png

Default keyword lines::

    .. {{{kw1,kw2
    .. {kw1,kw2}
    {{_ID3('kw1 kw2')}}
    %__ID3('kw1 kw2')
    :ID3: kw1 kw2

"""

)


def main(**args):
    '''
    This corresponds to the |rstdcx| shell command.

    '''

    import argparse

    if not args:
        parser = argparse.ArgumentParser(description=description,
                          formatter_class=argparse.RawDescriptionHelpFormatter)
        parser.add_argument('--version', action='version', version = __version__)
        parser.add_argument(
            '--rstrest',
            dest='rstrest',
            action='store_true',
            default=False,
            help='For the sample projects, make .rst main files and .rest included, reversing default.')
        parser.add_argument(
            '--rest',
            dest='restroot',
            action='store',
            help='Create a sample directory with `<arg>/doc/{sy,ra,sr,dd,tp}.rest` files.')
        parser.add_argument(
            '--stpl',
            dest='stplroot',
            action='store',
            help='Create a sample directory with `<arg>/doc/{sy,ra,sr,dd,tp}.rest.stpl` files.')
        parser.add_argument(
            '--ipdt',
            dest='ipdtroot',
            action='store',
            help='Create a sample directory with `<arg>/pdt/AAA/{i,p,d,t}.rest.stpl` for inform-plan-do-test cycles (A is base 36).')
        parser.add_argument(
            '--over',
            dest='overroot',
            action='store',
            help='Create a sample directory with `.rest` files all over the project tree.')
        parser.add_argument(
            '--pygrep',
            dest='pygrep',
            action='store',
            help='Grep rst doc using python regular expressions.')
        parser.add_argument(
            '--kw',
            dest='kw',
            action='store',
            help='List keyword lines (.. {kw1,kw2,...}) that contain all given as parameter, e.g kw1,kw2.')
        parser.add_argument(
            '-I',
            action='append',
            metavar='folder',
            nargs=1,
            help='Add folders to look for ``conf.py``, ``.[s]tpl`` and reference.docx/odt/tex')
        parser.add_argument(
            'infile',
            nargs='?',
            help='''\
Integrates Sphinx, Pandoc and Docutils to produce output supported by any of them.
To use all three, restructuredText must not use Sphinx extensions.
Input file, dir or - for stdin.''')
        parser.add_argument(
            'outfile',
            nargs='?',
            help='Output file, dir or - or nothing to print to std out.')
        parser.add_argument(
            'outtype',
            nargs='?',
            default=None,
            help="""One of {pandoc,sphinx,}x{html,docx,...}
or omitted for default (pandoc) (- if further code paramters are given)."""
        )
        parser.add_argument(
            'code',
            nargs='*',
            help="""Further parameters are python code,
to define variables that can be used in templates."""
        )
        args = parser.parse_args().__dict__

    if 'code' in args and args['code'] is not None and args['code'] != []:
        code = '\n'.join(args['code'])
        eval(compile(code, '<string>', 'exec'), globals())

    _chk_rstrest = lambda :'stplroot' in args and args['rstrest'] and _set_rstrest('.rst')
    if 'stplroot' in args and args['stplroot']:
        _chk_rstrest()
        initroot(args['stplroot'], 'stpl')
        return
    elif 'restroot' in args and args['restroot']:
        _chk_rstrest()
        initroot(args['restroot'], 'rest')
        return
    elif 'ipdtroot' in args and args['ipdtroot']:
        _chk_rstrest()
        initroot(args['ipdtroot'], 'ipdt')
        return
    elif 'overroot' in args and args['overroot']:
        _chk_rstrest()
        initroot(args['overroot'], 'over')
        return

    if 'pygrep' in args and args['pygrep']:
        for f,i,l in grep(args['pygrep']):
            print('"{}":{} {}'.format(f,i,l))
    elif 'kw' in args and args['kw']:
        for _, (f,i,l) in yield_with_kw(args['kw']):
            print('"{}":{} {}'.format(f,i,l))
    elif 'infile' in args and args['infile']:
        for x in 'infile outfile outtype'.split():
            if x not in args:
                args[x] = None
        if 'I' in args and args['I']:
            g_include[:] = reduce(lambda x,y:x+y,args['I'],[])
        outinfo = args['outtype'] if args['outtype'] != '-' else None
        outfile = args['outfile']
        infiles = [args['infile']]
        outfiles = [args['outfile']]
        notexistsout = outfile and outfile!='-' and not exists(outfile)
        imgfiles = []
        if isdir(args['infile']):
            with new_cwd(args['infile']):
                _get_rstrest()
                index_dir(args['infile'])
            if outfile is None:
                return
            imgfiles = [x for x in os.listdir(args['infile']) if _is_graphic(stem_ext(x)[1])]
            infiles = [x for x in os.listdir(args['infile']) if is_rest(x)]
            if notexistsout:
                mkdir(outfile)
        elif outfile:
            bout = base(outfile)
            fdo = bout.find('.')
            if notexistsout and not (fdo>0 and fdo<len(bout)-1):
                mkdir(outfile)
        if outinfo and outfile and isdir(outfile):
            if outinfo.startswith('sphinx'):
                onlyindex = [x for x in infiles if x.find('index.')>=0]
                if len(onlyindex)>0:
                    infiles = onlyindex
            outfiles = [normjoin(outfile, _in_2_out_name(inf,outinfo)) for inf in infiles]
        for i in imgfiles:
            convert(i,None)
        for i,o in zip(infiles,outfiles):
            for rxt in [_rst,_rest]:
                if i.endswith(rxt) or i.endswith(rxt+_stpl):
                    _set_rstrest(rxt)
            convert(i,o,outinfo)
    else:
        myroot = up_dir(is_project_root_file)
        if myroot:
            with new_cwd(myroot):
                _get_rstrest()
                index_dir()
                return
        _get_rstrest()
        index_dir()

if __name__ == '__main__':
    main()

# vim: ts=4 sw=4 sts=4 et noai nocin nosi
