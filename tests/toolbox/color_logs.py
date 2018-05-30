import logging
from termcolor import colored

# from https://gist.github.com/brainsik/1238935


class ColorLog(object):

    colormap = dict(
        info=dict(color='green', attrs=['bold']),        # success messges
        debug=dict(color='white'),                       # info
        warn=dict(color='yellow', attrs=['bold']),       # warning
        warning=dict(color='yellow', attrs=['bold']),    # warning
        error=dict(color='red'),                         # failure
        critical=dict(color='red', attrs=['bold']),      # failure
    )

    def __init__(self, logger):
        self._log = logger

    def __getattr__(self, name):
        if name in ['debug', 'info', 'warn', 'warning', 'error', 'critical']:
            return lambda s, *args: getattr(self._log, name)(
                colored(s, **self.colormap[name]), *args)

        return getattr(self._log, name)


log = ColorLog(logging.getLogger(__name__))
