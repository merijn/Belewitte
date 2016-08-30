import locale

from functools import total_ordering

@total_ordering
class Measurement:
    def __init__(self, measurements=None):
        if measurements:
            measurements = map(locale.atof, measurements.split(' '))
            self.min = measurements[0]
            self.avg = measurements[1]
            self.max = measurements[2]
            self.stddev = measurements[3]
            self.initialised = True
        else:
            self.min = 0.0
            self.avg = 0.0
            self.max = 0.0
            self.stddev = 0.0
            self.normalised = 0.0
            self.initialised = False

    def normalise(self, other):
        if self.initialised and other.initialised and other.avg != 0:
            self.normalised = self.avg / other.avg
        else:
            self.normalised = 0.0
        return self

    def __eq__(self, other):
        if self.initialised and other.initialised:
            return (self.min == other.min
                and self.avg == other.avg
                and self.max == other.max
                and self.stddev == other.stddev)
        else:
            return False

    def __lt__(self, other):
        if self.initialised and other.initialised:
            return self.avg < other.avg
        elif other.initialised:
            return False
        else:
            return True

    def __repr__(self):
        result = "Measurement {\n\tinitialised: " + str(self.initialised) + "\n"
        if self.initialised:
            result += "\tmin: " + str(self.min) + "\n"
            result += "\tavg: " + str(self.avg) + "\n"
            result += "\tmax: " + str(self.max) + "\n"
            result += "\tstddev: " + str(self.stddev) + "\n"
            try:
                result += "\tnormalised: " + str(self.normalised) + "\n"
            except AttributeError:
                pass
        result += "}\n"
        return result
