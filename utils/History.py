import pandas as pd
import numpy as np
from typing import Callable


class History:
    float_deafault_fmt = ":.3e"

    def __init__(self):
        self.__records: dict = {}
        self.__num_it: int = 0
        self.__formats: dict = {}
        self.__iterates: dict = {}
        self.__num_iterates: int = 0

    def update(self, *record: dict):

        for r in record:
            for key, value in r.items():
                if key not in self.__records:
                    self.__records[key] = [np.nan] * self.__num_it
                self.__records[key].append(r[key])

        self.__num_it += 1

    def add_iterate(self, *record: dict):

        for r in record:
            for key, value in r.items():
                if key not in self.__iterates:
                    self.__iterates[key] = [np.nan] * self.__num_iterates
                self.__iterates[key].append(r[key])

        self.__num_iterates += 1

    def set_formats(self, formats: dict):
        self.__formats.update(formats)

    def print_last(self):

        for key in self.__records.keys():
            if key not in self.__formats:
                self.__formats[key] = History.float_deafault_fmt if type(self.__records[key][0]) in [float,
                                                                                                     np.float64] else ""

        s = "it: {} -> ".format(self.__num_it) + ", ".join(
            [("{}: {" + self.__formats[key] + "}").format(key, value[-1]) for key, value in self.__records.items()])
        print(s)

    @property
    def dataframe(self) -> pd.DataFrame:
        df = pd.DataFrame(self.__records)
        df['it'] = np.arange(self.__num_it)
        df.set_index('it', inplace=True)
        return df

    @property
    def iterates(self):
        return self.__iterates


class Counter:

    def __init__(self, fnc: Callable, name: str):
        self.__fnc: Callable = fnc
        self.__lap_count: int = 0
        self.__count: int = 0
        self.__name: str = 'num_{}'.format(name)

    def __call__(self, *args, **kwargs):
        self.__count += 1
        self.__lap_count += 1
        return self.__fnc(*args, **kwargs)

    @property
    def name(self) -> str:
        return self.__name

    @property
    def count(self) -> int:
        return self.__count

    @property
    def lap(self):
        c = self.__lap_count
        self.__lap_count = 0
        return {self.__name: c}
