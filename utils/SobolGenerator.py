import sobol_seq


class SobolGenerator:

    def __init__(self, seed: int):
        self.__seed = seed

    def gen(self, n):
        """
        :param n: dimension of the output vector
        :returns: a quasi-random generated vector of dimension (n, 1)
        """
        vec, self.__seed = sobol_seq.i4_sobol(n, self.__seed)
        return vec.reshape(-1, 1)


if __name__ == '__main__':

    rnd = SobolGenerator(12)

    print(rnd.gen(3))
