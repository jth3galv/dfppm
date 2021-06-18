HEADER = "\documentclass{{article}}\n \
\\usepackage{{multirow}}\n \
\\begin{{document}}\n\n \
\\setlength{{\\tabcolsep}}{{2pt}} \n \
\\begin{{table}}[htb]\n \
\\centering\n \
\\begin{{footnotesize}}\n \
  \\begin{{tabular}}{{|{}}} \n \
    \hline \n \
    \multirow{{2}}{{*}}{{problem}} &"

FOOTER = "  \end{{tabular}}\n \
\end{{footnotesize}}\n \
\end{{table}}\n\n \
\end{{document}}"

paper_res = {
    'HS24': {'delta': 0, 'nf': 24},
    'HS37': {'delta': 0, 'nf': 34},
    'HS44': {'delta': 0, 'nf': 32},
    'HS86': {'delta': 1e-7, 'nf': 107},
    'HS224': {'delta': 0, 'nf': 100},
    'HS232': {'delta': 0, 'nf': 29},
    'HS331': {'delta': 1e-4, 'nf': 104},
    'AS6(N=6)': {'nf': 208},
    'AS6(N=7)': {'nf': 253},
    'AS6(N=8)': {'nf': 300},
    'AS7(N=6)': {'nf': 154},
    'AS7(N=7)': {'nf': 273},
    'AS7(N=8)': {'nf': 253},
    'HS22': {'nf': 120, 'f': 1.},
    'HS43': {'nf': 247, 'f': -2.26 * 1e1}
}


def add_literature(results):
    solvers = list(results.keys())
    problems = list(results[solvers[0]].keys())

    results['literature'] = {}

    for problem in problems:
        f, nf = '-', '-'
        f_star = results[solvers[0]][problem]['f_star']
        if problem.upper() in paper_res:
            p = paper_res[problem.upper()]
            if 'f' in p:
                f = p['f']
            elif 'delta' in p:
                delta = p['delta']
                f = delta * f_star + f_star

            nf = p['nf'] if 'nf' in p else '-'

        results['literature'][problem] = {'f_x': f, 'nf': nf, 'f_star': f_star}


def format_table(results, field_names, field_formats, field_fncs, problems=None) -> str:

    n_fields = len(field_names)
    assert n_fields == len(field_fncs)

    solvers = list(results.keys())[::-1]
    keys = list(results[solvers[0]].keys())
    problems = keys if problems is None else problems

    space = '       '

    header = HEADER.format('c|' * (len(solvers) * n_fields + 1))
    header += " & ".join(['\multicolumn{{{}}}{{c}}{{{}}}'.format(n_fields, s) for s in solvers[:-1]])
    header += ' & \multicolumn{{{}}}{{c|}}{{{}}}'.format(n_fields, solvers[-1])
    header += "\\\\\n"
    header += space + "& " + " & ".join([" & ".join(field_names) for i in range(len(solvers))]) + "  \\\\\n"
    header += space + "\hline\n"

    body = ""
    for p in problems:
        body += space + p + "& "
        v = []
        for s in solvers:
            v += [("{" + field_formats[i] + "}").format(field_fncs[i](results[s][p.upper()])) for i in range(n_fields)]
        body += " & ".join(v) + " \\\\\n"
        body += space + "\hline\n"

    return header + body + FOOTER.format()
