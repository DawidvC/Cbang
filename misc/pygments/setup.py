from setuptools import setup

setup(
    name = 'pygments-cbang',
    version = '0.1',
    py_modules = ['pygments_cbang'],

    install_requires = ['pygments'],

    entry_points = {
        'pygments.lexers': 'cbanglexer = pygments_cbang:CBangLexer',
    },

    author = 'Pierre-Marie de Rodat',
    author_email = 'pmderodat@lse.epita.fr',
    description = 'Pygments lexer for the C! language',
    license = 'BSD',
    keywords = 'pygments cbang',
    url = 'http://lse.epita.fr/projects/c!.html'
)
