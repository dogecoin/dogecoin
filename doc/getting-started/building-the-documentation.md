# Building the documentation

## Pre-requisites

```bash
brew install sphinx-doc
```

Ensure any post install steps are executed.

```bash
sphinx-build -b html . builddir
pip3 install sphinx-rtd-theme
pip3 install recommonmark
pip3 install sphinx_markdown_tables --user
pip3 install sphinxemoji --user
```

## Building documentation

```bash
sphinx-build doc html
```

Open the documentation at `html/index.html`
