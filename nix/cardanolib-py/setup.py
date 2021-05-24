import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="cardanolib-py",
    version="1.0.0",
    author="Samuel Leathers",
    author_email="samuel.leathers@iohk.io",
    description="Library for interacting with cardano-cli in python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/input-output-hk/cardano-node",
    py_modules= ["cardanolib"],
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache License",
        "Operating System :: OS Independent",
    ],
)
