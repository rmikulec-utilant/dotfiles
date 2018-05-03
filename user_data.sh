#!/bin/bash

pip install jupyter_contrib_nbextensions
pip install jupyter_nbextensions_configurator
pip install autopep8
jupyter contrib nbextension install --system
jupyter nbextensions_configurator enable --system

exit 0
