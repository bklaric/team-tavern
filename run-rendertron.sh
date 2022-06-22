#!/bin/bash
pkill rendertron
pkill chrome
rendertron &>> dist-server/rendertron.txt &
