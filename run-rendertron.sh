#!/bin/bash
pkill rendetron
pkill chrome
rendertron &>> dist-server/rendertron.txt &
