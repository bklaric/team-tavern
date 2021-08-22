#!/bin/bash
fuser -k 3000/tcp
rendertron >> rendertron.txt &
