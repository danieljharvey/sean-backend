#!/bin/bash

stack --docker-run-args='--net=bridge --publish=8080:8080' exec Sean-exe
