#!/bin/sh
(cat && kill 0) | redis-server $@