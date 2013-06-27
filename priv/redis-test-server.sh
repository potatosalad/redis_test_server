#!/bin/sh
(cat && kill 0) | redis-server --port $1