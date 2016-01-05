PROJECT = selleck
PROJECT_DESCRIPTION = Serve mustache files
PROJECT_VERSION = 1.0.0
DEPS = jiffy cowboy bbmustache

dep_bbmustache = git https://github.com/soranoba/bbmustache.git
include erlang.mk
