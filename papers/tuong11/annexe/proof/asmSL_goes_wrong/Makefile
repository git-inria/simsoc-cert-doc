# SimSoC-Cert, a toolkit for generating certified processor simulators
# See the COPYRIGHTS and LICENSE files.

DIR := ../../../../..

FILES := simlight

VFILES := $(FILES:%=%.v)

COQ_INCLUDES := -I $(DIR)/coq

include $(DIR)/Makefile.common

default: simlight.vo

simlight.vo: libcoq
