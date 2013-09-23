
empty=
space=$(empty) $(empty)

coffee_files = $(basename $(wildcard *.coffee))
sass_files = $(basename $(wildcard *.sass))

targets :=

define coffee_rule
js/$(1).js: $(1).coffee
	coffee -c -o js $$(arg1)
targets += js/$(1).js
endef

define sass_rule
$(1).css: $(1).sass
	sass $$(arg1) $$@
targets += $(1).css
endef

$(foreach coffee,$(coffee_files),$(eval $(call coffee_rule,$(coffee))))
$(foreach sass,$(sass_files),$(eval $(call sass_rule,$(sass))))

.PHONY: all

all: $(targets)

clean:
	rm -rf $(targets)

arg1 = $(word 1,$+)
arg2 = $(word 2,$+)
arg3 = $(word 3,$+)
