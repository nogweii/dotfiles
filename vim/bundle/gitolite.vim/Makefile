PLUGIN = gitolite

vimdir = $(HOME)/.vim

VIM = vim
INSTALL = install
RM = rm -f

VIM_FILES := $(wildcard */*.vim)

ifneq ($(findstring $(MAKEFLAGS),s),s)
ifndef V
	QUIET_GEN = @echo '   ' GEN $@;
endif
endif

all: $(PLUGIN).vba.gz

$(PLUGIN).vba.gz: $(PLUGIN).vba
	gzip -9 < $< > $@

$(PLUGIN).vba: $(VIM_FILES)
	$(QUIET_GEN)printf "%s\n" $^ | $(VIM) \
		-c 'let g:vimball_home="."' \
		-c '%MkVimball! $(PLUGIN)' \
		-c 'q!' - > /dev/null

clean:
	$(RM) $(PLUGIN).vba
	$(RM) $(PLUGIN).vba.gz

install-vba: $(PLUGIN).vba
	$(VIM) -c 'source %' -c 'q' $<

VIM_DIRS := $(addprefix $(DESTDIR)$(vimdir)/,$(dir $(VIM_FILES)))
install:
	$(INSTALL) -d -m 755 $(VIM_DIRS)
	@for f in $(VIM_FILES); do \
		$(INSTALL) -v -m 644 "$$f" "$(DESTDIR)$(vimdir)/$$f"; \
	done

dist:
	git archive --prefix='$(PLUGIN).vim/' HEAD | gzip -9 > $(PLUGIN).vim.tar.gz

.PHONY: all clean install dist
