.PHONY: all clean generate

EXECUTABLE_NAME = interpreter
GENERATED_DIR = src/generated

CABAL = cabal
CABAL_BUILD = $(CABAL) build --enable-executable-dynamic

DIST_DIR = dist-newstyle/build
EXECUTABLE_DIR = $(DIST_DIR)/x86_64-linux/ghc-*/interpreter-*/x/$(EXECUTABLE_NAME)/build/$(EXECUTABLE_NAME)

all: generate $(EXECUTABLE_NAME)

$(EXECUTABLE_NAME):
	$(CABAL_BUILD)
	@echo "Moving executable to current directory..."
	@mv $(EXECUTABLE_DIR)/$(EXECUTABLE_NAME) .

generate:
	@echo "Generating code from Fox.cf..."
	@mkdir -p $(GENERATED_DIR)
	@bnfc -d -m --functor --haskell -o $(GENERATED_DIR) Fox.cf

clean:
	$(CABAL) clean
	@rm -rf $(EXECUTABLE_NAME) $(GENERATED_DIR)
