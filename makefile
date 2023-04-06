
MAIN := $(BUILD_DIR)/Main
TARGET_MAIN := ./Main.hs
TEST_MAIN := ./Tests/Test.hs

BUILD_DIR := ./buildDir

MAIN_FILES := $(shell find . -name '*.hs' -exec grep -q "main =" {} ';' -print)
SRC_FILES := $(shell find . -name '*.hs' \! -exec grep -q "main =" {} ';' -print)

.PHONY: run
run: $(MAIN)
	$(BUILD_DIR)/Main

.PHONY: test
test: clean $(BUILD_DIR)
	ghc -O2 -outputdir $(BUILD_DIR) -i $(SRC_FILES) -o $(BUILD_DIR)/Main $(TEST_MAIN)
	$(BUILD_DIR)/Main

$(MAIN): clean $(BUILD_DIR)
	ghc -O2 -outputdir $(BUILD_DIR) -i $(SRC_FILES) -o $(BUILD_DIR)/Main $(TARGET_MAIN)

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

.PHONY: clean
clean:
	-rm -rf $(BUILD_DIR)

