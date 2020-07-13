# Run the program every time a source file changes

# `cabal run` doesn't seem to provide a good way to separate its own output from
# the output of the program it's running, so to work around that, we build
# first, then execute if the build succeeded
EXEC_NAME="problamming"
TEST_NAME="$EXEC_NAME-test"
SHARED_OPTS="--disable-optimization"
BUILD_CMD="cabal build $EXEC_NAME $SHARED_OPTS"
BUILD_LOG="build.log"
BUILD="$BUILD_CMD >$BUILD_LOG"
SRC="src"
TEST="test"

case $1 in
    test)
        RUN="cabal test $TEST_NAME $SHARED_OPTS"
        ;;
    *)
        RUN="$BUILD && cabal exec $EXEC_NAME $SHARED_OPTS"
        ;;
esac

exec watchexec "$RUN" \
     --restart \
     --clear \
     --watch "$SRC" \
     --watch "$TEST" \
     --watch cabal.project \
     --watch cabal.project.freeze \
     --ignore '.#*' \
     --ignore 'flycheck*'
