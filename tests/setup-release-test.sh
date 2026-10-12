#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
# Isolated regression tests: no network, package installation, or root writes.
set -eu
REPO_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TEST_TMP=$(mktemp -d)
trap 'rm -rf "$TEST_TMP"' EXIT
trap 'exit 1' HUP INT TERM
mkdir -p "$TEST_TMP/bin" "$TEST_TMP/payload" "$TEST_TMP/downloads" "$TEST_TMP/tmp"
export TEST_TMP
printf '#!/bin/sh\necho just-fixture\n' > "$TEST_TMP/payload/just"
ARCHIVE=just-1.58.0-x86_64-unknown-linux-musl.tar.gz
tar -czf "$TEST_TMP/$ARCHIVE" -C "$TEST_TMP/payload" just
(cd "$TEST_TMP" && sha256sum "$ARCHIVE" > SHA256SUMS)
cp "$TEST_TMP/$ARCHIVE" "$TEST_TMP/downloads/$ARCHIVE"
cp "$TEST_TMP/SHA256SUMS" "$TEST_TMP/downloads/SHA256SUMS"
# Trust our fixture manifest in an isolated copy; all production logic is kept.
FIXTURE_HASH=$(sha256sum "$TEST_TMP/SHA256SUMS" | cut -d ' ' -f 1)
sed -e "s/^    JUST_SUMS_SHA256=.*/    JUST_SUMS_SHA256=$FIXTURE_HASH/" \
    -e '/^main "\$@"$/d' "$REPO_ROOT/build/setup.sh" > "$TEST_TMP/setup.sh"
cat > "$TEST_TMP/bin/curl" <<'SH'
#!/bin/sh
set -eu
[ "${TEST_MODE:-}" != download-failure ] || exit 22
for arg do
    case "$arg" in https://*) source_file=${arg##*/} ;; esac
    destination=$arg
done
cp "$TEST_TMP/downloads/$source_file" "$destination"
SH
cat > "$TEST_TMP/bin/install" <<'SH'
#!/bin/sh
set -eu
[ "${TEST_MODE:-}" != install-failure ] || exit 1
[ "$1 $2 $3 $4" = '-m 755 just /usr/local/bin/just' ]
cmp just "$TEST_TMP/payload/just"
touch "$TEST_TMP/installed"
SH
chmod +x "$TEST_TMP/bin/curl" "$TEST_TMP/bin/install"
export PATH="$TEST_TMP/bin:$PATH"
export TMPDIR="$TEST_TMP/tmp"
run_case() {
    expected=$1
    rm -f "$TEST_TMP/installed"
    # Invoke in a conditional, like main: failures must not rely on set -e.
    if sh -c '. "$TEST_TMP/setup.sh"; OS=linux; ARCH=x86_64; if install_just_release; then exit 0; else exit 1; fi' > "$TEST_TMP/result.log" 2>&1; then
        actual=pass
    else
        actual=fail
    fi
    if [ "$actual" != "$expected" ]; then
        cat "$TEST_TMP/result.log"
        echo "FAIL: $TEST_MODE expected $expected, got $actual" >&2
        exit 1
    fi
    if [ "$expected" = pass ]; then
        test -f "$TEST_TMP/installed"
    else
        test ! -f "$TEST_TMP/installed"
    fi
    test -z "$(ls -A "$TMPDIR")"
    echo "PASS: $TEST_MODE (including temporary-file cleanup)"
}
TEST_MODE=valid; export TEST_MODE
run_case pass
TEST_MODE=corrupt-manifest
printf 'tampered\n' >> "$TEST_TMP/downloads/SHA256SUMS"
run_case fail
cp "$TEST_TMP/SHA256SUMS" "$TEST_TMP/downloads/SHA256SUMS"
TEST_MODE=corrupt-archive
printf 'tampered\n' >> "$TEST_TMP/downloads/$ARCHIVE"
run_case fail
cp "$TEST_TMP/$ARCHIVE" "$TEST_TMP/downloads/$ARCHIVE"
TEST_MODE=download-failure
run_case fail
TEST_MODE=install-failure
run_case fail
