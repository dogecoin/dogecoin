#!/usr/bin/env bash

function assert () {
  got=$1; want=$2

  if [[ $got =~ "$want" ]]; then
    echo "✓ $want"
  else
    echo -e "✗ $got\ndoes not match \"$want\""
  fi
}

function fail_if_server_is_not_running () {
  URL=$1
  # Check if the server is running
  TMP=$(mktemp)
  curl -v --stderr $TMP $URL
  if [[ $(cat $TMP) =~ "Failed to connect" ]]; then
    echo "cardano-submit-api must be up-and-running on ${URL%%/api*} to execute the test bench."
    exit 1
  fi;
}

URL=http://localhost:8090/api/submit/tx
DATA="83a40081825820c84dbd7b780ff9be2bcc6990f043030fe649342f9fa7ba1bfa6fb5c0079501a400018282582b82d818582183581cf810f5ffa3613a7edcc1e733d0d73c89645d02221648a031436fd840a0001a4f7ed7fd1a45bb41e082581d619fc8d50cd36af0e35f43db0e2f220e4ccb774925f2ef3542875b37701a000f4240021a00030d40031a00f9c7cca102818458205e3eb96f2cfd7844b5eed5498fe487fc5271178fb251ebf3d33c5d8f49036d9358403c9ef2ce22b8a3fce622472f84c932679f95e0f3e139378fe9d6ab5442c60a9b87de374a0bc37667b1bf8d5c6bffb436ffd84b0fa56a7c86342962b9c293390758208ed6ca9faabd8499f021c2e51745436c99f9eab6c92f4b09b255bdd9f39bae2a41a0f6"
# 83                                      # array(3)
#    A4                                   # map(4)
#       00                                # unsigned(0)
#       81                                # array(1)
#          82                             # array(2)
#             58 20                       # bytes(32)
#                C84DBD7B780FF9BE2BCC6990F043030FE649342F9FA7BA1BFA6FB5C0079501A4 # "\xC8M\xBD{x\x0F\xF9\xBE+\xCCi\x90\xF0C\x03\x0F\xE6I4/\x9F\xA7\xBA\e\xFAo\xB5\xC0\a\x95\x01\xA4"
#             00                          # unsigned(0)
#       01                                # unsigned(1)
#       82                                # array(2)
#          82                             # array(2)
#             58 2B                       # bytes(43)
#                82D818582183581CF810F5FFA3613A7EDCC1E733D0D73C89645D02221648A031436FD840A0001A4F7ED7FD # "\x82\xD8\x18X!\x83X\x1C\xF8\x10\xF5\xFF\xA3a:~\xDC\xC1\xE73\xD0\xD7<\x89d]\x02\"\x16H\xA01Co\xD8@\xA0\x00\x1AO~\xD7\xFD"
#             1A 45BB41E0                 # unsigned(1169900000)
#          82                             # array(2)
#             58 1D                       # bytes(29)
#                619FC8D50CD36AF0E35F43DB0E2F220E4CCB774925F2EF3542875B3770 # "a\x9F\xC8\xD5\f\xD3j\xF0\xE3_C\xDB\x0E/\"\x0EL\xCBwI%\xF2\xEF5B\x87[7p"
#             1A 000F4240                 # unsigned(1000000)
#       02                                # unsigned(2)
#       1A 00030D40                       # unsigned(200000)
#       03                                # unsigned(3)
#       1A 00F9C7CC                       # unsigned(16369612)
#    A1                                   # map(1)
#       02                                # unsigned(2)
#       81                                # array(1)
#          84                             # array(4)
#             58 20                       # bytes(32)
#                5E3EB96F2CFD7844B5EED5498FE487FC5271178FB251EBF3D33C5D8F49036D93 # "^>\xB9o,\xFDxD\xB5\xEE\xD5I\x8F\xE4\x87\xFCRq\x17\x8F\xB2Q\xEB\xF3\xD3<]\x8FI\x03m\x93"
#             58 40                       # bytes(64)
#                3C9EF2CE22B8A3FCE622472F84C932679F95E0F3E139378FE9D6AB5442C60A9B87DE374A0BC37667B1BF8D5C6BFFB436FFD84B0FA56A7C86342962B9C2933907 # "<\x9E\xF2\xCE\"\xB8\xA3\xFC\xE6\"G/\x84\xC92g\x9F\x95\xE0\xF3\xE197\x8F\xE9\xD6\xABTB\xC6\n\x9B\x87\xDE7J\v\xC3vg\xB1\xBF\x8D\\k\xFF\xB46\xFF\xD8K\x0F\xA5j|\x864)b\xB9\xC2\x939\a"
#             58 20                       # bytes(32)
#                8ED6CA9FAABD8499F021C2E51745436C99F9EAB6C92F4B09B255BDD9F39BAE2A # "\x8E\xD6\xCA\x9F\xAA\xBD\x84\x99\xF0!\xC2\xE5\x17ECl\x99\xF9\xEA\xB6\xC9/K\t\xB2U\xBD\xD9\xF3\x9B\xAE*"
#             41                          # bytes(1)
#                A0                       # "\xA0"
#    F6                                   # primitive(22)

fail_if_server_is_not_running $URL

echo -e "=== Data is Base16 ==="
RESULT=$(curl --silent -H "Content-Type: application/cbor" -X POST -d $DATA $URL)
assert "$RESULT" "Provided data was hex encoded and this webapi expects raw binary"

echo -e "=== Data is Base64 ==="
BASE64=$(xxd -r -p <<< $DATA | base64 -w 0)
RESULT=$(curl --silent -H "Content-Type: application/cbor" -X POST -d $BASE64 $URL)
assert "$RESULT" "Deserialisation failure while decoding Shelley Tx.\nCBOR failed with error: DeserialiseFailure 0 \\\"expected list len or indef\\\""

echo -e "=== Data is Empty ==="
TMP=$(mktemp)
RESULT=$(curl --silent -H "Content-Type: application/cbor" -X POST --data-binary @$TMP $URL)
assert "$RESULT" "Provided transaction has zero length"

echo -e "=== Content-Type is not 'application/cbor' ==="
TMP=$(mktemp)
RESULT=$(curl -v --stderr $TMP -H "Content-Type: application/json" -X POST --data-binary @$TMP $URL)
assert "$(cat $TMP)" "415 Unsupported Media Type"

echo -e "=== Data is well-formed but invalid ==="
TMP=$(mktemp)
xxd -r -p <<< $DATA > $TMP
RESULT=$(curl --silent -H "Content-Type: application/cbor" -X POST --data-binary @$TMP $URL)
assert "$RESULT" "ApplyTxError"
