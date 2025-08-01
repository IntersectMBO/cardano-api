#!/usr/bin/env bash

# Function to handle cleanup on exit or signal
cleanup() {
    echo "Cleaning up..."
    if [[ -n "$SOCAT_PID" ]] && kill -0 "$SOCAT_PID" 2>/dev/null; then
        kill "$SOCAT_PID"
        wait "$SOCAT_PID" 2>/dev/null
    fi
    exit
}

# Set trap for SIGINT and SIGTERM
trap cleanup SIGINT SIGTERM

# Start socat in the background
socat TCP-LISTEN:50051,fork UNIX-CONNECT:./rpc.socket &
SOCAT_PID=$!
echo "Started socat with PID $SOCAT_PID"

grpcui -import-path cardano-rpc/proto -proto utxorpc/v1alpha/query/query.proto -plaintext localhost:50051

cleanup
