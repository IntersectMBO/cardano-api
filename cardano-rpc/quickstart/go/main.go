package main

import (
	"encoding/hex"
	"fmt"

	sdk "github.com/utxorpc/go-sdk"
	"github.com/utxorpc/go-sdk/cardano"
)

const rpcURL = "http://localhost:50051"

func main() {
	client := cardano.NewClient(sdk.WithBaseUrl(rpcURL))

	tipResp, err := client.GetTip()
	if err != nil {
		panic(err)
	}
	tip := tipResp.Msg.GetTip()
	fmt.Printf("Tip: slot %d height %d hash %s\n", tip.GetSlot(), tip.GetHeight(), hex.EncodeToString(tip.GetHash()))

	paramsResp, err := client.GetProtocolParameters()
	if err != nil {
		panic(err)
	}
	pparams := paramsResp.Msg.GetValues().GetCardano()
	fmt.Printf("Protocol parameters: max_tx_size %d max_block_body_size %d\n", pparams.GetMaxTxSize(), pparams.GetMaxBlockBodySize())
}
