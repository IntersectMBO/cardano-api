module Cardano.Api.Query.New.IPC.Generic where



-- Left off here: LocalNodeClientProtocolsInMode is defined
-- to handle QueryInMode. We want to expose a generic function
-- that lets you specify the query you want. Maybe something like

type LocalNodeClientProtocolsInMode query =
  LocalNodeClientProtocols
    BlockInMode
    ChainPoint
    ChainTip
    SlotNo
    TxInMode
    TxIdInMode
    TxValidationErrorInCardanoMode
    query
    IO

-- | Convert from the mode-parametrised style to the block-parametrised style.
--
mkLocalNodeClientParams ::
     ConsensusModeParams
  -> (NodeToClientVersion -> LocalNodeClientProtocolsInMode)
  -> LocalNodeClientParams
mkLocalNodeClientParams modeparams clients =
    -- For each of the possible consensus modes we pick the concrete block type
    -- (by picking the appropriate 'ProtocolClient' value).
    --
    -- Though it is not immediately visible, this point where we use
    -- 'LocalNodeClientParams' is also where we pick up the necessary class
    -- instances. This works because in each case we have a monomorphic block
    -- type and the instances are all in scope. This is why the use of
    -- LocalNodeClientParams is repeated within each branch of the case:
    -- because it is only within each branch that the GADT match makes the
    -- block type monomorphic.
    --
    case modeparams of
      CardanoModeParams epochSlots ->
       LocalNodeClientParamsCardano
         (ProtocolClientInfoArgsCardano epochSlots)
         (convLocalNodeClientProtocols . clients)
