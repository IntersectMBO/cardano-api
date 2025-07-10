import * as jspb from 'google-protobuf'

import * as google_protobuf_empty_pb from 'google-protobuf/google/protobuf/empty_pb'; // proto import: "google/protobuf/empty.proto"


export class CurrentEra extends jspb.Message {
  getEra(): Era;
  setEra(value: Era): CurrentEra;

  serializeBinary(): Uint8Array;
  toObject(includeInstance?: boolean): CurrentEra.AsObject;
  static toObject(includeInstance: boolean, msg: CurrentEra): CurrentEra.AsObject;
  static serializeBinaryToWriter(message: CurrentEra, writer: jspb.BinaryWriter): void;
  static deserializeBinary(bytes: Uint8Array): CurrentEra;
  static deserializeBinaryFromReader(message: CurrentEra, reader: jspb.BinaryReader): CurrentEra;
}

export namespace CurrentEra {
  export type AsObject = {
    era: Era,
  }
}

export enum Era { 
  BYRON = 0,
  SHELLEY = 1,
  ALLEGRA = 2,
  MARY = 3,
  ALONZO = 4,
  BABBAGE = 5,
  CONWAY = 6,
}
