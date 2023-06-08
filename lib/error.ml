type protocol_error =
  | NonZeroReservedBit
  | WrongOpcode
  | FragmentedControlFrame
  | FrameIsNotContinuation
  | FirstFrameIsContinuation
  | PingPayloadToLarge
  | DecodeText
  | WrongCloseCode
  | ProtocolEndOfFile

type http_error =
  | ParsingHeader of {header_line: string}
  | SecKeyDoesntExists
  | InvalidSecKey
  | HttpEndOfFile
