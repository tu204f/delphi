unit WS.Framing;

interface

uses
  System.Classes,
  WS.Functions;

type
  TWebSocketOpcode = (
    __default = 0,
    ContinuationFrame = $00,
    TextFrame = $01,
    BinaryFrame = $02,
    CloseFrame = $08,
    PingFrame = $09,
    PongFrame = $09
  );

  TWebSocketFrame = class
  public
    function encode: AnsiString; virtual; abstract;
    function getData: AnsiString; virtual; abstract;
    function getType: TWebSocketOpcode; virtual; abstract;
    function isReady: Boolean; virtual; abstract;
    class function decode(var raw: AnsiString; head: TWebSocketFrame = nil): TWebSocketFrame; virtual; abstract;
    constructor Create(code: TWebSocketOpcode; data: AnsiString = ''); virtual; abstract;
    destructor Destroy; override;
  end;

  TWebSocketFrameEnumerator = class
  private
    _pointers: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(tframe: TWebSocketFrame);
    procedure Delete(index: Integer);
    function Count: Integer;
    function Get(index: Integer): TWebSocketFrame;
    property Frame[index: Integer]: TWebSocketFrame read Get;
  end;

  TWebSocketFrameHybie = class(TWebSocketFrame)
  public
    constructor Create(code: TWebSocketOpcode; data: AnsiString = ''); override;
    function encode: AnsiString; override;
    function getData: AnsiString; override;
    function getType: TWebSocketOpcode; override;

    class function decode(var raw: AnsiString; head: TWebSocketFrame = nil): TWebSocketFrame; override;

    function isReady: Boolean; override;
    function isFinal: Boolean;
    function isMasked: Boolean;

  protected
    procedure setType(code: TWebSocketOpcode);
  public
    // First Byte
    FIN: Integer;
    RSV1: Integer;
    RSV2: Integer;
    RSV3: Integer;
    opcode: TWebSocketOpcode;

    // Second Byte
    mask: Integer;
    payloadLength: Integer;
    maskingKey: AnsiString;

    payloadData: AnsiString;
    actualLength: Integer;
  end;

  {
    * Check if an opcode is a control frame. Control frames should be handled internally by the server.
    * @param TWebSocketOpcode type
  }
function isControlFrame(code: TWebSocketOpcode): Boolean;

implementation

uses
  SysUtils, Math;

function IsBitSet(byte: byte; p: Integer): Boolean; forward;
function rotMask(data: AnsiString; key: AnsiString; offset: Integer = 0)
  : AnsiString; forward;

/// /////////////////////////////////////////

{
  * Check if a opcode is a control frame. Control frames should be handled internally by the server.
  * @param TWebSocketOpcode type
}
function isControlFrame(code: TWebSocketOpcode): Boolean;
begin
  Result := false;

  if (code = CloseFrame) or (code = PingFrame) or (code = PongFrame) then
    Result := true;
end;

{
  * HYBIE WebSocketFrame
}
constructor TWebSocketFrameHybie.Create(code: TWebSocketOpcode;
  data: AnsiString = '');
begin
  FIN := 1;
  RSV1 := 0;
  RSV2 := 0;
  RSV3 := 0;
  setType(code);

  mask := 0;
  payloadLength := Length(data);
  maskingKey := '';

  payloadData := data;

  actualLength := 0;
end;

destructor TWebSocketFrame.Destroy;
begin
end;

function TWebSocketFrameHybie.isMasked: Boolean;
begin
  Result := (mask = 1);
end;

procedure TWebSocketFrameHybie.setType(code: TWebSocketOpcode);
begin
  opcode := code;

  if (code = CloseFrame) then
    mask := 1;
end;

function IsBitSet(byte: byte; p: Integer): Boolean;
begin
  Result := (byte and (1 shl p)) > 0;
end;

function rotMask(data: AnsiString; key: AnsiString; offset: Integer = 0)
  : AnsiString;
var
  i, j: Integer;
begin
  for i := 1 to Length(data) do
  begin
    j := (i - 1 + offset) mod 4;
    Result := Result + AnsiChar(Ord(data[i]) xor Ord(key[j + 1]));
  end;
end;

function TWebSocketFrameHybie.getType: TWebSocketOpcode;
begin
  Result := opcode;
end;

function TWebSocketFrameHybie.encode: AnsiString;
var
  firstByte, secondByte: byte;
  encoded, key: AnsiString;
begin
  payloadLength := Length(payloadData);

  firstByte := Integer(opcode);

  firstByte := firstByte + FIN * 128 + RSV1 * 64 + RSV2 * 32 + RSV3 * 16;

  encoded := AnsiChar(firstByte);

  if (payloadLength <= 125) then
  begin
    secondByte := payloadLength;
    secondByte := secondByte + mask * 128;
    encoded := encoded + AnsiChar(secondByte);
  end
  else if (payloadLength <= 255 * 255 - 1) then
  begin
    secondByte := 126;
    secondByte := secondByte + mask * 128;
    encoded := encoded + AnsiChar(secondByte) + packN16(payloadLength);
  end
  else
  begin
    secondByte := 127;
    secondByte := secondByte + mask * 128;
    encoded := encoded + AnsiChar(secondByte);
    encoded := encoded + packN64(payloadLength);
  end;

  key := '';
  if (mask = 1) then
  begin
    key := packN32(random(MaxInt));
    encoded := encoded + key;
  end;

  if (Length(payloadData) > 0) then
  begin
    if mask = 1 then
      encoded := encoded + rotMask(payloadData, key)
    else
      encoded := encoded + payloadData;
  end;

  Result := encoded;
end;

class function TWebSocketFrameHybie.decode(var raw: AnsiString; head: TWebSocketFrame = nil): TWebSocketFrame;
var
  tframe: TWebSocketFrame;
  this: TWebSocketFrameHybie;
  firstByte, secondByte: byte;
  len: Integer;
  currentOffset, fullLength: Integer;
  frameData: AnsiString;
  h, l: Cardinal;
begin
  if head <> nil then
  begin
    this := TWebSocketFrameHybie(head);
  end
  else
  begin
    tframe := TWebSocketFrameHybie.Create(TextFrame);
    this := TWebSocketFrameHybie(tframe);

    // Read the first two bytes, then chop them off
    firstByte := byte(raw[1]);
    secondByte := byte(raw[2]);
    raw := Copy(raw, 3, MaxInt);

    this.FIN := Integer(IsBitSet(firstByte, 7));
    this.RSV1 := Integer(IsBitSet(firstByte, 6));
    this.RSV2 := Integer(IsBitSet(firstByte, 5));
    this.RSV3 := Integer(IsBitSet(firstByte, 4));

    this.mask := Integer(IsBitSet(secondByte, 7));

    this.opcode := TWebSocketOpcode(firstByte and $F);

    len := secondByte and (not 128);

    if (len <= 125) then
      this.payloadLength := len
    else if (len = 126) then
    begin
      this.payloadLength := UnPackN16(raw);
      raw := Copy(raw, 3, MaxInt);
    end
    else if (len = 127) then
    begin
      h := UnPackN32(raw);
      raw := Copy(raw, 5, MaxInt);
      l := UnPackN32(raw);
      this.payloadLength := (l + (h * $100000000));
      raw := Copy(raw, 5, MaxInt);
    end;

    if (this.mask = 1) then
    begin
      this.maskingKey := Copy(raw, 1, 4);
      raw := Copy(raw, 5, MaxInt);
    end;
  end;

  currentOffset := this.actualLength;
  fullLength := min(this.payloadLength - this.actualLength, Length(raw));
  this.actualLength := this.actualLength + fullLength;

  if (fullLength < Length(raw)) then
  begin
    frameData := Copy(raw, 1, fullLength);
    raw := Copy(raw, fullLength + 1, MaxInt);
  end
  else
  begin
    frameData := raw;
    raw := '';
  end;

  if (this.mask = 1) then
    this.payloadData := this.payloadData + rotMask(frameData, this.maskingKey,
      currentOffset)
  else
    this.payloadData := this.payloadData + frameData;

  Result := this;
end;

function TWebSocketFrameHybie.isReady: Boolean;
begin
  if (actualLength > payloadLength) then
    raise Exception.Create('WebSocketFrameSizeMismatch')
  else
    Result := (actualLength = payloadLength);
end;

function TWebSocketFrameHybie.isFinal: Boolean;
begin
  Result := (FIN = 1);
end;

function TWebSocketFrameHybie.getData: AnsiString;
begin
  Result := payloadData;
end;

constructor TWebSocketFrameEnumerator.Create;
begin
  _pointers := TList.Create;
end;

destructor TWebSocketFrameEnumerator.Destroy;
begin
  _pointers.Free;
end;

procedure TWebSocketFrameEnumerator.Add(tframe: TWebSocketFrame);
begin
  _pointers.Add(tframe);
end;

procedure TWebSocketFrameEnumerator.Delete(index: Integer);
begin
  _pointers.Delete(index);
end;

function TWebSocketFrameEnumerator.Get(index: Integer): TWebSocketFrame;
begin
  Result := _pointers[index];
end;

function TWebSocketFrameEnumerator.Count: Integer;
begin
  Result := _pointers.Count;
end;

end.
