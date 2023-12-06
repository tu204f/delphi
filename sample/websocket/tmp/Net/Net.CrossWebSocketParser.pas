﻿{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Net.CrossWebSocketParser;

{$I zLib.inc}

{
  The WebSocket Protocol
  https://tools.ietf.org/html/rfc6455

  0                   1                   2                   3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
  +-+-+-+-+-------+-+-------------+-------------------------------+
  |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
  |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
  |N|V|V|V|       |S|             |   (if payload len==126/127)   |
  | |1|2|3|       |K|             |                               |
  +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
  |     Extended payload length continued, if payload len == 127  |
  + - - - - - - - - - - - - - - - +-------------------------------+
  |                               |Masking-key, if MASK set to 1  |
  +-------------------------------+-------------------------------+
  | Masking-key (continued)       |          Payload Data         |
  +-------------------------------- - - - - - - - - - - - - - - - +
  :                     Payload Data continued ...                :
  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  |                     Payload Data continued ...                |
  +---------------------------------------------------------------+
  opcode:
       *  %x0 denotes a continuation frame
       *  %x1 denotes a text frame
       *  %x2 denotes a binary frame
       *  %x3-7 are reserved for further non-control frames
       *  %x8 denotes a connection close
       *  %x9 denotes a ping
       *  %xA denotes a pong
       *  %xB-F are reserved for further control frames
  Payload length:  7 bits, 7+16 bits, or 7+64 bits
  Masking-key:  0 or 4 bytes
}

interface

uses
  SysUtils,
  Classes,

  Utils.Hash,
  Utils.Base64,
  Utils.Utils;

const
  WS_OP_CONTINUATION = $00;
  WS_OP_TEXT         = $01;
  WS_OP_BINARY       = $02;
  WS_OP_CLOSE        = $08;
  WS_OP_PING         = $09;
  WS_OP_PONG         = $0A;

  WS_MAGIC_STR       = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

type
  TWsMessageType = (wtUnknown, wtText, wtBinary);
  TWsFrameParseState = (wsHeader, wsBody, wsDone);

  TWsOnCommand = reference to procedure(const AOpCode: Byte; const AData: TBytes);
  TWsOnMessage = reference to procedure(const AType: TWsMessageType; const AData: TBytes);

  TWebSocketParser = class
  private
    FWsFrameState: TWsFrameParseState;
    FWsFrameHeader, FWsMessageBody: TBytesStream;
    FWsFIN: Boolean;
    FWsOpCode: Byte;
    FWsMask: Boolean;
    FWsMaskKey: Cardinal;
    FWsMaskKeyShift: Integer;
    FWsPayload: Byte;
    FWsHeaderSize: Byte;
    FWsBodySize: UInt64;

    FOnCommand: TWsOnCommand;
    FOnMessage: TWsOnMessage;

    procedure _ResetFrameHeader;
    procedure _ResetRequest;

  protected
    procedure TriggerCommand(const AOpCode: Byte; const AData: TBytes);
    procedure TriggerMessage(const AType: TWsMessageType; const AData: TBytes);
  public
    constructor Create(const AOnCommand: TWsOnCommand; const AOnMessage: TWsOnMessage);
    destructor Destroy; override;

    procedure Decode(ABuf: Pointer; ALen: Integer);

    class function OpCodeToReqType(AOpCode: Byte): TWsMessageType; static;
    class function MakeFrameData(AOpCode: Byte; AFin: Boolean; AMaskKey: Cardinal; AData: Pointer; ADataSize: UInt64): TBytes; static;

    class function NewSecWebSocketKey: string; static;
    class function MakeSecWebSocketAccept(const ASecWebSocketKey: string): string; static;
  end;

implementation

{ TWebSocketParser }

constructor TWebSocketParser.Create(const AOnCommand: TWsOnCommand;
  const AOnMessage: TWsOnMessage);
begin
  FOnCommand := AOnCommand;
  FOnMessage := AOnMessage;

  FWsFrameHeader := TBytesStream.Create(nil);
  FWsMessageBody := TBytesStream.Create(nil);
  _ResetRequest;
end;

destructor TWebSocketParser.Destroy;
begin
  FreeAndNil(FWsFrameHeader);
  FreeAndNil(FWsMessageBody);

  inherited;
end;

procedure TWebSocketParser.Decode(ABuf: Pointer; ALen: Integer);
var
  PBuf: PByte;
  LByte: Byte;
  LMessageData: TBytes;
begin
  PBuf := ABuf;
  while (ALen > 0) do
  begin
    // 使用循环处理粘包, 比递归调用节省资源
    while (ALen > 0) and (FWsFrameState <> wsDone) do
    begin
      case FWsFrameState of
        wsHeader:
          begin
            FWsFrameHeader.Write(PBuf^, 1);
            Dec(ALen);
            Inc(PBuf);

            if (FWsFrameHeader.Size = 2) then
            begin
              // 第1个字节最高位为 FIN 状态
              FWsFIN := (FWsFrameHeader.Bytes[0] and $80 <> 0);

              // 第1个字节低4位为 opcode 状态
              LByte := FWsFrameHeader.Bytes[0] and $0F;
              if (LByte <> WS_OP_CONTINUATION) then
                FWsOpCode := LByte;

              // 第2个字节最高位为 MASK 状态
              FWsMask := (FWsFrameHeader.Bytes[1] and $80 <> 0);

              // 第2个字节低7位为 payload len
              FWsPayload := FWsFrameHeader.Bytes[1] and $7F;

              FWsHeaderSize := 2;
              if (FWsPayload < 126) then
                FWsBodySize := FWsPayload
              else if (FWsPayload = 126) then
                Inc(FWsHeaderSize, 2)
              else if (FWsPayload = 127) then
                Inc(FWsHeaderSize, 8);
              if FWsMask then
                Inc(FWsHeaderSize, 4);
            end;

            if (FWsFrameHeader.Size = FWsHeaderSize) then
            begin
              FWsFrameState := wsBody;

              // 保存 mask key
              if FWsMask then
                Move(PCardinal(UIntPtr(FWsFrameHeader.Memory) + FWsHeaderSize - 4)^, FWsMaskKey, 4);

              if (FWsPayload = 126) then
                FWsBodySize := FWsFrameHeader.Bytes[3]
                  + Word(FWsFrameHeader.Bytes[2]) shl 8
              else if (FWsPayload = 127) then
                FWsBodySize := FWsFrameHeader.Bytes[9]
                  + UInt64(FWsFrameHeader.Bytes[8]) shl 8
                  + UInt64(FWsFrameHeader.Bytes[7]) shl 16
                  + UInt64(FWsFrameHeader.Bytes[6]) shl 24
                  + UInt64(FWsFrameHeader.Bytes[5]) shl 32
                  + UInt64(FWsFrameHeader.Bytes[4]) shl 40
                  + UInt64(FWsFrameHeader.Bytes[3]) shl 48
                  + UInt64(FWsFrameHeader.Bytes[2]) shl 56
                  ;

              // 接收完一帧
              if (FWsBodySize <= 0) then
              begin
                // 如果这是一个独立帧或者连续帧的最后一帧
                // 则表示一次请求数据接收完成
                if FWsFIN then
                begin
                  FWsFrameState := wsDone;
                  Break;
                // 否则继续接收下一帧
                end else
                  _ResetFrameHeader;
              end;
            end;
          end;

        wsBody:
          begin
            LByte := PBuf^;
            // 如果 MASK 状态为 1, 则将收到的数据与 mask key 做异或处理
            if FWsMask then
            begin
              LByte := LByte xor PByte(@FWsMaskKey)[FWsMaskKeyShift];
              FWsMaskKeyShift := (FWsMaskKeyShift + 1) mod 4;
            end;
            FWsMessageBody.Write(LByte, 1);
            Dec(ALen);
            Inc(PBuf);
            Dec(FWsBodySize);

            // 接收完一帧
            if (FWsBodySize <= 0) then
            begin
              // 如果这是一个独立帧或者连续帧的最后一帧
              // 则表示一次请求数据接收完成
              if FWsFIN then
              begin
                FWsFrameState := wsDone;
                Break;
              // 否则继续接收下一帧
              end else
                _ResetFrameHeader;
            end;
          end;
      end;
    end;

    // 一个完整的 WebSocket 数据帧接收完毕
    if (FWsFrameState = wsDone) then
    begin
      LMessageData := FWsMessageBody.Bytes;
      SetLength(LMessageData, FWsMessageBody.Size);
      _ResetRequest;

      case FWsOpCode of
        WS_OP_TEXT, WS_OP_BINARY:
          TriggerMessage(OpCodeToReqType(FWsOpCode), LMessageData);
      else
        TriggerCommand(FWsOpCode, LMessageData);
      end;
    end;
  end;
end;

class function TWebSocketParser.MakeFrameData(AOpCode: Byte; AFin: Boolean;
  AMaskKey: Cardinal; AData: Pointer; ADataSize: UInt64): TBytes;
var
  LPayload: Byte;
  LHeaderSize, LDataSize: Integer;
begin
  if (AData <> nil) and (ADataSize > 0) then
    LDataSize := ADataSize
  else
    LDataSize := 0;

  LHeaderSize := 2;
  if (ADataSize < 126) then
    LPayload := ADataSize
  else if (ADataSize <= $FFFF) then
  begin
    LPayload := 126;
    Inc(LHeaderSize, 2);
  end else
  begin
    LPayload := 127;
    Inc(LHeaderSize, 8);
  end;
  if (AMaskKey <> 0) then
    Inc(LHeaderSize, 4);

  SetLength(Result, LHeaderSize + LDataSize);
  FillChar(Result[0], LHeaderSize + LDataSize, 0);

  if AFin then
    Result[0] := Result[0] or $80;
  Result[0] := Result[0] or (AOpCode and $0F);

  if (AMaskKey <> 0) then
    Result[1] := Result[1] or $80;
  Result[1] := Result[1] or (LPayload and $7F);

  if (LPayload = 126) then
  begin
    Result[2] := PByte(@ADataSize)[1];
    Result[3] := PByte(@ADataSize)[0];
  end else
  if (LPayload = 127) then
  begin
    Result[2] := PByte(@ADataSize)[7];
    Result[3] := PByte(@ADataSize)[6];
    Result[4] := PByte(@ADataSize)[5];
    Result[5] := PByte(@ADataSize)[4];
    Result[6] := PByte(@ADataSize)[3];
    Result[7] := PByte(@ADataSize)[2];
    Result[8] := PByte(@ADataSize)[1];
    Result[9] := PByte(@ADataSize)[0];
  end;

  if (AMaskKey <> 0) then
    Move(AMaskKey, Result[LHeaderSize - 4], 4);

  if (LDataSize > 0) then
    Move(AData^, Result[LHeaderSize], LDataSize);
end;

class function TWebSocketParser.MakeSecWebSocketAccept(
  const ASecWebSocketKey: string): string;
begin
  Result := TBase64Utils.Encode(
    THashSHA1.GetHashBytes(ASecWebSocketKey + WS_MAGIC_STR)
  );
end;

class function TWebSocketParser.NewSecWebSocketKey: string;
var
  LRand: Int64;
begin
  Randomize;
  LRand := Trunc(High(Int64) * Random());
  Result := TBase64Utils.Encode(TUtils.BinToHex(@LRand, SizeOf(Int64)));
end;

class function TWebSocketParser.OpCodeToReqType(AOpCode: Byte): TWsMessageType;
begin
  case AOpCode of
    WS_OP_TEXT: Exit(wtText);
    WS_OP_BINARY: Exit(wtBinary);
  else
    Exit(wtUnknown);
  end;
end;

procedure TWebSocketParser.TriggerCommand(const AOpCode: Byte;
  const AData: TBytes);
begin
  if Assigned(FOnCommand) then
    FOnCommand(AOpCode, AData);
end;

procedure TWebSocketParser.TriggerMessage(const AType: TWsMessageType;
  const AData: TBytes);
begin
  if Assigned(FOnMessage) then
    FOnMessage(AType, AData);
end;

procedure TWebSocketParser._ResetFrameHeader;
begin
  FWsFrameState := wsHeader;
  FWsFrameHeader.Clear;
  FWsMaskKeyShift := 0;
end;

procedure TWebSocketParser._ResetRequest;
begin
  FWsFrameState := wsHeader;
  FWsFrameHeader.Clear;
  FWsMessageBody.Clear;
  FWsMaskKeyShift := 0;
end;

end.
