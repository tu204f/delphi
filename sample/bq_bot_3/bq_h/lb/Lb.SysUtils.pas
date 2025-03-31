(******************************************************************************)
(*  *)
(******************************************************************************)
unit Lb.SysUtils;

interface

{
    xS := AAsk.ToString + ';' + ABid.ToString + ';' + ADeviationValue.ToString + ';';
    xS := xS + ACandel.Time.ToString + ';';
    xS := xS + DateTimeToStr(UnixToDateTime(ACandel.Time)) + ';';
    xS := xS + ACandel.Open.ToString + ';';
    xS := xS + ACandel.High.ToString + ';';
    xS := xS + ACandel.Low.ToString + ';';
    xS := xS + ACandel.Close.ToString + ';';
    xS := xS + ACandel.Vol.ToString + ';';
    xS := xS + 'endl';
}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  ///<summary>Состояние сделки</summary>
  TTypeTrade = (
    ttNull,
    ttOpen,
    ttClose,
    ttCloseUser,
    ttCloseSL,
    ttCloseRateSL,
    ttCloseRateTK
  );

  ///<summary>Напровление сделки</summary>
  TTypeDirection = (
    tdNull,
    tdLong,
    tdShort
  );

  TTypeBuySell = (
    tsNull, {добавлено новое значение}
    tsBuy,
    tsSell
  );


type
  TCandel = record
    TimeValue: Int64;   //
    Time: TDateTime;    //
    Open: Double;       // Открытие
    High: Double;       // Максимум
    Low: Double;        // Минимум
    Close: Double;      // Закрытие
    Vol: Double;        // Объем
  end;

  TSourceLine = record
    Ask: Double;        // Цена продавца
    Bid: Double;        // Цена покупки
    Deviation: Double;  // Значение откланение
    Candel: TCandel;    // Значение свечей
  end;

type
  TOnEventSourceStreame = procedure(Sender: TObject; ASourceLine: TSourceLine) of object;

  TSourceStreame = class(TObject)
  private
    FStream: TStream;
    FOnEventSourceStreame: TOnEventSourceStreame;
    function GetEOF: Boolean;
  protected
    procedure DoEventSourceStreame(ASourceLine: TSourceLine);
    procedure SetLineRead(const ASources: TStrings);
    function GetLine(const ASources: TStrings): TSourceLine;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open(const AFileName: String);
    procedure First;
    procedure Next;
    property EOF: Boolean read GetEOF;
    property OnEventSourceStreame: TOnEventSourceStreame write FOnEventSourceStreame;
  end;

function GetCrossSide(ASide: TTypeBuySell): TTypeBuySell;
function GetStrToSide(ASide: TTypeBuySell): String;
function GetSiseToStr(AValue: String): TTypeBuySell;
function GetStrToTypeTrade(const ATypeTrade: TTypeTrade): String;
function GetTypeTradeToStr(const AValue: String): TTypeTrade;

function GetNewDateTime: TDateTime;
function GetRound(const AValue: Double): Double; inline;

implementation

function GetStrToTypeTrade(const ATypeTrade: TTypeTrade): String;
begin
  case ATypeTrade of
    ttNull: Result := 'null';
    ttOpen: Result := 'open';
    ttClose: Result := 'close';
    ttCloseUser: Result := 'close_user';
    ttCloseSL: Result := 'close_sl';
    ttCloseRateSL: Result := 'close_rate_sl';
    ttCloseRateTK: Result := 'close_rate_tk';
  else
    Result := 'not_type_trade';
  end;
end;

function GetTypeTradeToStr(const AValue: String): TTypeTrade;
var
  xC: Char;
begin
  if AValue.IsEmpty then
  begin
    Result := TTypeTrade.ttNull;
    Exit;
  end;
  xC := AValue[1];
  case xC of
    'o': Result := TTypeTrade.ttOpen;
    'c': Result := TTypeTrade.ttClose;
  else
    Result := TTypeTrade.ttNull;
  end;
end;

function GetStrToSide(ASide: TTypeBuySell): String;
begin
  case ASide of
    tsNull: Result := 'N';
    tsBuy: Result := 'B';
    tsSell: Result := 'S';
  end;
end;

function GetSiseToStr(AValue: String): TTypeBuySell;
var
  xC: Char;
begin
  xC := #0;
  if AValue.Length > 0 then
    xC := AValue[1];
  case xC of
    'B': Result := TTypeBuySell.tsBuy;
    'S': Result := TTypeBuySell.tsSell;
  else
    Result :=  TTypeBuySell.tsNull;
  end;
end;

function GetCrossSide(ASide: TTypeBuySell): TTypeBuySell;
begin
  case ASide of
    tsBuy: Result := TTypeBuySell.tsSell;
    tsSell: Result := TTypeBuySell.tsBuy;
  else
    Result := ASide;
  end;
end;

function GetNewDateTime: TDateTime;
begin
  Result := System.SysUtils.Date + System.SysUtils.Time;
end;


function GetRound(const AValue: Double): Double;
begin
  Result := Trunc(AValue * 1000)/1000;
end;

{ TSourceStreame }

constructor TSourceStreame.Create;
begin
  FStream := nil;
end;

destructor TSourceStreame.Destroy;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
  inherited;
end;


procedure TSourceStreame.Open(const AFileName: String);
begin
  FStream := TFileStream.Create(AFileName,fmOpenRead);
end;

procedure TSourceStreame.SetLineRead(const ASources: TStrings);
var
  xB: Byte;
  xC: Char;
  tmpS: String;
  xMaxCount: Integer;
begin
  if not Assigned(ASources) then
    raise Exception.Create('Error Message: Объект не определен');
  ASources.Clear;

  tmpS := '';
  xMaxCount := 256;
  while True do
  begin
    FStream.Read(xB,1);

    xC := Chr(xB);
    if xC = ';' then
    begin
      ASources.Add(tmpS);
      tmpS := '';
    end
    else
      tmpS := tmpS + xC;

    if (xB = 10) then
    begin
      ASources.Add(tmpS);
      Break;
    end;
    if xMaxCount <= 0 then
      Break;

    xMaxCount := xMaxCount - 1;
  end;
end;

function TSourceStreame.GetLine(const ASources: TStrings): TSourceLine;

  function _Parser10(const ASources: TStrings): TSourceLine;
  var
    xLine: TSourceLine;
  begin
    try
      xLine.Ask       := StrToFloatDef(ASources[0],0);
      xLine.Bid       := StrToFloatDef(ASources[1],0);
      xLine.Deviation := 0;
      with xLine do
      begin
        Candel.TimeValue := StrToInt64Def(ASources[2],0);
        Candel.Time      := StrToDateTimeDef(ASources[3],0);
        Candel.Open      := StrToFloatDef(ASources[4],0);
        Candel.High      := StrToFloatDef(ASources[5],0);
        Candel.Low       := StrToFloatDef(ASources[6],0);
        Candel.Close     := StrToFloatDef(ASources[7],0);
        Candel.Vol       := StrToFloatDef(ASources[8],0);
      end;
      Result := xLine;
    except
      raise Exception.Create('Error Message: Парсинг');
    end;
  end;

  function _Parser11(const ASources: TStrings): TSourceLine;
  var
    xLine: TSourceLine;
  begin
    try
      xLine.Ask       := StrToFloatDef(ASources[0],0);
      xLine.Bid       := StrToFloatDef(ASources[1],0);
      xLine.Deviation := StrToFloatDef(ASources[2],0);
      with xLine do
      begin
        Candel.TimeValue := StrToInt64Def(ASources[3],0);
        Candel.Time      := StrToDateTimeDef(ASources[4],0);
        Candel.Open      := StrToFloatDef(ASources[5],0);
        Candel.High      := StrToFloatDef(ASources[6],0);
        Candel.Low       := StrToFloatDef(ASources[7],0);
        Candel.Close     := StrToFloatDef(ASources[8],0);
        Candel.Vol       := StrToFloatDef(ASources[9],0);
      end;
      Result := xLine;
    except
      raise Exception.Create('Error Message: Парсинг');
    end;
  end;

begin
  if ASources.Count = 10 then
    Result := _Parser10(ASources)
  else
    Result := _Parser11(ASources);
end;

function TSourceStreame.GetEOF: Boolean;
begin
  if Assigned(FStream) then
    Result := FStream.Position >= FStream.Size
  else
    Result := False;
end;

procedure TSourceStreame.First;
var
  xStr: TStrings;
  xLine: TSourceLine;
begin
  FStream.Position := 0;
  xStr := TStringList.Create;
  try
    SetLineRead(xStr);
    if xStr.Count > 0 then
    begin
      xLine := GetLine(xStr);
      DoEventSourceStreame(xLine);
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TSourceStreame.Next;
var
  xStr: TStrings;
  xLine: TSourceLine;
begin
  xStr := TStringList.Create;
  try
    SetLineRead(xStr);
    if xStr.Count > 0 then
    begin
      xLine := GetLine(xStr);
      DoEventSourceStreame(xLine);
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TSourceStreame.DoEventSourceStreame(ASourceLine: TSourceLine);
begin
  if Assigned(FOnEventSourceStreame) then
    FOnEventSourceStreame(Self,ASourceLine);
end;


end.
