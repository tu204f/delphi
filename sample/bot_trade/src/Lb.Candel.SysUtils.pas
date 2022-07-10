unit Lb.Candel.SysUtils;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>Тип цены</summary>
  TTypePrice = (tpNon,tpOpen,tpHigh,tpLow,tpClose);

  ///<summary>Свеча</summary>
  TCandel = record
    Date: TDateTime;
    Time: TDateTime;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Vol: Double;
  public
    constructor Create(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double); overload;
    constructor CreateCandel(ACandel: TCandel); overload;
    constructor Cretae(AValue: String); overload;
    function ToString: String;
  end;
  TCandelList = TList<TCandel>;

type
  ///<summary>Поток свечей</summary>
  TCnadelStream = class(TObject)
  private
    FStream: TStream;
    FFileName: String;
    function GetEOF: Boolean;
    procedure SetFileName(const Value: String);
  protected
    procedure CheсkStream;
    function GetLine: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function First: TCandel;
    function Next: TCandel;
    property EOF: Boolean read GetEOF;
    property FileName: String read FFileName write SetFileName;
  end;

/// <summary>С равниваем два числа, с точностью</summary>
function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;

implementation

uses
  System.Math;

function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;
var
  xEpsilon: Double;
  xLowValue, xHighValue: Double;
begin
  Result := False;
  if (AEpsilon > 0)  and (AEpsilon < 100) then
  begin
    xLowValue := Min(AValue1,AValue2);
    xHighValue := Max(AValue1,AValue2);
    xEpsilon := (xHighValue - xLowValue)/xHighValue;
    Result := AEpsilon > (xEpsilon * 100);
  end;
end;

function GetToDate(S: String): TDateTime;
begin
  Result := 0;
  if not S.IsEmpty then
  begin
    var xS := Copy(S,7,2) + '.' + Copy(S,5,2) + '.' + Copy(S,1,4);
    Result :=  StrToDateDef(xS,0);
  end;
end;


function GetToTime(S: String): TDateTime;
begin
  Result := 0;
  if not S.IsEmpty then
  begin
    var xS := '';
    if Pos(':',S) = 0 then
      xS := Copy(S,1,2) + ':' + Copy(S,3,2) + ':' + Copy(S,5,2)
    else
      xS := S;
    Result := StrToTimeDef(xS,0);
  end;
end;

function GetStrToFloat(S: String): Double;
begin
  Result := 0;
  if not S.IsEmpty then
  begin
    var xIndex := Pos('.',S);
    if xIndex > 1 then
      S[xIndex] := ',';
    Result := StrToFloatDef(S,0);
  end;
end;

// <DATE>;<TIME>;<OPEN>;<HIGH>;<LOW>;<CLOSE>;<VOL>
// 20210628;100500;277.2300000;277.3700000;276.9000000;277.1800000;672190
function GetParserCandel(S: String): TCandel;
var
  xR: TCandel;
begin
  var xStr := TStringList.Create;
  try
    xStr.Delimiter := ';';
    xStr.DelimitedText := S;
    if xStr.Count >= 7 then
    begin
      xR.Date := GetToDate(xStr[0]);
      xR.Time := GetToTime(xStr[1]);
      xR.Open := GetStrToFloat(xStr[2]);
      xR.High := GetStrToFloat(xStr[3]);
      xR.Low  := GetStrToFloat(xStr[4]);
      xR.Close:= GetStrToFloat(xStr[5]);
      xR.Vol  := StrToInt64Def(xStr[6],0);
    end;
  finally
    FreeAndNil(xStr);
  end;
  Result := xR;
end;

{ TCandel }

constructor TCandel.Create(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double);
begin
  with Self do
  begin
    Date := ADate;
    Time := ATime;
    Open := AOpen;
    High := AHigh;
    Low := ALow;
    Close := AClose;
    Vol := AVol;
  end;
end;

constructor TCandel.CreateCandel(ACandel: TCandel);
begin
  Self.Create(ACandel.Date,ACandel.Time,ACandel.Open,ACandel.High,ACandel.Low,ACandel.Close,ACandel.Close);
end;

constructor TCandel.Cretae(AValue: String);
var
  xCandel: TCandel;
begin
  xCandel := GetParserCandel(AValue);
  Self.CreateCandel(xCandel);
end;

function TCandel.ToString: String;
var
  xS: String;
begin
  xS := 'D: ' + DateToStr(Self.Date) + '; ' +
        'T: ' + TimeToStr(Self.Time) + '; ' +
        'O: ' + FloatToStr(Self.Open) + '; ' +
        'H: ' + FloatToStr(Self.High) + '; ' +
        'L: ' + FloatToStr(Self.Low) + '; ' +
        'C: ' + FloatToStr(Self.Close) + '; ' +
        'V: ' + FloatToStr(Self.Vol);
  Result := xS;
end;

{ TCnadelStream }

constructor TCnadelStream.Create;
begin
  FStream := nil;
end;

destructor TCnadelStream.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TCnadelStream.CheсkStream;
begin
  if not Assigned(FStream) then
    raise Exception.Create('Error Message: Stream не определен');
end;

function TCnadelStream.GetLine: String;
const
  SIZE_BUFFER = 256;
var
  xLengthByte: Integer;
  xOldPosition: Int64;
  xBytes: TBytes;
  xByte: Byte;
  xS: String;
begin
  xOldPosition := FStream.Position;
  SetLength(xBytes,SIZE_BUFFER);
  FStream.Read(xBytes,SIZE_BUFFER);
  xLengthByte := 0;
  for xByte in xBytes do
  begin
    if (xByte = 13) then
    begin
      FStream.Position := xOldPosition + xLengthByte + 2;
      Break;
    end
    else
    begin
      Inc(xLengthByte);
      xS := xS + Chr(xByte);
    end;
  end;
  Result := xS;
end;

function TCnadelStream.First: TCandel;
begin
  CheсkStream;
  FStream.Position := 0;
  Result := TCandel.Cretae(GetLine);
end;

function TCnadelStream.Next: TCandel;
begin
  CheсkStream;
  Result := TCandel.Cretae(GetLine);
end;

function TCnadelStream.GetEOF: Boolean;
begin
  CheсkStream;
  Result := FStream.Position >= FStream.Size;
end;

procedure TCnadelStream.SetFileName(const Value: String);
begin
  FFileName := Value;
  if Assigned(FStream) then
    FreeAndNil(FStream);
  FStream := TFileStream.Create(FFileName,fmOpenRead);
end;

end.
