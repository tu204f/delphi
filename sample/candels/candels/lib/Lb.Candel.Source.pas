unit Lb.Candel.Source;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Candel.SysUtils;

type
  ///<summary>
  /// Источние биржевых свечей
  ///</summary>
  TSourceCandel = class(TObject)
  private
    FSource: TCandelList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetLoadFile(const AFileName: String);
    procedure SetParserCandels(const ASource: TStrings);
    property Candels: TCandelList read FSource;
  public {Определение предела, Максимальной и минимальной цены или объема}
    procedure GetMaxAndMinPriceVol(out AMaxPrice, AMinPrice: Double; out AMaxVol, AMinVol: Double);
    procedure GetMaxAndMinValue(out AMaxValue, AMinValue: Double); overload;
    procedure GetMaxAndMinValue(const ACount: Integer; out AMaxValue, AMinValue: Double); overload;
    procedure GetMaxAndMinValue(const ABeginIndex, ACount: Integer; out AMaxValue, AMinValue: Double); overload;
  public
    procedure SetSelected(ACountSource, ACountVectory: Integer; ASources: TSourceCandel);
  end;

implementation

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
    var xS := Copy(S,1,2) + ':' + Copy(S,3,2) + ':' + Copy(S,5,2);
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
      xR.Status := -1;
    end;
  finally
    FreeAndNil(xStr);
  end;
  Result := xR;
end;

{ TSourceCandel }

constructor TSourceCandel.Create;
begin
  FSource := TCandelList.Create;
end;

destructor TSourceCandel.Destroy;
begin
  FreeAndNil(FSource);
  inherited;
end;

procedure TSourceCandel.GetMaxAndMinPriceVol(out AMaxPrice, AMinPrice: Double;
  out AMaxVol, AMinVol: Double);
var
  xCandel: TCandel;
  i, iCount: Integer;
begin
  iCount := FSource.Count;
  if iCount > 0 then
  begin
    xCandel := FSource[0];
    AMaxPrice := xCandel.High;
    AMinPrice := xCandel.Low;
    AMaxVol   := xCandel.Vol;
    AMinVol   := xCandel.Vol;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FSource[i];
      if AMaxPrice < xCandel.High then
        AMaxPrice := xCandel.High;
      if AMinPrice > xCandel.Low then
        AMinPrice := xCandel.Low;
      if AMaxVol < xCandel.Vol then
        AMaxVol := xCandel.Vol;
      if AMaxVol > xCandel.Vol then
        AMaxVol := xCandel.Vol;
    end;
  end;
end;

procedure TSourceCandel.GetMaxAndMinValue(const ABeginIndex, ACount: Integer; out AMaxValue, AMinValue: Double);
var
  xCandel: TCandel;
  i, xLow, xHigh: Integer;
begin
  AMaxValue := 0;
  AMinValue := 0;

  if FSource.Count > 0 then
  begin

    if ABeginIndex >= 0 then
    begin
      xLow := ABeginIndex;
      xHigh := xLow + ACount;
      if xHigh >= FSource.Count then
        xHigh := FSource.Count - 1;
    end
    else
    begin
      if ACount > 0 then
      begin
        xLow := FSource.Count - ACount;
        xHigh := FSource.Count - 1;
      end
      else
      begin
        xLow := 0;
        xHigh := FSource.Count - 1;
      end;
    end;

    xCandel := FSource[xLow];
    AMaxValue := xCandel.High;
    AMinValue := xCandel.Low;

    for i := (xLow + 1) to xHigh do
    begin
      xCandel := FSource[i];
      if AMaxValue < xCandel.High then AMaxValue := xCandel.High;
      if AMinValue > xCandel.Low  then AMinValue := xCandel.Low;
    end;

  end;
end;

procedure TSourceCandel.GetMaxAndMinValue(const ACount: Integer; out AMaxValue, AMinValue: Double);
begin
  Self.GetMaxAndMinValue(-1,ACount,AMaxValue, AMinValue);
end;

procedure TSourceCandel.GetMaxAndMinValue(out AMaxValue, AMinValue: Double);
begin
  Self.GetMaxAndMinValue(-1,0,AMaxValue,AMinValue);
end;

procedure TSourceCandel.SetLoadFile(const AFileName: String);
var
  xStr: TStrings;
begin
  if FileExists(AFileName) then
    xStr := TStringList.Create;
    try
      xStr.Clear;
      xStr.LoadFromFile(AFileName);
      Self.SetParserCandels(xStr);
    finally
      FreeAndNil(xStr);
    end;
end;

procedure TSourceCandel.SetParserCandels(const ASource: TStrings);
var
  xCandel: TCandel;
begin
  FSource.Clear;
  if Assigned(ASource) then
    for var S in ASource do
    begin
      if not S.IsEmpty then
      begin
        if S.Chars[0] = '<' then
          Continue;
        xCandel := GetParserCandel(S);

        FSource.Add(xCandel);
      end
    end;
end;

procedure TSourceCandel.SetSelected(ACountSource, ACountVectory: Integer; ASources: TSourceCandel);
var
  i, iCount: Integer;
  xCountSource, xCountVectory: Integer;
begin
  if ACountSource <= 0 then
    raise Exception.Create('Error Message: ACountSource: ' + IntToStr(ACountSource));

  if ACountVectory <= 0 then
    raise Exception.Create('Error Message: ACountVectory: ' + IntToStr(ACountVectory));

  xCountSource := 0;
  xCountVectory := 0;

  iCount := ACountSource + ACountVectory;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      if xCountSource < ACountSource then
        Inc(xCountSource)
      else
        Inc(xCountVectory);



    end;
end;

end.
