unit Lb.Source;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TLine = record
    Time : TDateTime;
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
    L    : Double;
    M    : Double;
    S    : Double;
  end;
  TLineList = TList<TLine>;

  TTypeSid = (
    tsUp,
    tsDown
  );

  ///<summary>
  /// Исходнные данные
  ///</summary>
  TSource = class(TObject)
  private
    FLongRSI: Double;
    FMediumRSI: Double;
    FShortSRI: Double;
    FDeltaValue: Double;
    FLines: TLineList;
    function GetTypeSid: TTypeSid;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetOpen(const AFileName: String);
    ///<summary>
    /// Глубина обследование
    ///</summary>
    procedure SetDepth(const ACount: Integer);
  public
    property LongRSI: Double read FLongRSI;
    property MediumRSI: Double read FMediumRSI;
    property ShortSRI: Double read FShortSRI;
    property DeltaValue: Double read FDeltaValue;
    property TypeSid: TTypeSid read GetTypeSid;
  end;

  ///<summary>
  /// Массив источников
  ///</summary>
  TSourceList = TObjectList<TSource>;

function GetStrToTypeSid(const ATypeSid: TTypeSid): String;
procedure SetPathFiles(const APath: String; AFiles: TStrings);
procedure SetParserCandels(ALines: TLineList; ASources: TStrings);

implementation

function GetStrToTypeSid(const ATypeSid: TTypeSid): String;
begin
  Result := '';
  case ATypeSid of
    tsUp: Result := 'Up';
    tsDown: Result := 'Down';
  end;
end;

procedure SetPathFiles(const APath: String; AFiles: TStrings);
var
  xSDA: TStringDynArray;
begin
  AFiles.Clear;
  xSDA := TDirectory.GetFiles(APath, '*.csv');
  for var xS in xSDA do
    AFiles.Add(xS);
end;

procedure SetParserCandels(ALines: TLineList; ASources: TStrings);

  function _GetStrToTime(AStrings: TStrings; AIndex: Integer): TDateTime;
  begin
    Result := 0;
    if (AIndex >= 0) and (AIndex < AStrings.Count) then
      Result := StrToTime(AStrings[AIndex]);
  end;

  function _GetStrToDouble(AStrings: TStrings; AIndex: Integer): Double;
  var
    xS: String;
    xF: TFormatSettings;
  begin
    Result := 0;
    if (AIndex >= 0) and (AIndex < AStrings.Count) then
    begin
      xS := AStrings[AIndex];

      xF := FormatSettings;
      xF.DecimalSeparator := ',';

      Result := StrToFloatDef(xS,0,xF);
    end;
  end;

  function _Parser(S: String): TLine;
  var
    xStr: TStrings;
  begin
    xStr := TStringList.Create;
    try
      xStr.Delimiter := ';';
      xStr.DelimitedText := S;
      with Result do
      begin
        Time := _GetStrToTime(xStr,0);
        Open := _GetStrToDouble(xStr,1);
        High := _GetStrToDouble(xStr,2);
        Low  := _GetStrToDouble(xStr,3);
        Close:= _GetStrToDouble(xStr,4);
        Vol  := _GetStrToDouble(xStr,5);
        L    := _GetStrToDouble(xStr,6);
        M    := _GetStrToDouble(xStr,7);
        S    := _GetStrToDouble(xStr,8);
      end;
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  xL: TLine;
begin
  ALines.Clear;
  for var xS in ASources do
  begin
    xL := _Parser(xS);
    ALines.Add(xL);
  end;
end;

{ TSource }

constructor TSource.Create;
begin
  FLines   := TLineList.Create;
end;

destructor TSource.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

procedure TSource.SetDepth(const ACount: Integer);

  function _RoundTo(const AValue: Double): Double;
  begin
    Result := Round(AValue * 1000)/1000;
  end;

var
  xLine: TLine;
  xCount: Integer;
begin
  FDeltaValue := 0;

  // Входные параметры
  if (ACount >= 0) and (ACount <= FLines.Count) then
    xCount := ACount
  else
    xCount := FLines.Count;

  xLine := FLines[xCount - 1];
  FLongRSI   := xLine.L/100;
  FMediumRSI := xLine.M/100;
  FShortSRI  := xLine.S/100;
  FDeltaValue := _RoundTo(FLines[0].Close - xLine.Close);
end;

procedure TSource.SetOpen(const AFileName: String);
var
  xSources: TStringList;
begin
  xSources := TStringList.Create;
  try
    xSources.LoadFromFile(AFileName);
    SetParserCandels(FLines,xSources);
  finally
    FreeAndNil(xSources);
  end;
end;

function TSource.GetTypeSid: TTypeSid;
begin
  if FDeltaValue > 0 then
    Result := TTypeSid.tsUp
  else
    Result := TTypeSid.tsDown;
end;

end.
