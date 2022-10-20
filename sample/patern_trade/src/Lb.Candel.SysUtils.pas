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
  /// Определям тип свячи
  TTypeCandel = (tcSource,tcFuture);

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
    // Допольнительный параметр, свячи
    Status: TTypeCandel;
  private
    function GetDateTime: TDateTime;
  public
    constructor Create(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double); overload;
    constructor CreateCandel(ACandel: TCandel); overload;
    constructor Cretae(AValue: String); overload;
    function ToString: String;
    property DateTime: TDateTime read GetDateTime;
  end;
  TCandelList = TList<TCandel>;

type
  TStructure = class;
  TVectorStructure = class;
  TValueStructure = class;

  ///<summary>Тип данных</summary>
  TTypeValue = (
    tvSource, // источние
    tvFuture  // ожидание
  );

  ///<summary>Тип структуры</summary>
  ///<remarks>
  /// <para>tsValue  - в ценновом значение</para>
  /// <para>tsVector - в вектором значение</para>
  ///</remarks>
  TTypeStructure = (
    tsNull,
    tsValue,
    tsVector
  );

  ///<summary>Помещаем все массив свечей в память в формате - csv</summary>
  TMemoryCandels = class(TObject)
  private
    FStrings: TStringList;
    FFileName: String;
    procedure SetFileName(const Value: String);
    function GetCount: Integer;
    function GetCandels(Index: Integer): TCandel;
  protected
    procedure CheсkStream;
    function GetLine(const AIndex: Integer): String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Candels[Index: Integer]: TCandel read GetCandels;
    property Count: Integer read GetCount;
    property FileName: String read FFileName write SetFileName;
  end;

  ///<summary>По блочно читаем данные из памяти (точнее по структурно)</summary>
  TMemoryStructures = class(TMemoryCandels)
  public const
    SOURCE_COUNT = 100;    // Примерный размер блока - опорного массива
    FUTURE_COUNT = 5;      // Примерный размер блока - массива ожиданий
  private
    FIndexCandel: Integer; // Номер свячи
    FSourceCount: Integer;
    FFutureCount: Integer;
    FValueStructure: TValueStructure;
    function GetEOF: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FirstStructure;
    procedure NextStructure;
    property EOF: Boolean read GetEOF;
    property IndexCandel: Integer read FIndexCandel;
    property Structure: TValueStructure read FValueStructure;
    property SourceCount: Integer read FSourceCount write FSourceCount;
    property FutureCount: Integer read FFutureCount write FFutureCount;
  end;

  ///<summary>Структура данных</summary>
  TStructure = class(TObject)
  private
    FStatus: TTypeStructure;
    FSourceVectors: TCandelList;
    FFutureVectors: TCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    ///<summary>Опорный вектор</summary>
    property SourceVectors: TCandelList read FSourceVectors;
    ///<summary>Прогнозный вектор</summary>
    property FutureVectors: TCandelList read FFutureVectors;
    ///<summary>Состояние объекта</summary>
    property Status: TTypeStructure read FStatus;
  end;

  ///<summary>Список структура</summary>
  TStructureList = TObjectList<TStructure>;


  ///<summary>Векторная форма</summary>
  TVectorStructure = class(TStructure)
  public
    constructor Create; override;
    ///<summary>При образование данных из значений</summary>
    procedure Transform(const AValueStructure: TValueStructure);
  end;

  ///<summary>Форма со значениями цен</summary>
  TValueStructure = class(TStructure)
  private
    FSourceRowID: Integer;
  public
    constructor Create; override;
    ///<summary>При образование данных из вектора с опорной ценнй</summary>
    procedure Transform(const AVectorStructure: TVectorStructure; const APrice, AVol: Double);
    property SourceRowID: Integer read FSourceRowID write FSourceRowID;
  end;

/// <summary>С равниваем два числа, с точностью</summary>
function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;

/// <summary>Получаем последний значение цены и объема</summary>
procedure SetLastPriceAndVol(const ACandels: TCandelList; var APrice, AVol: Double);

/// <summary>Конвертируем ценовой вектор в числовой</summary>
procedure SetTransformCandels(const AInCandels, AOutCandels: TCandelList; const APrice, AVol: Double);

/// <summary>Копирование свечай</summary>
procedure MoveCandels(const ASource, ADest: TCandelList);

/// <summary>Сократить значение работы</summary>
procedure CutDownCandels(const ASource: TCandelList; ACount: Integer);

implementation

uses
  System.Math;

(******************************************************************************)
(* Математека - работы с вектором                                             *)

(******************************************************************************)

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
    //xStr.Delimiter := ';';
    //xStr.DelimitedText := S;

    var xS := '';
    var i := 1;
    while i <= S.Length do
    begin
      var xC := S[i];
      if xC = ';' then
      begin
        xStr.Add(xS);
        xS := '';
      end
      else
        xS := xS + xC;
      if xStr.Count = 7 then
        Break;
      Inc(i);
    end;
    if not xS.IsEmpty then
      xStr.Add(xS);



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

(* ************************************************************************** *)
(* Для транформации векторного массива *)

procedure SetLastPriceAndVol(const ACandels: TCandelList; var APrice, AVol: Double);
var
  Count: Integer;
begin
  Count  := ACandels.Count;
  APrice := ACandels[Count - 1].Close;
  AVol   := ACandels[Count - 1].Vol;
end;

procedure SetTransformCandels(const AInCandels, AOutCandels: TCandelList; const APrice, AVol: Double);
var
  xInCandel: TCandel;
  xOutCandel: TCandel;
begin
  AOutCandels.Clear;
  for xInCandel in AInCandels do
  begin
    FillChar(xOutCandel,Sizeof(TCandel),0);
    xOutCandel.Open  := xInCandel.Open / APrice;
    xOutCandel.High  := xInCandel.High / APrice;
    xOutCandel.Low   := xInCandel.Low  / APrice;
    xOutCandel.Close := xInCandel.Close/ APrice;
    xOutCandel.Vol   := xInCandel.Vol  / AVol;
    AOutCandels.Add(xOutCandel);
  end;
end;


(* ************************************************************************** *)

procedure MoveCandels(const ASource, ADest: TCandelList);
begin
  // Копировать
  ADest.Clear;
  for var xC in ASource do
    ADest.Add(xC);
end;

procedure CutDownCandels(const ASource: TCandelList; ACount: Integer);
begin
  // Удалить лишние значение, сократить до нужного размера
  while ASource.Count > ACount do
    ASource.Delete(0);
end;

(* ************************************************************************** *)

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
  Self.Create(ACandel.Date,ACandel.Time,ACandel.Open,ACandel.High,ACandel.Low,ACandel.Close,ACandel.Vol);
end;

constructor TCandel.Cretae(AValue: String);
var
  xCandel: TCandel;
begin
  xCandel := GetParserCandel(AValue);
  Self.CreateCandel(xCandel);
end;

function TCandel.GetDateTime: TDateTime;
begin
  Result := Self.Date + Self.Time;
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

{ TMemoryCandels }

constructor TMemoryCandels.Create;
begin
  //FIndexLine := 0;
  FStrings := nil;
end;

destructor TMemoryCandels.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

procedure TMemoryCandels.CheсkStream;
begin
  if not Assigned(FStrings) then
    raise Exception.Create('Error Message: String не определен');
end;

function TMemoryCandels.GetLine(const AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < FStrings.Count) then
    Result := FStrings[AIndex]
  else
    raise Exception.Create('Error Message: Выход за границы массива');
end;

function TMemoryCandels.GetCandels(Index: Integer): TCandel;
var
  xCandel: TCandel;
  xS: String;
begin
  xS := GetLine(Index);
  xCandel.Cretae(xS);
  Result := xCandel;
end;

function TMemoryCandels.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

procedure TMemoryCandels.SetFileName(const Value: String);
begin
  //FIndexLine := 0;
  FFileName := Value;
  if Assigned(FStrings) then
    FreeAndNil(FStrings);
  FStrings := TStringList.Create;
  FStrings.LoadFromFile(FFileName);
  FStrings.Delete(0);
end;

{ TMemoryStructures }

constructor TMemoryStructures.Create;
begin
  inherited Create;
  FIndexCandel := 0;
  FSourceCount := SOURCE_COUNT;
  FFutureCount := FUTURE_COUNT;
  FValueStructure := TValueStructure.Create;
end;

destructor TMemoryStructures.Destroy;
begin
  FreeAndNil(FValueStructure);
  inherited Destroy;
end;

procedure TMemoryStructures.FirstStructure;
begin
  FIndexCandel := 0;
  NextStructure;
end;

function TMemoryStructures.GetEOF: Boolean;
var
  xInd: Integer;
begin
  xInd := FIndexCandel + FSourceCount + FFutureCount;
  Result := xInd >= FStrings.Count;
end;

procedure TMemoryStructures.NextStructure;

  procedure _NextStructureSource(const ACandels: TCandelList);
  var
    xCandel: TCandel;
    i: Integer;
  begin
    // Опорный векторо
    ACandels.Clear;
    for i := 0 to FSourceCount - 1 do
    begin
      var xInd := i + FIndexCandel;
      xCandel := Self.Candels[xInd];
      ACandels.Add(xCandel);
    end;
  end;

  procedure _NextStructureFuture(const ACandels: TCandelList);
  var
    xCandel: TCandel;
    i: Integer;
  begin
    // Будущий вектор
    ACandels.Clear;
    for i := 0 to FFutureCount - 1 do
    begin
      var xInd := i + FSourceCount + FIndexCandel;
      xCandel := Self.Candels[xInd];
      ACandels.Add(xCandel);
    end;
  end;

begin
  try
    FValueStructure.SourceRowID := FIndexCandel;
    _NextStructureSource(FValueStructure.SourceVectors);
    _NextStructureFuture(FValueStructure.FutureVectors);
    Inc(FIndexCandel);
  except
    on E : Exception do
      raise Exception.Create('Error Message: TMemoryStructures.NextStructure ' + sLineBreak +  '{' + E.Message + '}');
  end;
end;

{ TStructure }

constructor TStructure.Create;
begin
  FStatus := TTypeStructure.tsNull;
  FSourceVectors := TCandelList.Create;
  FFutureVectors := TCandelList.Create;
end;

destructor TStructure.Destroy;
begin
  FreeAndNil(FFutureVectors);
  FreeAndNil(FSourceVectors);
  inherited;
end;

procedure TStructure.Clear;
begin
  FSourceVectors.Clear;
  FSourceVectors.Clear;
end;

{ TVectorStructure }

constructor TVectorStructure.Create;
begin
  inherited;
  FStatus := TTypeStructure.tsVector;
end;

procedure TVectorStructure.Transform(const AValueStructure: TValueStructure);

//  procedure _SetLastPriceAndVol(const AValueStructure: TValueStructure; var APrice, AVol: Double);
//  var
//    Count: Integer;
//  begin
//    Count  := AValueStructure.SourceVectors.Count;
//    APrice := AValueStructure.SourceVectors[Count - 1].Close;
//    AVol   := AValueStructure.SourceVectors[Count - 1].Vol;
//  end;
//
//  procedure _SetTransformCandels(const AInCandels, AOutCandels: TCandelList; const APrice, AVol: Double);
//  var
//    xInCandel: TCandel;
//    xOutCandel: TCandel;
//  begin
//    AOutCandels.Clear;
//    for xInCandel in AInCandels do
//    begin
//      FillChar(xOutCandel,Sizeof(TCandel),0);
//      xOutCandel.Open  := xInCandel.Open / APrice;
//      xOutCandel.High  := xInCandel.High / APrice;
//      xOutCandel.Low   := xInCandel.Low  / APrice;
//      xOutCandel.Close := xInCandel.Close/ APrice;
//      xOutCandel.Vol   := xInCandel.Vol  / AVol;
//      AOutCandels.Add(xOutCandel);
//    end;
//  end;

  procedure _SetTransformSource(const AValueStructure: TValueStructure; const APrice, AVol: Double);
  begin
    SetTransformCandels(AValueStructure.SourceVectors,FSourceVectors,APrice,AVol);
  end;

  procedure _SetTransformFuture(const AValueStructure: TValueStructure; const APrice, AVol: Double);
  begin
    SetTransformCandels(AValueStructure.FutureVectors,FFutureVectors,APrice,AVol);
  end;

var
  xPrice, xVol: Double;
begin
  SetLastPriceAndVol(AValueStructure.SourceVectors,xPrice,xVol);
  _SetTransformSource(AValueStructure,xPrice,xVol);
  _SetTransformFuture(AValueStructure,xPrice,xVol);
end;

{ TValueStructure }

constructor TValueStructure.Create;
begin
  inherited;
  FStatus := TTypeStructure.tsValue;
end;

procedure TValueStructure.Transform(const AVectorStructure: TVectorStructure; const APrice, AVol: Double);

  procedure _SetTransformCandels(const AInCandels, AOutCandels: TCandelList; const APrice, AVol: Double);
  var
    xInCandel: TCandel;
    xOutCandel: TCandel;
  begin
    AOutCandels.Clear;
    for xInCandel in AInCandels do
    begin
      FillChar(xOutCandel,Sizeof(TCandel),0);
      xOutCandel.Open  := xInCandel.Open * APrice;
      xOutCandel.High  := xInCandel.High * APrice;
      xOutCandel.Low   := xInCandel.Low  * APrice;
      xOutCandel.Close := xInCandel.Close* APrice;
      xOutCandel.Vol   := xInCandel.Vol  * AVol;
      AOutCandels.Add(xOutCandel);
    end;
  end;

begin
  _SetTransformCandels(AVectorStructure.SourceVectors,FSourceVectors,APrice, AVol);
  _SetTransformCandels(AVectorStructure.FutureVectors,FFutureVectors,APrice, AVol);
end;





end.
