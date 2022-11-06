unit Lb.SysUtils.Candel;

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
  TTypeCandel = (
    tcSource, // источник данных
    tcFuture  // Будушие свиячи
  );

  ///<summary>Тип цены</summary>
  TTypePrice = (tpNon, tpOpen, tpHigh, tpLow, tpClose);

  ///<summary>Свеча</summary>
  TCandel = record
  public
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
    function GetPrice(ATypePrice: TTypePrice): Double;
    procedure SetPrice(ATypePrice: TTypePrice; const Value: Double);
  public
    constructor Create(ADate, ATime: TDateTime; AOpen, AHigh, ALow, AClose, AVol: Double); overload;
    constructor CreateCandel(ACandel: TCandel); overload;
    constructor Cretae(AValue: String); overload;
    function ToString: String;
    function ToStringShort: String;
    function ToStringCandel: String;
    property DateTime: TDateTime read GetDateTime;
    property Price[ATypePrice: TTypePrice]: Double read GetPrice write SetPrice;
  end;

  ///<summary>Список свечей</summary>
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

  ///<summary>Помещаем все массив свечей в память в потоке</summary>
  TMemoryCandels = class(TObject)
  private type
    TIntegration = (tiForward, tiBack);
  private
    FLine: String;
    FCandel: TCandel;
    FFileName: String;
    FStream: TFileStream;
    function GetLine(AIntegration: TIntegration): String;
    procedure SetFileName(const Value: String);
  protected
    procedure CheсkStream;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CandelFirst;
    procedure CandelsFirstOneStep(const ASelectCount: Integer; const ACandels: TCandelList);
    procedure CandelLast;
    procedure CandelNext;
    procedure CandelsNextOneStep(const ASelectCount: Integer; const ACandels: TCandelList);
    procedure CandelPrior;
    function CandelEOF: Boolean;
    function CandelBOF: Boolean;
    property Line: String read FLine;
    property Candel: TCandel read FCandel;
    property FileName: String read FFileName write SetFileName;
  end;

  ///<summary>По блочно читаем данные из памяти (точнее по структурно)</summary>
  TMemoryStructures = class(TMemoryCandels)
  public const
    SOURCE_COUNT = 10;    // Примерный размер блока - опорного массива
    FUTURE_COUNT = 3;     // Примерный размер блока - массива ожиданий
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
    procedure Assign(AStructure: TStructure);
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
    ///<summary>При образование данных из значений, в целое число от 0 до 100</summary>
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
    ///<summary>Получаем максимальное и минимальное значение</summary>
    procedure MaxAndMin(var AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);
    property SourceRowID: Integer read FSourceRowID write FSourceRowID;
  end;

  ///<summary>Математика с векторами</summary>
  TMathVector = record
    class procedure SetLength(const ACandels: TCandelList; var ALengthPrice, ALengthVol: Double); static;
    class procedure SetSubtract(const AValue1, AValue2, AResult: TCandelList); static;
    class function IsComparisonStructure(const AStructure1, AStructure2: TStructure): Boolean; static;
    class procedure SetSubtractStructure(const AStructure1, AStructure2: TStructure; var ALengthPrice, ALengthVol: Double); static;
  end;

type
  ///<summary>Патерн структуры</summary>
  TStructurePatern = class(TObject)
  private
    FStructure: TVectorStructure;
    FLengthPrice: Double;
    FLengthVol: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Structure: TVectorStructure read FStructure;
    property LengthPrice: Double read FLengthPrice write FLengthPrice;
    property LengthVol: Double read FLengthVol write FLengthVol;
  end;

  ///<summary>Список патерн структур</summary>
  TStructurePaternList = TObjectList<TStructurePatern>;


  ///<summary>Создаем массив близких структур по вектору</summary>
  TStructureSearch = class(TObject)
  public type
    TSearchThread = class(TThread)
    private
      FStructureSearch: TStructureSearch;
    protected
      procedure DoAddStructurePatern;
      procedure Execute; override;
    public
      constructor Create(AStructureSearch: TStructureSearch);
      destructor Destroy; override;
    end;
  private
    FFileName: String;
    FSourceCount: Integer;
    FFutureCount: Integer;
    FVectorStructure: TVectorStructure;
    FStructurePaterns: TStructurePaternList;
    procedure SetVectorStructure(const Value: TVectorStructure);
  protected
    FOnAddStructurePatern: TNotifyEvent;
    procedure DoAddStructurePatern;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property StructurePaterns: TStructurePaternList read FStructurePaterns;
    property VectorStructure: TVectorStructure read FVectorStructure write SetVectorStructure;
    property FileName: String read FFileName write FFileName;
    property SourceCount: Integer read FSourceCount write FSourceCount;
    property FutureCount: Integer read FFutureCount write FFutureCount;
    property OnAddStructurePatern: TNotifyEvent write FOnAddStructurePatern;
  end;

type
  TVectorAPI = record
    /// <summary>С равниваем два числа, с точностью</summary>
    class function GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean; static;
    /// <summary>Копирование свечай</summary>
    class procedure MoveCandels(const ASource, ADest: TCandelList); static;
    /// <summary>Сократить значение работы</summary>
    class procedure CutDownCandels(const ASource: TCandelList; ACount: Integer); static;
    /// <summary>Польное сравнение векторов</summary>
    class function SameStructureSource(const AStructure1, AStructure2: TStructure): Boolean; static;
  end;

implementation

uses
  Lb.Logger,
  System.Math;

(******************************************************************************)

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

/// <summary>Получаем последний значение цены и объема</summary>
procedure SetLastPriceAndVol(const ACandels: TCandelList; var APrice, AVol: Double);
var
  Count: Integer;
begin
  Count  := ACandels.Count;
  APrice := ACandels[Count - 1].Close;
  AVol   := ACandels[Count - 1].Vol;
end;

procedure SetTransformCandels(const AInCandels, AOutCandels: TCandelList; const AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);

  function _GetValue(const AValue, AMaxValue, AMinValue: Double): Integer;
  begin
    Result := Trunc(100 * (AValue - AMinValue)/(AMaxValue - AMinValue));
  end;

var
  xInCandel: TCandel;
  xOutCandel: TCandel;
begin
  AOutCandels.Clear;
  for xInCandel in AInCandels do
  begin
    FillChar(xOutCandel,Sizeof(TCandel),0);

    xOutCandel.Open  := _GetValue(xInCandel.Open ,AMaxPrice , AMinPrice);
    xOutCandel.High  := _GetValue(xInCandel.High ,AMaxPrice , AMinPrice);
    xOutCandel.Low   := _GetValue(xInCandel.Low  ,AMaxPrice , AMinPrice);
    xOutCandel.Close := _GetValue(xInCandel.Close,AMaxPrice , AMinPrice);
    xOutCandel.Vol   := _GetValue(xInCandel.Vol  ,AMaxVol   , AMinVol);

    AOutCandels.Add(xOutCandel);
  end;
end;

(******************************************************************************)
(* Математека - работы с вектором                                             *)
(******************************************************************************)

{ TVectorAPI }

class function TVectorAPI.GetSameValue(const AValue1, AValue2: Double; const AEpsilon: Integer): Boolean;
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

class procedure TVectorAPI.MoveCandels(const ASource, ADest: TCandelList);
begin
  // Копировать
  ADest.Clear;
  for var xC in ASource do
    ADest.Add(xC);
end;

class procedure TVectorAPI.CutDownCandels(const ASource: TCandelList; ACount: Integer);
begin
  // Удалить лишние значение, сократить до нужного размера
  while ASource.Count > ACount do
    ASource.Delete(0);
end;

class function TVectorAPI.SameStructureSource(const AStructure1, AStructure2: TStructure): Boolean;

  function _SameValue(const AValue1, AValue2: Double): Boolean;
  var
    xValue1, xValue2: Integer;
  begin
    xValue1 := Trunc(AValue1);
    xValue2 := Trunc(AValue2);
    Result := xValue1 = xValue2;
  end;


  function _SameCandel(const ACandel1, ACandel2: TCandel): Boolean;
  begin
    Result := _SameValue(ACandel1.Open,  ACandel2.Open) and
              _SameValue(ACandel1.High,  ACandel2.High) and
              _SameValue(ACandel1.Low,   ACandel2.Low)  and
              _SameValue(ACandel1.Close, ACandel2.Close);
  end;

var
  xC1, xC2: TCandel;
  xCount1, xCount2: Integer;
begin
  Result := False;

  if not Assigned(AStructure1) then
    Exit;

  if not Assigned(AStructure2) then
    Exit;

  xCount1 := AStructure1.SourceVectors.Count;
  xCount2 := AStructure2.SourceVectors.Count;
  if (xCount1 > 1) and (xCount1 = xCount2) then
  begin
    // На одну свечу меньше так как последния свяча постоянно обновляется
    for var i := 0 to xCount1 - 2 do
    begin
      xC1 := AStructure1.SourceVectors[i];
      xC2 := AStructure2.SourceVectors[i];
      if not _SameCandel(xC1,xC2) then
        Exit;
    end;
    Result := True;
  end;
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

function TCandel.GetPrice(ATypePrice: TTypePrice): Double;
begin
  case ATypePrice of
    tpOpen: Result := Self.Open;
    tpHigh: Result := Self.High;
    tpLow: Result := Self.Low;
    tpClose: Result := Self.Close;
  else
    Result := 0;
  end;
end;

procedure TCandel.SetPrice(ATypePrice: TTypePrice; const Value: Double);
begin
  case ATypePrice of
    tpOpen: Self.Open := Value;
    tpHigh: Self.High := Value;
    tpLow: Self.Low := Value;
    tpClose: Self.Close := Value;
  end;
end;

function TCandel.ToString: String;
var
  xS: String;
begin
  xS := 'D: ' + DateToStr(Self.Date) + '; ' +
        'T: ' + TimeToStr(Self.Time) + '; ' +
        ToStringShort;
  Result := xS;
end;

function TCandel.ToStringShort: String;
begin
  var xS :=
    'O: ' + FloatToStr(Self.Open) + '; ' +
    'H: ' + FloatToStr(Self.High) + '; ' +
    'L: ' + FloatToStr(Self.Low) + '; ' +
    'C: ' + FloatToStr(Self.Close) + '; ' +
    'V: ' + FloatToStr(Self.Vol);
  Result := xS;
end;

function TCandel.ToStringCandel: String;
begin
  var xS :=
    IntToStr(Trunc(Self.Open)) + ';' +
    IntToStr(Trunc(Self.High)) + ';' +
    IntToStr(Trunc(Self.Low)) + ';' +
    IntToStr(Trunc(Self.Close)) + ';' +
    IntToStr(Trunc(Self.Vol));
  Result := xS;
end;

{ TMemoryCandels }

constructor TMemoryCandels.Create;
begin
  FStream := nil;
end;

destructor TMemoryCandels.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TMemoryCandels.SetFileName(const Value: String);
begin
  FFileName := Value;
  if FileExists(FFileName) then
  begin
    if not Assigned(FStream) then
      FStream := TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite);
  end;
end;


function TMemoryCandels.CandelEOF: Boolean;
begin
  Result := FStream.Position = FStream.Size;
end;

function TMemoryCandels.CandelBOF: Boolean;
begin
  Result := FStream.Position = 0;
end;

procedure TMemoryCandels.CheсkStream;
begin
  if not Assigned(FStream) then
    raise Exception.Create('Error Message: String не определен');
end;

function TMemoryCandels.GetLine(AIntegration: TIntegration): String;

  function _ForwardLine: String;
  var
    xS: String;
    xC1, xC2: AnsiChar;
  begin
    xS := '';
    while True do
    begin
      FStream.Read(xC1,1);
      FStream.Read(xC2,1);
      FStream.Position := FStream.Position - 1;
      if not((xC1 = #10) or (xC1 = #13)) then
        xS := xS + Char(xC1);
      if ((xC1 = #10) and (xC2 = #13) or (xC1 = #10)) then
      begin
        FStream.Position := FStream.Position + 1;
        Break;
      end;
    end;
    Result := xS;
  end;


  function _BackLine: String;
  var
    xS: String;
    xC1, xC2: AnsiChar;
  begin
    xS := '';
    while True do
    begin
      FStream.Position := FStream.Position - 2;
      FStream.Read(xC1,1);
      FStream.Read(xC2,1);
      if not CharInSet(xC1,[#13,#10]) then
        xS := Char(xC1) + xS;
      if not xS.IsEmpty then
        if ((xC1 = #10) and (xC2 = #13) or (xC2 = #10)) then
        begin
          FStream.Position := FStream.Position + 1;
          Break;
        end;
      FStream.Position := FStream.Position - 1;
    end;
    Result := xS;
  end;

var
  xS: String;
begin
  xS := '';
  case AIntegration of
    tiForward: xS := _ForwardLine;
    tiBack: xS := _BackLine;
  end;
  FCandel.Cretae(xS);
  Result := xS;
end;

procedure TMemoryCandels.CandelFirst;
begin
  CheсkStream;
  FStream.Position := 0;
  FLine := GetLine(TIntegration.tiForward);
end;

procedure TMemoryCandels.CandelLast;
begin
  CheсkStream;
  FStream.Position := FStream.Size;
  FLine := GetLine(TIntegration.tiBack);
end;

procedure TMemoryCandels.CandelNext;
begin
  CheсkStream;
  FLine := GetLine(TIntegration.tiForward);
end;

procedure TMemoryCandels.CandelsFirstOneStep(const ASelectCount: Integer; const ACandels: TCandelList);
begin
  FStream.Position := 0;
  CandelsNextOneStep(ASelectCount,ACandels);
end;


var
  IndexNext: Integer = 0;

procedure TMemoryCandels.CandelsNextOneStep(const ASelectCount: Integer; const ACandels: TCandelList);
var
  xC: TCandel;
  xLine: String;
  xPosition: Int64;
begin
  {$IFDEF DEBUG}
  Inc(IndexNext);

  if IndexNext = 4191 then
  begin

    with TStringList.Create do
    begin
      Clear;
      Free;
    end;

  end;


  TLogger.LogTree(0,'TMemoryCandels.CandelsNextOneStep: ' + IndexNext.ToString);
  TLogger.LogTreeText(3,'>> ASelectCount = ' + ASelectCount.ToString);
  {$ENDIF}

  if not Assigned(ACandels) then
    raise Exception.Create('Error Message: Массив не определен');

  if ASelectCount > 0 then
  begin
    CheсkStream;
    ACandels.Clear;
    // Читаем первую свечу
    xLine := GetLine(TIntegration.tiForward);
    {$IFDEF DEBUG}
    TLogger.LogTreeText(3,'>> ' + xLine);
    {$ENDIF}
    xC := TCandel.Cretae(xLine);
    ACandels.Add(xC);
    xPosition := FStream.Position;
    // Перебираем все остальные свечи
    for var i := 1 to ASelectCount - 1 do
    begin
      xLine := GetLine(TIntegration.tiForward);
      {$IFDEF DEBUG}
      TLogger.LogTreeText(3,'>> ' + xLine);
      {$ENDIF}
      xC := TCandel.Cretae(xLine);
      ACandels.Add(xC);
    end;
    {$IFDEF DEBUG}
    TLogger.LogTreeText(3,'>> ' + ACandels.Count.ToString);
    {$ENDIF}
    FStream.Position := xPosition;
  end;
end;

procedure TMemoryCandels.CandelPrior;
begin
  CheсkStream;
  FLine := GetLine(TIntegration.tiBack);
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

  // Читаем первую строку, с наименование полей
  FStream.Position := 0;
  GetLine(TIntegration.tiForward);

  NextStructure;
end;

function TMemoryStructures.GetEOF: Boolean;
begin
  Result := Self.CandelEOF;
end;

procedure TMemoryStructures.NextStructure;

  procedure _NextStructureSource(const ASourceCandels, ACandels: TCandelList);
  begin
    ACandels.Clear;
    for var i := 0 to FSourceCount - 1 do
    begin
      var xCandel := ASourceCandels.Items[i];
      ACandels.Add(xCandel);
    end;
  end;

  procedure _NextStructureFuture(const ASourceCandels, ACandels: TCandelList);
  begin
    ACandels.Clear;
    for var i := 0 to FFutureCount - 1 do
    begin
      var xInd := i + FSourceCount;
      var xCandel := ASourceCandels.Items[xInd];
      ACandels.Add(xCandel);
    end;
  end;

var
  xCandels: TCandelList;
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TMemoryStructures.NextStructure');
  {$ENDIF}
  xCandels := TCandelList.Create;
  try
    FValueStructure.SourceRowID := FIndexCandel;
    Self.CandelsNextOneStep(FSourceCount + FFutureCount,xCandels);
    _NextStructureSource(xCandels, FValueStructure.SourceVectors);
    _NextStructureFuture(xCandels, FValueStructure.FutureVectors);
    Inc(FIndexCandel); {индекс работы}
  finally
    FreeAndNil(xCandels);
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

procedure TStructure.Assign(AStructure: TStructure);

  procedure _AssingCandels(const ASource, ADesc: TCandelList);
  begin
    ADesc.Clear;
    for var xC in ASource do
      ADesc.Add(xC);
  end;

begin
  FStatus := AStructure.Status;
  _AssingCandels(AStructure.SourceVectors,FSourceVectors);
  _AssingCandels(AStructure.FutureVectors,FFutureVectors);
end;

{ TVectorStructure }

constructor TVectorStructure.Create;
begin
  inherited;
  FStatus := TTypeStructure.tsVector;
end;

procedure TVectorStructure.Transform(const AValueStructure: TValueStructure);

  procedure _SetTransformSource(const AValueStructure: TValueStructure; const AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);
  begin
    SetTransformCandels(AValueStructure.SourceVectors,FSourceVectors, AMaxPrice, AMinPrice, AMaxVol, AMinVol);
  end;

  procedure _SetTransformFuture(const AValueStructure: TValueStructure; const AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);
  begin
    SetTransformCandels(AValueStructure.FutureVectors,FFutureVectors, AMaxPrice, AMinPrice, AMaxVol, AMinVol);
  end;

var
  xMaxPrice, xMinPrice, xMaxVol, xMinVol: Double;
begin
  AValueStructure.MaxAndMin(xMaxPrice, xMinPrice, xMaxVol, xMinVol);
  //SetLastPriceAndVol(AValueStructure.SourceVectors,xPrice,xVol);
  _SetTransformSource(AValueStructure, xMaxPrice, xMinPrice, xMaxVol, xMinVol);
  _SetTransformFuture(AValueStructure, xMaxPrice, xMinPrice, xMaxVol, xMinVol);
end;

{ TValueStructure }

constructor TValueStructure.Create;
begin
  inherited;

  FStatus := TTypeStructure.tsValue;
end;

procedure TValueStructure.MaxAndMin(var AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);

  procedure _SetMaxAndMinValue(const ACandels: TCandelList; var AMaxPrice, AMinPrice, AMaxVol, AMinVol: Double);
  begin
    if ACandels.Count > 0 then
      for var xC in ACandels do
      begin
        if xC.High > AMaxPrice then AMaxPrice := xC.High;
        if xC.Low  < AMinPrice then AMinPrice := xC.Low;
        if xC.Vol  > AMaxVol   then AMaxVol := xC.Vol;
        if xC.Vol  < AMinVol   then AMinVol := xC.Vol;
      end;
  end;

begin
  AMaxPrice := 0;
  AMinPrice := 0;
  AMaxVol := 0;
  AMinVol := 0;
  if FSourceVectors.Count > 0 then
  begin
    var Count := FSourceVectors.Count;
    AMaxPrice := FSourceVectors[Count - 1].High;
    AMinPrice := FSourceVectors[Count - 1].Low;
    AMaxVol := FSourceVectors[Count - 1].Vol;
    AMinVol := FSourceVectors[Count - 1].Vol;
    _SetMaxAndMinValue(FSourceVectors, AMaxPrice, AMinPrice, AMaxVol, AMinVol);
    //_SetMaxAndMinValue(FFutureVectors, AMaxPrice, AMinPrice, AMaxVol, AMinVol);
  end;
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

{ TMathVector }

class function TMathVector.IsComparisonStructure(const AStructure1, AStructure2: TStructure): Boolean;
const
  IS_COMPARISON_STRUCTURE = 50;


  function _GetLenght(const ACandel1, ACandel2: TCandel): Double;
  var
    xR: TCandel;
    xSum: Double;
  begin
    xR.Open  := ACandel1.Open  - ACandel2.Open;
    xR.High  := ACandel1.High  - ACandel2.High;
    xR.Low   := ACandel1.Low   - ACandel2.Low;
    xR.Close := ACandel1.Close - ACandel2.Close;

    xSum :=
      Power(xR.Open,2) +
      Power(xR.High,2) +
      Power(xR.Low,2) +
      Power(xR.Close,2);

    Result := Power(xSum,0.5);

  end;

var
  i, iCount: Integer;
  xC1, xC2: TCandel;
  xIsComparison: Boolean;
begin
  xIsComparison := False;
  if (AStructure1.SourceVectors.Count > 0) and
     (AStructure2.SourceVectors.Count > 0) and
     (AStructure1.SourceVectors.Count = AStructure2.SourceVectors.Count)
  then
  begin
    iCount := AStructure1.SourceVectors.Count;
    for i := 0 to iCount - 1 do
    begin
      xC1 := AStructure1.SourceVectors[i];
      xC2 := AStructure2.SourceVectors[i];

      xIsComparison := _GetLenght(xC1,xC2) <= IS_COMPARISON_STRUCTURE;
      if not xIsComparison then
        Break;
    end;
  end;
  Result := xIsComparison;
end;

class procedure TMathVector.SetLength(const ACandels: TCandelList; var ALengthPrice, ALengthVol: Double);
var
  xLengthPriceSum, xLengthVolSum: Double;
begin
  ALengthPrice := 0;
  ALengthVol   := 0;

  xLengthPriceSum := 0;
  xLengthVolSum := 0;
  for var xC in ACandels do
  begin
    xLengthPriceSum := xLengthPriceSum + 
      Power(xC.Open,2) +
      Power(xC.High,2) +
      Power(xC.Low,2) +
      Power(xC.Close,2);
      
    xLengthVolSum := xLengthVolSum + 
      Power(xC.Vol,2);
  end;
  ALengthPrice := Power(xLengthPriceSum, 0.5);
  ALengthVol   := POwer(xLengthVolSum,   0.5);
end;

class procedure TMathVector.SetSubtract(const AValue1, AValue2, AResult: TCandelList);
var
  xR: TCandel;
  i, Count: Integer;
begin
  AResult.Clear;
  if (AValue1.Count = AValue2.Count) and (AValue1.Count > 0) then
  begin
    Count := AValue1.Count;
    for i := 0 to Count - 1 do
    begin
      var xC1 := AValue1[i];
      var xC2 := AValue2[i];
    
      xR.Open  := xC1.Open   - xC2.Open;
      xR.High  := xC1.High   - xC2.High;
      xR.Low   := xC1.Low    - xC2.Low;
      xR.Close := xC1.Close  - xC2.Close;
      xR.Vol   := xC1.Vol    - xC2.Vol;

      AResult.Add(xR);
    end;
  end;
end;

class procedure TMathVector.SetSubtractStructure(const AStructure1, AStructure2: TStructure; var ALengthPrice, ALengthVol: Double);
var
  xResult: TCandelList;
begin
  ALengthPrice := 0; 
  ALengthVol := 0;
  xResult := TCandelList.Create;
  try 
    TMathVector.SetSubtract(AStructure1.SourceVectors,AStructure2.SourceVectors,xResult);
    TMathVector.SetLength(xResult,ALengthPrice, ALengthVol);
  finally
    FreeAndNil(xResult);
  end;
end;

{ TStructurePatern }

constructor TStructurePatern.Create;
begin
  FStructure := TVectorStructure.Create;
  FLengthPrice := 0;
  FLengthVol := 0;
end;

destructor TStructurePatern.Destroy;
begin
  FreeAndNil(FStructure);
  inherited;
end;

{ TStructureSearch.TSearchThread }

constructor TStructureSearch.TSearchThread.Create(AStructureSearch: TStructureSearch);
begin
  inherited Create(True);
  FStructureSearch := AStructureSearch;
end;

destructor TStructureSearch.TSearchThread.Destroy;
begin
  inherited;
end;

procedure TStructureSearch.TSearchThread.DoAddStructurePatern;
begin
  // Послать сообщений
  TThread.Synchronize(nil,
    procedure ()
    begin
      FStructureSearch.DoAddStructurePatern;
    end
  );
end;

procedure TStructureSearch.TSearchThread.Execute;

  function _GetCreateStructurePatern(const AVectorStructure: TVectorStructure; const ALengthPrice, ALengthVol: Double): TStructurePatern;
  var
    xPatern: TStructurePatern;
  begin
    {Создание структуры}
    xPatern := TStructurePatern.Create;
    xPatern.Structure.Assign(AVectorStructure);
    xPatern.LengthPrice := ALengthPrice;
    xPatern.LengthVol := ALengthVol;
    Result := xPatern;
  end;

  procedure _SetAddStructurePatern(const AVectorStructure: TVectorStructure; const ALengthPrice, ALengthVol: Double);
  const
    STRUCTURE_PATERNS_COUNT = 100;
  var
    i, iCount: Integer;
    xStructurePatern: TStructurePatern;
    xPatern: TStructurePatern;
  var
    xSng: Boolean;
  begin
    xSng := False;

    iCount := FStructureSearch.StructurePaterns.Count;
    if iCount > 0 then
    begin
      // ---------------------------------------------------------------------
      // Находим ближайшие патерны
      for i := 0 to iCount - 1 do
      begin
        xStructurePatern := FStructureSearch.StructurePaterns[i];
        if xStructurePatern.LengthPrice > ALengthPrice then
        begin
          xPatern := _GetCreateStructurePatern(AVectorStructure, ALengthPrice, ALengthVol);
          FStructureSearch.StructurePaterns.Insert(i,xPatern);
          DoAddStructurePatern;
          xSng := True;
          Break;
        end;
      end;
      // ---------------------------------------------------------------------
      // Добавляем первый патерн
      if (not xSng) and (FStructureSearch.StructurePaterns.Count < STRUCTURE_PATERNS_COUNT) then
      begin
        xPatern := _GetCreateStructurePatern(AVectorStructure, ALengthPrice, ALengthVol);
        FStructureSearch.StructurePaterns.Add(xPatern);
        DoAddStructurePatern;
      end;
      // Контролируем количество, найденных патернов
      if iCount > STRUCTURE_PATERNS_COUNT then
        FStructureSearch.StructurePaterns.Delete(iCount - 1);
    end
    else
    begin
      xPatern := _GetCreateStructurePatern(AVectorStructure, ALengthPrice, ALengthVol);
      FStructureSearch.StructurePaterns.Add(xPatern);
      DoAddStructurePatern;
    end;
  end;

var
  xVectorStructure: TVectorStructure;
  xLengthPrice, xLengthVol: Double;
  xSearchStructures: TMemoryStructures;
begin
  if Assigned(FStructureSearch) then
  begin
    FStructureSearch.StructurePaterns.Clear;
    xSearchStructures := TMemoryStructures.Create;

    // Условие структуры
    xSearchStructures.FileName    := FStructureSearch.FileName;
    xSearchStructures.SourceCount := FStructureSearch.SourceCount;
    xSearchStructures.FutureCount := FStructureSearch.FutureCount;

    {todo: Переработать структуру в обратном порядке}
    xSearchStructures.FirstStructure;
    while not xSearchStructures.EOF do
    begin
      xVectorStructure := TVectorStructure.Create;
      try
        xVectorStructure.Transform(xSearchStructures.Structure);
        if TMathVector.IsComparisonStructure(FStructureSearch.VectorStructure, xVectorStructure) then
        begin
          TMathVector.SetSubtractStructure(FStructureSearch.VectorStructure, xVectorStructure, xLengthPrice, xLengthVol);
          _SetAddStructurePatern(xVectorStructure, xLengthPrice, xLengthVol);
        end;
      finally
        FreeAndNil(xVectorStructure);
      end;
      xSearchStructures.NextStructure;
    end;
  end;
end;

{ TStructureSearch }

constructor TStructureSearch.Create;
begin
  FStructurePaterns := TStructurePaternList.Create;
  FVectorStructure := TVectorStructure.Create;
end;

destructor TStructureSearch.Destroy;
begin
  FreeAndNil(FVectorStructure);
  FreeAndNil(FStructurePaterns);
  inherited;
end;

procedure TStructureSearch.DoAddStructurePatern;
begin
  if Assigned(FOnAddStructurePatern) then
    FOnAddStructurePatern(Self);
end;

procedure TStructureSearch.SetVectorStructure(const Value: TVectorStructure);
begin
  if not TVectorAPI.SameStructureSource(Value,FVectorStructure) then
  begin

  end;
end;

end.
