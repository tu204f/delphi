unit Lb.Export;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>
  /// Свеча движения цены
  ///</sumary>
  TCandel = class(TObject)
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
  public
    procedure CopyCandel(const ACandel: TCandel); virtual;
    function ToString: String; override;
  end;

  ///<summary>
  /// Список свячей
  ///</summary>
  TCandelList = class(TObjectList<TCandel>)
  private
    function GetMaxPrice: Double;
    function GetMinPrice: Double;
  public
    property MaxPrice: Double read GetMaxPrice;
    property MinPrice: Double read GetMinPrice;
  end;

  ///<summary>
  /// Парсинг свечи
  ///</summary>
  TParam = class(TCandel)
  public
    Date : TDateTime;
    Time : TDateTime;
  public
    procedure SetTextExport(const AValue: String);
    function ToString: String; override;
  end;

  ///<summary>
  /// Прасинг свячи
  ///</summary>
  TParsingCandels = class(TStringList)
  private
    FParam: TParam;
    function GetParams(Index: Integer): TParam;
  public
    constructor Create;
    destructor Destroy; override;
    property Params[Index: Integer]: TParam read GetParams;
  end;

type
  ///<summary>
  /// Блок
  ///</summary>
  TBlock = class(TObject)
  public type
    TBlockParam = record
      MaxValue: Double;
      MinValue: Double;
      Count: Integer;
    public
      procedure Clear;
    end;
  private
    FID: Integer;
    FLeft: TBlockParam;
    FRight: TBlockParam;
    FSources: TCandelList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TCandel;
    function GetCountLeft: Integer;
    function GetCountRight: Integer;
  protected
    property Left: TBlockParam read FLeft;
    property Right: TBlockParam read FRight;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    procedure SetCountParam(const ACountLeft, ACountRight: Integer);
    procedure UpDataParam(const AParam: TParam);
    function IsFull: Boolean;
    procedure SetMaxAndMinValue;
  public
    property Items[Index: Integer]: TCandel read GetItems;
    property Count: Integer read GetCount;
    property CountLeft: Integer read GetCountLeft;
    property CountRight: Integer read GetCountRight;
  end;

  ///<summary>
  /// Параметрический блок
  ///</summary>
  TParamBlock = class(TBlock)
  private
    function GetDelta: Double;
    function GetTop: Double;
    function GetBottom: Double;
    function GetCoffTop: Double;
    function GetCoffBottom: Double;
  private
    function GetBuyTP: Double;
    function GetBuySL: Double;
    function GetSellTP: Double;
    function GetSellSL: Double;
    function GetBuyR: Double;
    function GetSellR: Double;
    function GetRisk: Double;
  public
    ///<summary>
    /// Верхний придел
    ///</summary>
    property Top: Double read GetTop;
    ///<summary>
    /// Нижний предел
    ///</summary>
    property Bottom: Double read GetBottom;
    ///<summary>
    /// Средния значение
    ///</summary>
    property Delta: Double read GetDelta;
  public
    property CoffTop: Double read GetCoffTop;
    property CoffBottom: Double read GetCoffBottom;
  public
    property BuyTP: Double read GetBuyTP;
    property BuySL: Double read GetBuySL;
    property BuyR: Double read GetBuyR;
    property SellTP: Double read GetSellTP;
    property SellSL: Double read GetSellSL;
    property SellR: Double read GetSellR;
    property Risk: Double read GetRisk;
  end;


  TParamBlockList = TObjectList<TParamBlock>;

procedure SetSaveBlock(const ABlock: TParamBlock);

implementation

uses
  System.Math;

procedure SetCandelNorm(const AMax, AMin: Double; ASource, ADest: TCandel);

  function _NormPrice(const AMax, AMin, AValue: Double): Double;
  var
    xDelta, xDeltaMin: Double;
  begin
    xDelta := (AMax - AMin);
    xDeltaMin := (AValue - AMin);
    Result := Trunc(RoundTo(xDeltaMin/xDelta,-3) * 100);
  end;

begin
  ADest.Open  := _NormPrice(AMax,AMin,ASource.Open);
  ADest.High  := _NormPrice(AMax,AMin,ASource.High);
  ADest.Low   := _NormPrice(AMax,AMin,ASource.Low);
  ADest.Close := _NormPrice(AMax,AMin,ASource.Close);
end;

procedure SetSaveBlock(const ABlock: TParamBlock);

  procedure AddSection(ASource: TStrings; ASection: String);
  begin
    ASource.Add(Format('[%s]',[ASection]));
  end;

  procedure AddValue(ASource: TStrings; AName, AValue: String);
  begin
    ASource.Add(Format('%s=%s',[AName,AValue]));
  end;

var
  xCandel: TCandel;
  xDest: TCandel;
  xParam: TStrings;
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + PathDelim +
    'block' + PathDelim +
    'block_' + ABlock.ID.ToString + '.ini';
  xParam := TStringList.Create;
  try
    AddSection(xParam,'systrem');
    AddValue(xParam,'id',ABlock.ID.ToString);

    AddSection(xParam,'left');
    AddValue(xParam,'max',ABlock.Left.MaxValue.ToString);
    AddValue(xParam,'min',ABlock.Left.MinValue.ToString);
    AddValue(xParam,'count',ABlock.Left.Count.ToString);

    AddSection(xParam,'right');
    AddValue(xParam,'max',ABlock.Right.MaxValue.ToString);
    AddValue(xParam,'min',ABlock.Right.MinValue.ToString);
    AddValue(xParam,'count',ABlock.Right.Count.ToString);

    AddSection(xParam,'sources');
    xDest := TCandel.Create;
    try
      for var i := 0 to ABlock.Count - 1 do
      begin
        xCandel := ABlock.Items[i];
        SetCandelNorm(
          ABlock.Right.MaxValue,
          ABlock.Right.MinValue,
          xCandel,
          xDest
        );
        AddValue(xParam,'c_' + i.ToString,xDest.ToString);
      end;
    finally
      FreeAndNil(xDest);
    end;




    AddSection(xParam,'params');
    AddValue(xParam,'top',ABlock.Top.ToString);
    AddValue(xParam,'bottom',ABlock.Bottom.ToString);
    AddValue(xParam,'delta',ABlock.Delta.ToString);
    AddValue(xParam,'coff_top',ABlock.CoffTop.ToString);
    AddValue(xParam,'coff_bottom',ABlock.CoffBottom.ToString);

    AddSection(xParam,'buy');
    AddValue(xParam,'tp',ABlock.BuyTP.ToString);
    AddValue(xParam,'sl',ABlock.BuySL.ToString);
    AddValue(xParam,'r',ABlock.BuyR.ToString);

    AddSection(xParam,'sell');
    AddValue(xParam,'tp',ABlock.SellTP.ToString);
    AddValue(xParam,'sl',ABlock.SellSL.ToString);
    AddValue(xParam,'r',ABlock.SellR.ToString);

    AddSection(xParam,'risk');
    AddValue(xParam,'r',ABlock.Risk.ToString);

    xParam.SaveToFile(xFileName);
  finally
    FreeAndNil(xParam);
  end;
end;



{ TCandel }

procedure TCandel.CopyCandel(const ACandel: TCandel);
begin
  Open := ACandel.Open;
  High := ACandel.High;
  Low  := ACandel.Low;
  Close:= ACandel.Close;
  Vol  := ACandel.Vol;
end;

function TCandel.ToString: String;
var
  xS: String;
begin
  xS :=
    Open.ToString + ';' +
    High.ToString + ';' +
    Low.ToString + ';' +
    Close.ToString + ';' +
    Vol.ToString;
  Result := xS;
end;

{ TCandelList }

function TCandelList.GetMaxPrice: Double;
var
  xV: Double;
begin
  xV := 0;
  for var xC in Self do
  begin
    if xC.High > xV then
      xV := xC.High;
  end;
  Result := xV;
end;

function TCandelList.GetMinPrice: Double;
var
  xV: Double;
begin
  xV := 0;
  if Count > 0 then
  begin
    var xC := Self.Items[0];
    xV := xC.Low;
    for var i := 1 to Count - 1 do
    begin
      xC := Self.Items[i];
      if xC.Low < xV then
        xV := xC.Low;
    end;
  end;
  Result := xV;
end;

{ TParam }

procedure TParam.SetTextExport(const AValue: String);

  procedure _Parser(const ASource: String; AParam: TStrings);
  var
    xValue: String;
    xParam: TStrings;
  begin
    AParam.Clear;
    if ASource.IsEmpty then
      Exit;
    xParam := TStringList.Create;
    try
      xValue := '';
      for var xC in ASource do
      begin
        case xC of
          ';': begin
            if not xValue.IsEmpty then
            begin
              AParam.Add(xValue);
              xValue := '';
            end;
          end;
        else
          xValue := xValue + xC;
        end;
      end;
      if not xValue.IsEmpty then
        AParam.Add(xValue);
    finally
      FreeAndNil(xParam);
    end;
  end;

  function _StrValueParam(const AIndex: Integer; AParam: TStrings): String;
  begin
    Result := '';
    if (AIndex >= 0) and (AIndex < AParam.Count) then
      Result := AParam[AIndex];
  end;

  function _StrToDate(const AValue: String): TDateTime;
  var
    xY, xM, xD: Word;
  begin
    xY := StrToIntDef(Copy(AValue,1,4),0);
    xM := StrToIntDef(Copy(AValue,5,2),0);
    xD := StrToIntDef(Copy(AValue,7,2),0);
    Result := EncodeDate(xY,xM,xD);
  end;

  function _StrToTime(const AValue: String): TDateTime;
  var
    xH, xM, xS, xZ: Word;
  begin
    xH := StrToIntDef(Copy(AValue,1,2),0);
    xM := StrToIntDef(Copy(AValue,3,2),0);
    xS := StrToIntDef(Copy(AValue,5,2),0);
    xZ := 0;
    Result := EncodeTime(xH,xM,xS,xZ);
  end;

var
  xSource: TStrings;
begin
  xSource := TStringList.Create;
  try
    _Parser(AValue, xSource);
    Date := _StrToDate(_StrValueParam(0,xSource));
    Time := _StrToTime(_StrValueParam(1,xSource));
    Open := StrToFloatDef(_StrValueParam(2,xSource),0);
    High := StrToFloatDef(_StrValueParam(3,xSource),0);
    Low  := StrToFloatDef(_StrValueParam(4,xSource),0);
    Close:= StrToFloatDef(_StrValueParam(5,xSource),0);
    Vol  := StrToFloatDef(_StrValueParam(6,xSource),0);
  finally
    FreeAndNil(xSource);
  end;
end;

function TParam.ToString: String;
begin
  Result :=
     'D: ' + DateToStr(Date) + '; ' +
     'T: ' + TimeToStr(Time) + '; ' +
     'O: ' + FloatToStr(Open) + '; ' +
     'H: ' + FloatToStr(High) + '; ' +
     'L: ' + FloatToStr(Low) + '; ' +
     'C: ' + FloatToStr(Close) + '; ' +
     'V: ' + FloatToStr(Vol);
end;

{ TParsingCandels }

constructor TParsingCandels.Create;
begin
  inherited;
  FParam := TParam.Create;
end;

destructor TParsingCandels.Destroy;
begin
  FreeAndNil(FParam);
  inherited;
end;

function TParsingCandels.GetParams(Index: Integer): TParam;
var
  xS: String;
begin
  xS := Strings[Index];
  FParam.SetTextExport(xS);
  Result := FParam;
end;

{ TBlock.TBlockParam }

procedure TBlock.TBlockParam.Clear;
begin
  MaxValue := 0;
  MinValue := 0;
  Count := 0;
end;

{ TBlock }

constructor TBlock.Create;
begin
  FSources := TCandelList.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FSources);
  inherited;
end;

procedure TBlock.SetMaxAndMinValue;
var
  xCandel: TCandel;
  i, iCount: Integer;
  xMaxValue, xMinValue: Double;
begin
  iCount := FSources.Count;
  if iCount > 0 then
  begin
    xCandel := FSources[0];
    xMaxValue := xCandel.High;
    xMinValue := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := FSources[i];
      if xCandel.High > xMaxValue then
        xMaxValue := xCandel.High;
      if xCandel.Low < xMinValue then
        xMinValue := xCandel.Low;
      if i = (FLeft.Count - 1) then
      begin
        FLeft.MaxValue := xMaxValue;
        FLeft.MinValue := xMinValue;
      end;
    end;
    FRight.MaxValue := xMaxValue;
    FRight.MinValue := xMinValue;
  end;
end;

function TBlock.IsFull: Boolean;
begin
  Result := (FLeft.Count + FRight.Count) = FSources.Count;
end;

procedure TBlock.SetCountParam(const ACountLeft, ACountRight: Integer);
begin
  FLeft.Clear;
  FRight.Clear;
  FLeft.Count := ACountLeft;
  FRight.Count := ACountRight;
  FSources.Clear;
end;

procedure TBlock.UpDataParam(const AParam: TParam);
var
  xCandel: TCandel;
begin
  if not IsFull then
  begin
    xCandel := TCandel.Create;
    xCandel.CopyCandel(TCandel(AParam));
    FSources.Add(xCandel);
  end;
end;

function TBlock.GetCount: Integer;
begin
  Result := FLeft.Count;
end;

function TBlock.GetItems(Index: Integer): TCandel;
begin
  if (Index >= 0) and (Index < FLeft.Count) then
    Result := FSources[Index]
  else
    raise Exception.Create('Error Message: Превышение индекса');
end;

{ TParamBlock }

function TParamBlock.GetDelta: Double;
begin
  Result := RoundTo(FLeft.MaxValue - FLeft.MinValue,-5);
end;

function TParamBlock.GetTop: Double;
begin
  Result := RoundTo(FRight.MaxValue - FLeft.MaxValue,-5);
end;

function TParamBlock.GetBottom: Double;
begin
  Result := RoundTo(FLeft.MinValue - FRight.MinValue,-5);
end;

function TParamBlock.GetCoffTop: Double;
var
  xDelta: Double;
begin
  Result := 0;
  xDelta := Self.Delta;
  if xDelta > 0 then
    Result := RoundTo(Self.Top/xDelta,-2);
end;

function TParamBlock.GetCoffBottom: Double;
var
  xDelta: Double;
begin
  Result := 0;
  xDelta := Self.Delta;
  if xDelta > 0 then
    Result := RoundTo(Self.Bottom/xDelta,-2);
end;

function TParamBlock.GetBuyTP: Double;
begin
  Result := RoundTo(FRight.MaxValue - FLeft.MaxValue,-2);
end;

function TParamBlock.GetBuySL: Double;
begin
  Result := RoundTo(FLeft.MaxValue - FRight.MinValue,-2);
end;

function TParamBlock.GetBuyR: Double;
var
  xSL: Double;
begin
  Result := 0;
  xSL := BuySL;
  if xSL > 0 then
    Result :=  RoundTo(BuyTP/xSL,-2);
end;

function TParamBlock.GetSellTP: Double;
begin
  Result := RoundTo(FLeft.MinValue - FRight.MinValue,-2);
end;

function TParamBlock.GetSellSL: Double;
begin
  Result := RoundTo(FRight.MaxValue - FLeft.MinValue,-2);
end;

function TParamBlock.GetSellR: Double;
var
  xSL: Double;
begin
  Result := 0;
  xSL := SellSL;
  if xSL > 0 then
    Result :=  RoundTo(SellTP/xSL,-2);
end;

function TParamBlock.GetRisk: Double;
var
  xBuyR, xSellR: Double;
begin
  xBuyR := BuyR;
  xSellR := SellR;
  Result := IfThen(xBuyR > xSellR,xBuyR,xSellR);
end;

function TBlock.GetCountLeft: Integer;
begin
  Result := FLeft.Count;
end;

function TBlock.GetCountRight: Integer;
begin
  Result := FRight.Count;
end;

end.
