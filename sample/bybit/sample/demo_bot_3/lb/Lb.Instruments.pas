unit Lb.Instruments;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

type
  ///<summary>
  /// Придельные значение объема
  ///</summary>
  TLeverageFilter = class(TObject)
    MinLeverage: Double;             // 9
    MaxLeverage: Double;             // 10
    LeverageStep: Double;            // 11
  end;

  ///<summary>
  /// Придельное значение цены
  ///</summary>
  TPriceFilter = class(TObject)
    MinPrice: Double;                // 12
    MaxPrice: Double;                // 13
    TickSize: Double;                // 14
  end;

  ///<summary>
  /// Размер количество заявки
  ///</summary>
  TLotSizeFilter = class(TObject)
    MaxOrderQty: Double;             // 15
    MinOrderQty: Double;             // 16
    QtyStep: Double;                 // 17
    PostOnlyMaxOrderQty: Double;     // 18
  end;

  ///<summary>
  /// Информации инструмента
  ///</summary>
  TInfo = class(TObject)
  public
    Symbol: String;                  // 0
    ContractType: String;            // 1
    Status: String;                  // 2
    BaseCoin: String;                // 3
    QuoteCoin: String;               // 4
    LaunchTime: Int64;               // 5
    DeliveryTime: Int64;             // 6
    DeliveryFeeRate: String;         // 7
    PriceScale: Integer;             // 8
  public
    LeverageFilter: TLeverageFilter;
    PriceFilter: TPriceFilter;
    LotSizeFilter: TLotSizeFilter;
  public
    UnifiedMarginTrade: Boolean;     // 19
    FundingInterval: Integer;        // 20
    SettleCoin: String;              // 21
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetValue(const AValue: String);
  end;
  TInfoList = TObjectList<TInfo>;

  ///<summary>
  /// Инструмент — загрузки из instrument.csv
  ///</sumamry>
  TInstruments = class(TObject)
  private
    FInfos: TInfoList;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load(AFileName: String);
    property Items: TInfoList read FInfos;
    property Count: Integer read GetCount;
  end;

implementation

{ TInfo }

constructor TInfo.Create;
begin
  LeverageFilter := TLeverageFilter.Create;
  PriceFilter := TPriceFilter.Create;
  LotSizeFilter := TLotSizeFilter.Create;
end;

destructor TInfo.Destroy;
begin
  FreeAndNil(LotSizeFilter);
  FreeAndNil(PriceFilter);
  FreeAndNil(LeverageFilter);
  inherited;
end;

procedure TInfo.SetValue(const AValue: String);

  function _GetStrToStr(ASource: TStrings; AIndex: Integer): String;
  begin
    Result := '';
    if (AIndex >= 0) and (ASource.Count > AIndex) then
      Result := ASource[AIndex];
  end;

  function _GetStrToInt(ASource: TStrings; AIndex: Integer): Integer;
  begin
    Result := StrToIntDef(_GetStrToStr(ASource,AIndex),0);
  end;

  function _GetStrToInt64(ASource: TStrings; AIndex: Integer): Int64;
  begin
    Result := StrToInt64Def(_GetStrToStr(ASource,AIndex),0);
  end;

  function _GetStrToFloat(ASource: TStrings; AIndex: Integer): Double;
  begin
    Result := StrToFloatDef(_GetStrToStr(ASource,AIndex),0);
  end;

  function _GetStrToBool(ASource: TStrings; AIndex: Integer): Boolean;
  var
    xS: String;
  begin
    Result := False;
    xS := _GetStrToStr(ASource,AIndex);
    if not xS.IsEmpty then
    begin
      case xS[1] of
        't': Result := True;
        'f': Result := False;
      else
        Result := False;
      end;
    end;
  end;

  procedure _LeverageFilter(ASource: TStrings);
  begin
    LeverageFilter.MinLeverage  := _GetStrToFloat(ASource,9);
    LeverageFilter.MaxLeverage  := _GetStrToFloat(ASource,10);
    LeverageFilter.LeverageStep := _GetStrToFloat(ASource,11);
  end;

  procedure _PriceFilter(ASource: TStrings);
  begin
    PriceFilter.MinPrice := _GetStrToFloat(ASource,12);
    PriceFilter.MaxPrice := _GetStrToFloat(ASource,13);
    PriceFilter.TickSize := _GetStrToFloat(ASource,14);
  end;

  procedure _LotSizeFilter(ASource: TStrings);
  begin
    LotSizeFilter.MaxOrderQty := _GetStrToFloat(ASource,15);
    LotSizeFilter.MinOrderQty := _GetStrToFloat(ASource,16);
    LotSizeFilter.QtyStep     := _GetStrToFloat(ASource,17);
    LotSizeFilter.PostOnlyMaxOrderQty := _GetStrToFloat(ASource,18);
  end;


var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xStr.Delimiter := ';';
    xStr.DelimitedText := AValue;

    Symbol          := _GetStrToStr(xStr,0);
    ContractType    := _GetStrToStr(xStr,1);
    Status          := _GetStrToStr(xStr,2);
    BaseCoin        := _GetStrToStr(xStr,3);
    QuoteCoin       := _GetStrToStr(xStr,4);
    LaunchTime      := _GetStrToInt64(xStr,5);
    DeliveryTime    := _GetStrToInt64(xStr,6);
    DeliveryFeeRate := _GetStrToStr(xStr,7);
    PriceScale      := _GetStrToInt64(xStr,8);

    _LeverageFilter(xStr);
    _PriceFilter(xStr);
    _LotSizeFilter(xStr);

    UnifiedMarginTrade := _GetStrToBool(xStr,19);
    FundingInterval := _GetStrToInt(xStr,20);
    SettleCoin := _GetStrToStr(xStr,21);
  finally
    FreeAndNil(xStr);
  end;
end;

{ TInstruments }

constructor TInstruments.Create;
begin
  FInfos := TInfoList.Create;
end;

destructor TInstruments.Destroy;
begin
  FreeAndNil(FInfos);
  inherited;
end;

function TInstruments.GetCount: Integer;
begin
  Result := FInfos.Count;
end;

procedure TInstruments.Load(AFileName: String);
var
  xS: String;
  xStr: TStrings;
  xInfo: TInfo;
begin
  FInfos.Clear;
  xStr := TStringList.Create;
  try
    xStr.LoadFromFile(AFileName);
    for xS in xStr do
    begin
      xInfo := TInfo.Create;
      xInfo.SetValue(xS);
      FInfos.Add(xInfo);
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

end.
