unit Lb.ParamPosition;

interface

{$I debug.inc}

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils;

type
  TTypeParamPositon = (
    tppNull    = 0,
    tppJournal = 1,
    tppTrades  = 2,
    tppCandels = 3,
    tppProfits = 4
  );

  TParamPosition = class;
  TParamTrade  = class;

  ///<summary>араметры свячи</summary>
  TParamCandel = class(TObject)
  public
    TradeID: Integer;
    CandelID: Integer;
    Time: Int64;
    Open: Double;
    High: Double;
    Low: Double;
    Close: Double;
    Vol: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
 TParamCandelList = TObjectList<TParamCandel>;

  ///<summary>Информация по сделке</summary>
  TParamTrade  = class(TObject)
  public
    ID: Integer;
    Time: TDateTime;
    Price: Double;
    Qty: Double;
    Side: TTypeBuySell;
  private
    FCandels: TParamCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Candels: TParamCandelList read FCandels;
  end;
  TParamTradeList = TObjectList<TParamTrade>;

  ///<summary>ПОлучаемый доход</summary>
  TParamProfit = class(TObject)
  public
    ID: Integer;
    Value: Double;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  TParamProfitList = TObjectList<TParamProfit>;

  ///<summary>
  /// Параметры позиции
  ///</summary>
  TParamPosition = class(TObject)
  public
    ID: Integer;
    IsActive: Boolean;
    Price: Double;
    Qty: Double;
    Side: TTypeBuySell;
    Profit: Double;
    MaxProfit: Double;
    MinProfit: Double;
  private
    FParamTrades: TParamTradeList;
    FParamProfits: TParamProfitList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Trades: TParamTradeList read FParamTrades;
    property Profits: TParamProfitList read FParamProfits;
  end;

///<summary>
/// Загрузка параметров - позиции
///</summary>
procedure SetLoadParamPosition(const ASource: TStrings; AParamPosition: TParamPosition);

implementation

procedure SetParserValueFields(const S: String; AValues: TStrings);
begin
  if not Assigned(AValues) then
    raise Exception.Create('Error Message: Ошибка парсинга значений полей');
  AValues.Clear;
  var tmpS := '';
  for var xC in S do
  begin
    if xC = ';' then
    begin
      if not tmpS.IsEmpty then
        AValues.Add(tmpS);
      tmpS := '';
    end
    else
      tmpS := tmpS + xC;
  end;
  if not tmpS.IsEmpty then
    AValues.Add(tmpS);
end;

procedure SetParserParamPosition(const S: String; AParamPosition: TParamPosition);
var
  xStr: TStrings;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';

  xStr := TStringList.Create;
  try
    SetParserValueFields(S, xStr);
    if xStr.Count >= 1 then
      AParamPosition.ID := xStr[0].ToInteger;
    if xStr.Count >= 2 then
      AParamPosition.IsActive := xStr[1].ToBoolean;
    if xStr.Count >= 3 then
      AParamPosition.Price := StrToFloat(xStr[2],xF);
    if xStr.Count >= 4 then
      AParamPosition.Qty := StrToFloat(xStr[3],xF);
    if xStr.Count >= 5 then
      AParamPosition.Side := GetSiseToStr(xStr[4]);
    if xStr.Count >= 6 then
      AParamPosition.Profit := StrToFloat(xStr[5],xF);
    if xStr.Count >= 7 then
      AParamPosition.MaxProfit := StrToFloat(xStr[6],xF);
    if xStr.Count >= 8 then
      AParamPosition.MinProfit := StrToFloat(xStr[7],xF);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure SetParserParamTrade(const S: String; AParamTrade: TParamTrade);
var
  xStr: TStrings;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';

  xStr := TStringList.Create;
  try
    SetParserValueFields(S, xStr);

    if xStr.Count >= 1 then
      AParamTrade.ID := xStr[0].ToInteger;
    if xStr.Count >= 2 then
      AParamTrade.Time := StrToDateTime(xStr[1]);
    if xStr.Count >= 3 then
      AParamTrade.Price := StrToFloat(xStr[2],xF);
    if xStr.Count >= 4 then
      AParamTrade.Qty := StrToFloat(xStr[3],xF);
    if xStr.Count >= 5 then
      AParamTrade.Side := GetSiseToStr(xStr[4]);


  finally
    FreeAndNil(xStr);
  end;
end;

procedure SetParserParamCandel(const S: String; AParamCandel: TParamCandel);
var
  xStr: TStrings;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';

  xStr := TStringList.Create;
  try
    SetParserValueFields(S, xStr);

    if xStr.Count >= 1 then
      AParamCandel.TradeID := xStr[0].ToInteger;
    if xStr.Count >= 2 then
      AParamCandel.CandelID := xStr[1].ToInteger;
    if xStr.Count >= 3 then
      AParamCandel.Time := xStr[2].ToInt64;
    if xStr.Count >= 4 then
      AParamCandel.Open := StrToFloat(xStr[3],xF);
    if xStr.Count >= 5 then
      AParamCandel.High := StrToFloat(xStr[4],xF);
    if xStr.Count >= 6 then
      AParamCandel.Low := StrToFloat(xStr[5],xF);
    if xStr.Count >= 7 then
      AParamCandel.Close := StrToFloat(xStr[6],xF);
    if xStr.Count >= 8 then
      AParamCandel.Vol := StrToFloat(xStr[7],xF);


  finally
    FreeAndNil(xStr);
  end;
end;


procedure SetParserParamProfit(const S: String; AParamProfit: TParamProfit);
var
  xStr: TStrings;
  xF: TFormatSettings;
begin
  xF := FormatSettings;
  xF.DecimalSeparator := '.';

  xStr := TStringList.Create;
  try
    SetParserValueFields(S, xStr);

    if xStr.Count >= 1 then
      AParamProfit.ID := xStr[0].ToInteger;
    if xStr.Count >= 2 then
      AParamProfit.Value := StrToFloat(xStr[1],xF);

  finally
    FreeAndNil(xStr);
  end;
end;

procedure SetLoadParamPosition(const ASource: TStrings; AParamPosition: TParamPosition);
var
  xS: String;
  xIndex: Integer;
  xParamPositon: TTypeParamPositon;
begin
  xParamPositon := TTypeParamPositon.tppNull;

  AParamPosition.Trades.Clear;
  AParamPosition.Profits.Clear;

  xIndex := 0;
  while xIndex < ASource.Count do
  begin

    xS := ASource[xIndex];
    if Pos('##',xS) >= 1 then
    begin
      Inc(xIndex);
      Continue;
    end;

    if SameText('[journal]',xS) then
    begin
      xParamPositon := TTypeParamPositon.tppJournal;
      Inc(xIndex,2);
      Continue;
    end;

    if SameText('[trades]',xS) then
    begin
      xParamPositon := TTypeParamPositon.tppTrades;
      Inc(xIndex,2);
      Continue;
    end;

    if SameText('[candels]',xS) then
    begin
      xParamPositon := TTypeParamPositon.tppCandels;
      Inc(xIndex,2);
      Continue;
    end;

    if SameText('[profits]',xS) then
    begin
      xParamPositon := TTypeParamPositon.tppProfits;
      Inc(xIndex,2);
      Continue;
    end;

    case xParamPositon of
      tppJournal: begin
        SetParserParamPosition(xS,AParamPosition);
      end;
      tppTrades: begin
        var xParamTrade := TParamTrade.Create;
        SetParserParamTrade(xS, xParamTrade);
        AParamPosition.Trades.Add(xParamTrade);
      end;
      tppCandels: begin
        var xParamCandel := TParamCandel.Create;
        SetParserParamCandel(xS,xParamCandel);
        var xParamTrade := AParamPosition.Trades.Items[xParamCandel.TradeID];
        xParamTrade.Candels.Add(xParamCandel);
      end;
      tppProfits: begin
        var xParamProfit := TParamProfit.Create;
        SetParserParamProfit(xS,xParamProfit);
        AParamPosition.Profits.Add(xParamProfit);
      end;
    end;

    Inc(xIndex);
    if xIndex >= ASource.Count then
      Break;
  end;
end;

{ TParamCandel }

constructor TParamCandel.Create;
begin
  TradeID := 0;
  CandelID := 0;
  Time := 0;
  Open := 0;
  High := 0;
  Low := 0;
  Close := 0;
  Vol := 0;
end;

destructor TParamCandel.Destroy;
begin

  inherited;
end;

{ TParamTrade }

constructor TParamTrade.Create;
begin
  ID := 0;
  Time := 0;
  Price := 0;
  Qty := 0;
  Side := TTypeBuySell.tsNull;
  FCandels := TParamCandelList.Create;
end;

destructor TParamTrade.Destroy;
begin
  FreeAndNil(FCandels);
  inherited;
end;

{ TParamProfit }

constructor TParamProfit.Create;
begin
  ID := 0;
  Value := 0;
end;

destructor TParamProfit.Destroy;
begin

  inherited;
end;

{ TParamPosition }

constructor TParamPosition.Create;
begin
  ID := -1;
  IsActive := False;
  Price := 0;
  Qty := 0;
  Side := TTypeBuySell.tsNull;
  Profit := 0;
  MaxProfit := 0;
  MinProfit := 0;

  FParamTrades := TParamTradeList.Create;
  FParamProfits:= TParamProfitList.Create;
end;

destructor TParamPosition.Destroy;
begin
  FreeAndNil(FParamProfits);
  FreeAndNil(FParamTrades);
  inherited;
end;

end.
