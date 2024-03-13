unit Lb.Bybit.RecentTrade;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  Lb.Bybit.SysUtils;

type
  TRecentTradeObject = class;
  TRecentTradeObjectList = TObjectList<TRecentTradeObject>;

  TBybitRecentTrade = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FBaseCoin: String;
    FOptionType: TOptionType;
    FLimit: Integer;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetOptionType(const Value: TOptionType);
    procedure SetLimit(const Value: Integer);
  protected
    FListJson: TJSONArray;
    FRecentTrades: TRecentTradeObjectList;
    procedure DoEventParser; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    ///<summary>“ип продукта. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>»м€ символа</summary>
    property Symbol: String read FSymbol write SetSymbol;
    ///<summary>Ѕазова€ монета</summary>
    ///<remarks>
    /// применить только к опции
    /// если поле не введено, верните данные BTC по умолчанию
    ///</remarks>
    property BaseCoin: String read FBaseCoin write SetBaseCoin;
    property OptionType: TOptionType read FOptionType write SetOptionType;
    property Limit: Integer read FLimit write SetLimit;
    property RecentTrades: TRecentTradeObjectList read FRecentTrades;
  end;

  TRecentTradeObject = class(TCustonObjectJson)
  public
    ExecID: String;        // Execution ID
    Symbol: String;
    Price: Double;
    Size: Double;
    Side: TTypeSide;
    Time: Int64;
    IsBlockTrade: Boolean; // явл€етс€ ли сделка блочной сделкой
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;

procedure SetRecentTradeObjects(AListJson: TJSONArray; ARecentTradeObjects: TRecentTradeObjectList);

implementation

procedure SetRecentTradeObjects(AListJson: TJSONArray; ARecentTradeObjects: TRecentTradeObjectList);
var
  i, iCount: Integer;
  xObjectJson: TJSONObject;
  xRecentTradeObject: TRecentTradeObject;
begin
  if not Assigned(AListJson) then
  begin
    raise Exception.Create('Error Message: ќбъект JSON Ц не определен');
    Exit;
  end;

  if not Assigned(ARecentTradeObjects) then
  begin
    Exit;
  end;

  ARecentTradeObjects.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xRecentTradeObject := TRecentTradeObject.Create;
      xRecentTradeObject.SetObjectJson(xObjectJson);
      ARecentTradeObjects.Add(xRecentTradeObject);
    end;
end;

{ TBybitRecentTrade }

constructor TBybitRecentTrade.Create;
begin
  inherited Create;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/market/recent-trade';
  FRecentTrades := TRecentTradeObjectList.Create;
end;

destructor TBybitRecentTrade.Destroy;
begin
  FreeAndNil(FRecentTrades);
  inherited;
end;


procedure TBybitRecentTrade.DoEventParser;
var
  xValueJson: TJSONValue;
begin
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
  begin
    FListJson := TJSONArray(xValueJson);
    SetRecentTradeObjects(FListJson,FRecentTrades);
  end;
end;

procedure TBybitRecentTrade.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory))
end;

procedure TBybitRecentTrade.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitRecentTrade.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitRecentTrade.SetOptionType(const Value: TOptionType);
begin
  FOptionType := Value;
  BybitModule.Params.SetParam('optionType',GetStrToOptionType(FOptionType));
end;

procedure TBybitRecentTrade.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

{ TRecentTradeObject }

constructor TRecentTradeObject.Create;
begin
  inherited;
end;

destructor TRecentTradeObject.Destroy;
begin
  inherited;
end;

procedure TRecentTradeObject.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
  ExecID := GetStrToJson(AObjectJson.Values['execId']);
  Symbol := GetStrToJson(AObjectJson.Values['symbol']);
  Price  := GetFloatToJson(AObjectJson.Values['price']);
  Size   := GetFloatToJson(AObjectJson.Values['size']);
  Side   := GetTypeSideToStr(
    GetStrToJson(AObjectJson.Values['side'])
  );
  Time   := GetInt64ToJson(AObjectJson.Values['time']);
  IsBlockTrade := GetBoolToJson(AObjectJson.Values['isBlockTrade']);
end;

end.
