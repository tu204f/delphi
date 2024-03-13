unit Lb.Bybit.Position;

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
  TPositionObject = class;

  ///<summary>������ ������</summary>
  TPositionObjectList = TObjectList<TPositionObject>;

  TBybitPosition = class(TBybitHttpClient)
  private
    FCategory: TTypeCategory;
    FSymbol: String;
    FBaseCoin: String;
    FSettleCoin: String;
    FLimit: Integer;
    FCursor: String;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetBaseCoin(const Value: String);
    procedure SetSettleCoin(const Value: String);
    procedure SetLimit(const Value: Integer);
    procedure SetCursor(const Value: String);
  protected
    FPositionObjects: TPositionObjectList;
    procedure DoEventMessage(const AMessage: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property PositionObjects: TPositionObjectList read FPositionObjects;
  public {Request Parameters}
    ///<summary>��� ��������. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>��� �������</summary>
    property Symbol: String read FSymbol write SetSymbol;
    ///<summary>������� ������. linear, inverse, option ������</summary>
    ///<remarks>� �������� ����� �� ���������� BTC �� ���������.</remarks>
    property BaseCoin: String read FBaseCoin write SetBaseCoin;
    ///<summary>
    ///Settle coin. For linear, either symbol or settleCoin
    ///is required. symbol has a higher priority
    ///</summary>
    property SettleCoin: String read FSettleCoin write SetSettleCoin;
    ///<summary>Limit for data size per page. [1, 1000]. Default: 500</summary>
    property Limit: Integer read FLimit write SetLimit;
    property Cursor: String read FCursor write SetCursor;
  end;

  TPositionObject = class(TCustonObjectJson)
  public
    ///<summary>
    ///������������� �������, ������������ ��� ������������� �������
    ///� ��������� ����������� �������
    ///</summary>
    PositionIdx: Integer;
    ///<summary>
    ///������������� ������ �����. ����������: ��� ������ ����� ��������
    ///��� ���� ���������� �������� 0, ��� ��������,
    ///��� ������� ����������� ����� ���������������
    ///</summary>
    RiskId: Integer;
    ///<summary>
    ///�������� ������� �����. ����������: ��� ������ �����
    ///�������� ��� ���� ���������� �������� 0, ��� ��������,
    ///��� ������� ����������� ����� ���������������
    ///</summary>
    RiskLimitValue: String;
    ///<summary>�������� �������</summary>
    Symbol: String;
    ///<summary>Position side. Buy: long, Sell: short</summary>
    Side: String;
    ///<summary>Position size</summary>
    Size: String;
    ///<summary>Average entry price</summary>
    AvgPrice: String;
    ///<summary>Position value</summary>
    PositionValue: String;
    ///<summary>Trade mode</summary>
    TradeMode: Integer;
    ///<summary>������� �� ������������� ��������� �����</summary>
    AutoAddMargin: Integer;
    ///<summary>������ ���������</summary>
    ///<remarks>
    /// Normal
    /// Liq - � �������� ����������
    /// Adl - � �������� ��������������� ���������� ���� ������� �������
    ///</remarks>
    PositionStatus: String;
    ///<summary>��������� ����� �������. ������������� ��� ���������</summary>
    Leverage: String;
    ///<summary>���� ��������� �������</summary>
    MarkPrice: String;
    ///<summary>���� ���������� �������</summary>
    ///<remarks>
    ///������ ��� ���� ����� ��� ������ ����� ��������, �
    ///�������������� ���� ������� �� �����
    ///</remarks>
    LiqPrice: String;
    ///<summary>���� �����������</summary>
    BustPrice: String;
    ///<summary>��������� �����. � ������ ����� �������� ������������ �������� ""</summary>
    PositionIM: String;
    ///<summary>�������������� �����. � ������ ����� �������� ������������ �������� ""</summary>
    PositionMM: String;
    ///<summary>����� �������. � ������ ����� �������� ������������ �������� ""</summary>
    PositionBalance: String;
    ///<summary>��������������, ����� �� ����� ������, ������ "���������". Spot �� ���������� ��� ����. �������� ���������� ""</summary>
    TpslMode: String;
    ///<summary>���� ����-�������</summary>
    TakeProfit: String;
    ///<summary>���� ����-�����</summary>
    StopLoss: String;
    ///<summary>��������-���� (���������� �� �������� ����)</summary>
    TrailingStop: String;
    ///<summary>Unrealised PnL</summary>
    UnrealisedPnl: String;
    ///<summary>Cumulative realised pnl</summary>
    CumRealisedPnl: String;
    ///<summary>��������� ����� ��������������� �������� ���� ������� �������.</summary>
    AdlRankIndicator: Integer;
    ///<summary>�������, ����� Bybit ������� ����� �����</summary>
    IsReduceOnly: Boolean;
    ///<summary>�������, ����� Bybit ������� ����� �����</summary>
    mmrSysUpdatedTime: String;
    ///<summary>�������, ����� Bybit ������� ����� �����</summary>
    leverageSysUpdatedTime: String;
    createdTime: String;
    updatedTime: String;
    Seq: UInt64;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetObjectJson(const AObjectJson: TJSONObject); override;
  end;

implementation

procedure SetPositionObjects(AListJson: TJSONArray; APositionObjects: TPositionObjectList);
var
  i, iCount: Integer;
  //xListJson: TJSONArray;
  xObjectJson: TJSONObject;
  xPositionObject: TPositionObject;
begin
  if not Assigned(APositionObjects) then
    Exit;
  APositionObjects.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONObject(AListJson.Items[i]);
      xPositionObject := TPositionObject.Create;
      xPositionObject.SetObjectJson(xObjectJson);
      APositionObjects.Add(xPositionObject);
    end;
end;

{ TBybitPosition }

constructor TBybitPosition.Create;
begin
  inherited Create;
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/position/list';
  FPositionObjects := TPositionObjectList.Create;
end;

destructor TBybitPosition.Destroy;
begin
  FreeAndNil(FPositionObjects);
  inherited;
end;

procedure TBybitPosition.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory));
end;

procedure TBybitPosition.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;

procedure TBybitPosition.SetBaseCoin(const Value: String);
begin
  FBaseCoin := Value;
  BybitModule.Params.SetParam('baseCoin',FBaseCoin);
end;

procedure TBybitPosition.SetSettleCoin(const Value: String);
begin
  FSettleCoin := Value;
  BybitModule.Params.SetParam('settleCoin',FSettleCoin);
end;

procedure TBybitPosition.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitPosition.SetCursor(const Value: String);
begin
  FCursor := Value;
  BybitModule.Params.SetParam('cursor',FCursor);
end;

procedure TBybitPosition.DoEventMessage(const AMessage: String);
var
  xListJson: TJSONArray;
  xValueJson: TJSONValue;
begin
  inherited DoEventMessage(AMessage);
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
  begin
    xListJson := TJSONArray(xValueJson);
    SetPositionObjects(xListJson,FPositionObjects);
  end;
end;

{ TPositionObject }

constructor TPositionObject.Create;
begin
  inherited;

end;

destructor TPositionObject.Destroy;
begin

  inherited;
end;

procedure TPositionObject.SetObjectJson(const AObjectJson: TJSONObject);
begin
  inherited;
  PositionIdx            := AObjectJson.Values['positionIdx'].Value.ToInteger;
  RiskId                 := AObjectJson.Values['riskId'].Value.ToInteger;
  RiskLimitValue         := AObjectJson.Values['riskLimitValue'].Value;
  Symbol                 := AObjectJson.Values['symbol'].Value;
  Side                   := AObjectJson.Values['side'].Value;
  Size                   := AObjectJson.Values['size'].Value;
  avgPrice               := AObjectJson.Values['avgPrice'].Value;
  positionValue          := AObjectJson.Values['positionValue'].Value;
  tradeMode              := AObjectJson.Values['tradeMode'].Value.ToInteger;
  autoAddMargin          := AObjectJson.Values['autoAddMargin'].Value.ToInteger;
  positionStatus         := AObjectJson.Values['positionStatus'].Value;
  leverage               := AObjectJson.Values['leverage'].Value;
  markPrice              := AObjectJson.Values['markPrice'].Value;
  liqPrice               := AObjectJson.Values['liqPrice'].Value;
  bustPrice              := AObjectJson.Values['bustPrice'].Value;
  positionIM             := AObjectJson.Values['positionIM'].Value;
  positionMM             := AObjectJson.Values['positionMM'].Value;
  positionBalance        := AObjectJson.Values['positionBalance'].Value;
  tpslMode               := AObjectJson.Values['tpslMode'].Value;
  takeProfit             := AObjectJson.Values['takeProfit'].Value;
  stopLoss               := AObjectJson.Values['stopLoss'].Value;
  trailingStop           := AObjectJson.Values['trailingStop'].Value;
  unrealisedPnl          := AObjectJson.Values['unrealisedPnl'].Value;
  cumRealisedPnl         := AObjectJson.Values['cumRealisedPnl'].Value;
  adlRankIndicator       := AObjectJson.Values['adlRankIndicator'].Value.ToInteger;
  isReduceOnly           := AObjectJson.Values['isReduceOnly'].Value.ToBoolean;
  mmrSysUpdatedTime      := AObjectJson.Values['mmrSysUpdatedTime'].Value;
  leverageSysUpdatedTime := AObjectJson.Values['leverageSysUpdatedTime'].Value;
  createdTime            := AObjectJson.Values['createdTime'].Value;
  updatedTime            := AObjectJson.Values['updatedTime'].Value;
  seq                    := AObjectJson.Values['seq'].Value.ToInteger;
end;

end.
