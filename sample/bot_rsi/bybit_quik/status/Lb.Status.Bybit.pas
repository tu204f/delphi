unit Lb.Status.Bybit;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.Bybit.Trade,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator,
  Lb.Bybit.Position,
  Lb.Status;

type
  ///<summary>
  /// Отслеживание состояние рынка — торговый терминал Quik
  ///</summary>
  TBybitStatus = class(TCustomStatus)
  private
    FHistoryIndicator: THistoryIndicator;
    FInstrumentPrice: TInstrumentPrice;
    FBybitPosition: TBybitPosition;
    procedure EventResponse(ASander: TObject; ATypeObject: TTypeObject);
    procedure EventBybitPositionEndLoading(Sender: TObject);
  protected
    procedure DoStart; override;
    procedure DoSelected; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetOperationTrade(ASide: TQBTypeSide;
      APrice: Double; AQty: Double; ALine: TTypeLine): String; override;
  end;

implementation

{ TBybitStatus }

constructor TBybitStatus.Create;
begin
  inherited;
  FHistoryIndicator := THistoryIndicator.Create;
  FHistoryIndicator.OnResponse := EventResponse;

  FInstrumentPrice := TInstrumentPrice.Create;
  FInstrumentPrice.OnResponse := EventResponse;

  FBybitPosition := TBybitPosition.Create;
  FBybitPosition.OnEventEndLoading := EventBybitPositionEndLoading;
end;

destructor TBybitStatus.Destroy;
begin
  FreeAndNil(FBybitPosition);
  FreeAndNil(FInstrumentPrice);
  FreeAndNil(FHistoryIndicator);
  inherited;
end;


procedure TBybitStatus.DoStart;
begin
  inherited;
  // Настройки по дефолту
  ParamApplication.Interval := TTypeInterval.ti_5;

  FHistoryIndicator.Symbol   := ParamApplication.Symble;
  FHistoryIndicator.Category := ParamApplication.Category;
  FHistoryIndicator.Interval := ParamApplication.Interval;
  FHistoryIndicator.UpDate;

  FInstrumentPrice.Symbol   := ParamApplication.Symble;
  FInstrumentPrice.Category := ParamApplication.Category;
  FInstrumentPrice.Limit    := 10;

  FBybitPosition.Symbol := ParamApplication.Symble;
  FBybitPosition.Category := ParamApplication.Category;
  FBybitPosition.SetEncryption(
    ParamApplication.ApiKey,
    ParamApplication.ApiSecret
  );
end;

procedure TBybitStatus.DoSelected;
begin
  inherited DoSelected;
  FHistoryIndicator.UpDate;
  FInstrumentPrice.UpDate;
  FBybitPosition.Selected;
end;

procedure TBybitStatus.EventResponse(ASander: TObject; ATypeObject: TTypeObject);
begin
  case ATypeObject of
    tobHistoryIndicator: begin
      FastRSI := FHistoryIndicator.FastRSI.Current.AvgValue;
      SlowRSI := FHistoryIndicator.SlowRSI.Current.AvgValue;
    end;
    tobInstrumentPrice: begin
      Bid := FInstrumentPrice.Bid;
      Ask := FInstrumentPrice.Ask;
    end;
  end;
  if (FastRSI > 0) and (SlowRSI > 0) and (Ask > 0) and (Bid > 0) then
    DoParams;
  DoUpDate;
end;

procedure TBybitStatus.EventBybitPositionEndLoading(Sender: TObject);
var
  xF: TFormatSettings;
  xS: String;
begin
  if ParamApplication.IsVirtualChecked then 
    Exit;

  Qty := 0;
  if FBybitPosition.PositionObjects.Count > 0 then
  begin
    xS := FBybitPosition.PositionObjects[0].Size;
    if not xS.IsEmpty then
    begin
      xF := FormatSettings;
      xF.DecimalSeparator := '.';
      Qty := StrToFloatDef(xS,0,xF);
    end;
    if Qty > 0 then
      Side := GetTypeSideToStr(FBybitPosition.PositionObjects[0].Side);
  end;
  DoUpDate;
end;

function TBybitStatus.GetOperationTrade(ASide: TQBTypeSide; APrice,
  AQty: Double; ALine: TTypeLine): String;

  function _Qty(const AQty: Double): Double;
  begin
    Result := AQty;
  end;

var
  xPlaceOrder: TParamOrder;
  xResponse: TOrderResponse;
begin
  Result := inherited GetOperationTrade(ASide, APrice, AQty, ALine);
  if not ParamApplication.IsVirtualChecked then
  begin
    try
      // Инструмент отслеживания
      // Передача ключей программе
      xPlaceOrder := TParamOrder.Create;
      try
        xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

        {todo: сохранение данных}
        xPlaceOrder.Category    := TTypeCategory.tcLinear;
        xPlaceOrder.Symbol      := ParamApplication.Symble;
        xPlaceOrder.Side        := ASide;
        xPlaceOrder.PositionIdx := 0;
        xPlaceOrder.OrderType   := TTypeOrder.Limit;
        xPlaceOrder.Qty         := _Qty(AQty);
        xPlaceOrder.Price       := APrice;
        xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
        xPlaceOrder.OrderLinkId := CreateOrderLinkId(ASide,ALine);

        xResponse := TOrderResponse.Create;
        try
          if not ParamApplication.IsVirtualChecked then
          begin
            // Реальные торговые операции
            SelectedOrder(
               ParamApplication.ApiKey,
               ParamApplication.ApiSecret,
               xPlaceOrder,
               xResponse // Возрат сообщение ос делке
            );
          end;
          DoInfoMsg(xResponse.RetMsg);
        finally
          FreeAndNil(xResponse);
        end;

      finally
        FreeAndNil(xPlaceOrder);
      end;
    except
      on E: Exception do
        raise Exception.Create('Error Message:' + E.Message);
    end;
  end;
end;

end.
