unit Lb.Status.Bybit;

interface

{$i platform.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils,
  Lb.Bybit.Trade,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator.Bybit,
  Lb.Bybit.Position,
  Lb.Status;

type
  ///<summary>
  /// Отслеживание состояние рынка — торговый терминал Quik
  ///</summary>
  TBybitStatus = class(TCustomStatus)
  private
    FStartTime: String;
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
    function GetOperationTrade(AParamStatus: TParamStatus): String; override;
  public
    property HistoryIndicator: THistoryIndicator read FHistoryIndicator;
    property InstrumentPrice: TInstrumentPrice read FInstrumentPrice;
    property BybitPosition: TBybitPosition read FBybitPosition;
  end;

implementation

uses
  Lb.Logger;

{ TBybitStatus }

constructor TBybitStatus.Create;
begin
  inherited;
  FTypePlatform := TTypePlatform.tpBybit;

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
  FStartTime := '';
  // Настройки по дефолту
  FHistoryIndicator.Symbol   := ParamPlatform.Symble;
  FHistoryIndicator.Category := ParamPlatform.Category;
  FHistoryIndicator.Interval := ParamPlatform.Interval;
  FHistoryIndicator.UpDate;

  FInstrumentPrice.Symbol   := ParamPlatform.Symble;
  FInstrumentPrice.Category := ParamPlatform.Category;
  FInstrumentPrice.Limit    := 5;

  FBybitPosition.Symbol := ParamPlatform.Symble;
  FBybitPosition.Category := ParamPlatform.Category;
  FBybitPosition.SetEncryption(
    ParamPlatform.ApiKey,
    ParamPlatform.ApiSecret
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
var
  xCurrentTime: String;
begin
  IsNewCandel := False;
  {$IFDEF DBG_BYBIT_STATUS}
  TLogger.LogText('*',80);
  TLogger.LogTree(0,'procedure TBybitStatus.EventResponse:');
  {$ENDIF}
  case ATypeObject of
    tobHistoryIndicator: begin
      {$IFDEF DBG_BYBIT_STATUS}
      TLogger.LogTreeText(3,'>> tobHistoryIndicator');
      {$ENDIF}
      if Assigned(FHistoryIndicator.CurrentCandel) then
      begin
        xCurrentTime := FHistoryIndicator.CurrentCandel.startTime;
        {$IFDEF DBG_BYBIT_STATUS}
        TLogger.LogTreeText(3,'>> CrtTime: ' + xCurrentTime);
        TLogger.LogTreeText(3,'>> StrTime: ' + FStartTime);
        {$ENDIF}
        if FStartTime.IsEmpty then
        begin
          FStartTime := xCurrentTime;
        end
        else if not SameText(FStartTime,xCurrentTime) then
        begin
          {$IFDEF DBG_BYBIT_STATUS}
          TLogger.LogTreeText(3,'>> NEW_CANDEL');
          {$ENDIF}
          FStartTime := xCurrentTime;
          Self.DoNewCandel;
        end;
      end;
      FastRSI := FHistoryIndicator.FastRSI.Current.Value;
      SlowRSI := FHistoryIndicator.SlowRSI.Current.Value;
    end;
    tobInstrumentPrice: begin
      {$IFDEF DBG_BYBIT_STATUS}
      TLogger.LogTreeText(3,'>> tobInstrumentPrice');
      {$ENDIF}
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
  if ParamPlatform.IsVirtualChecked then
    Exit;

  Position.Qty := 0;
  if FBybitPosition.PositionObjects.Count > 0 then
  begin
    xS := FBybitPosition.PositionObjects[0].Size;
    if not xS.IsEmpty then
    begin
      xF := FormatSettings;
      xF.DecimalSeparator := '.';
      Position.Qty := StrToFloatDef(xS,0,xF);
    end;
    if Position.Qty > 0 then
      Position.Side := GetTypeSideToStr(FBybitPosition.PositionObjects[0].Side);
  end;
  DoUpDate;
end;

function TBybitStatus.GetOperationTrade(AParamStatus: TParamStatus): String;

  function _Qty(const AQty: Double): Double;
  begin
    Result := AQty;
  end;

var
  xPlaceOrder: TParamOrder;
  xResponse: TOrderResponse;
begin
  Result := inherited GetOperationTrade(AParamStatus);
  if not ParamPlatform.IsVirtualChecked then
  begin
    try
      // Инструмент отслеживания
      // Передача ключей программе
      xPlaceOrder := TParamOrder.Create;
      try
        xPlaceOrder.TypeProc    := TParamOrder.TTypeProc.Place;

        {todo: сохранение данных}
        xPlaceOrder.Category    := TTypeCategory.tcLinear;
        xPlaceOrder.Symbol      := ParamPlatform.Symble;
        xPlaceOrder.Side        := AParamStatus.Side;
        xPlaceOrder.PositionIdx := 0;
        xPlaceOrder.OrderType   := TTypeOrder.Limit;
        xPlaceOrder.Qty         := _Qty(AParamStatus.Qty);
        xPlaceOrder.Price       := AParamStatus.Price;
        xPlaceOrder.timeInForce := TTypeTimeInForce.GTC;
        xPlaceOrder.OrderLinkId := CreateOrderLinkId(AParamStatus.Side,AParamStatus.Line);

        xResponse := TOrderResponse.Create;
        try
          if not ParamPlatform.IsVirtualChecked then
          begin
            // Реальные торговые операции
            SelectedOrder(
               ParamPlatform.ApiKey,
               ParamPlatform.ApiSecret,
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
