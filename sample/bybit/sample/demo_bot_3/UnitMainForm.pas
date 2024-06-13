unit UnitMainForm;

interface

{$I demo_bot.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator,
  Lb.Bybit.InstrumentsInfo,
  Lb.Bybit.OrderBook,
  Lb.Bybit.Kline,
  Lb.OperationTrade,
  FMX.Objects,
  FMX.TabControl,
  UnitSecurityFrame,
  UnitOrderFrame,
  UnitGridOrdersFrame,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid;

type
  TMainForm = class(TForm)
    LayoutMenu: TLayout;
    Layout: TLayout;
    ButtonStartUpData: TButton;
    Timer: TTimer;
    LayoutClient: TLayout;
    MemoLog: TMemo;
    EditSymbol: TEdit;
    GridLayout: TGridPanelLayout;
    LayoutBuy: TLayout;
    LayoutSell: TLayout;
    TimerTradeUpdata: TTimer;
    ButtonStartTrade: TButton;
    GridPanelInfo: TGridPanelLayout;
    LayoutOrder: TLayout;
    procedure ButtonStartUpDataClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TimerTradeUpdataTimer(Sender: TObject);
    procedure ButtonStartTradeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLongRSI: Double;
    FMediumRSI: Double;
    FShortSRI: Double;
    FOperation: TTypeOperation;
  private
    FQuantity: Double;
    FEventCount: Integer;
    {$IFDEF HISTORY_INDICATOR}
    HistoryIndicator: THistoryIndicator;
    {$ENDIF}
    {$IFDEF INSTRUMENT_PRICE}
    InstrumentPrice: TInstrumentPrice;
    {$ENDIF}
    procedure SetStart;
    procedure SetStop;
    procedure SetOperationTrade;
    procedure EventResponse(ASander: TObject; ATypeObject: TTypeObject);
  protected
    OrderBuyFrame: TOrderFrame;
    OrderSellFrame: TOrderFrame;
    {$IFDEF VERTUAL_TRADE}
    OrdersFrame: TGridOrdersFrame;
    {$ENDIF}
  protected {Лог операций}
    procedure SetLog(S: String);
    procedure SetLogText(S: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  Lb.Logger,
  Lb.Params,
  Lb.Setting;

var
  ParamInstrumentInfo: TStringParams = nil;

function GetFileNamePath(const AStartTime: String): String;
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0)) + 'data\';
  Result := xPath + AStartTime + '.csv';
end;

{ TForm5 }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _SetAddColumn(AHeader: String; AGrid: TStringGrid);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(AGrid);
    xColumn.Header := AHeader;
    xColumn.Parent := AGrid;
  end;

begin
  inherited Create(AOwner);
  FQuantity := 0.01;
  FOperation := TTypeOperation.toNull;
  {$IFDEF HISTORY_INDICATOR}
  // Загужаем история
  HistoryIndicator := THistoryIndicator.Create;
  HistoryIndicator.OnResponse := EventResponse;
  {$ENDIF}
  {$IFDEF INSTRUMENT_PRICE}
  // Котеровальный стакан
  InstrumentPrice := TInstrumentPrice.Create;
  InstrumentPrice.OnResponse := EventResponse;
  {$ENDIF}
  // Параметры работы
  ParamInstrumentInfo := TStringParams.Create;

  OrderBuyFrame := TOrderFrame.Create(nil);
  OrderBuyFrame.Parent := LayoutBuy;
  OrderBuyFrame.Align := TAlignLayout.Client;

  OrderSellFrame := TOrderFrame.Create(nil);
  OrderSellFrame.Parent := LayoutSell;
  OrderSellFrame.Align := TAlignLayout.Client;

  {$IFDEF VERTUAL_TRADE}
  OrdersFrame := TGridOrdersFrame.Create(nil);
  OrdersFrame.Parent := LayoutOrder;
  OrdersFrame.Align := TAlignLayout.Client;
  {$ENDIF}
end;


destructor TMainForm.Destroy;
begin
  {$IFDEF VERTUAL_TRADE}
  FreeAndNil(OrdersFrame);
  {$ENDIF}
  FreeAndNil(OrderSellFrame);
  FreeAndNil(OrderBuyFrame);
  FreeAndNil(ParamInstrumentInfo);
  {$IFDEF HISTORY_INDICATOR}
  FreeAndNil(HistoryIndicator);
  {$ENDIF}
  {$IFDEF INSTRUMENT_PRICE}
  FreeAndNil(InstrumentPrice);
  {$ENDIF}
  inherited;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OrderBuyFrame.SetSave;
  OrderSellFrame.SetSave;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ParamInstrumentInfo.Load('instrument');
  {$IFDEF HISTORY_INDICATOR}
  HistoryIndicator.Symbol := ParamInstrumentInfo.AsString['symbol'];
  HistoryIndicator.Category := TTypeCategory.tcLinear;
  HistoryIndicator.Interval := TTypeInterval.ti_5;
  {$ENDIF}
  {$IFDEF INSTRUMENT_PRICE}
  InstrumentPrice.Symbol := ParamInstrumentInfo.AsString['symbol'];
  InstrumentPrice.Category := TTypeCategory.tcLinear;
  InstrumentPrice.Limit := 10;
  {$ENDIF}
  EditSymbol.Text := ParamInstrumentInfo.AsString['symbol'];

  OrderBuyFrame.Side := TTypeSide.tsBuy;
  OrderSellFrame.Side := TTypeSide.tsSell;

  OrderBuyFrame.SetLoad;
  OrderSellFrame.SetLoad;
end;

procedure TMainForm.SetStart;
begin

  OrderBuyFrame.Quantity := 0.01;
  OrderSellFrame.Quantity := 0.01;

  SetInitialization('','');
  ButtonStartUpData.Text := 'Стоп Об.';
  {$IFDEF HISTORY_INDICATOR}
  HistoryIndicator.UpDate;
  {$ENDIF}
  {$IFDEF INSTRUMENT_PRICE}
  InstrumentPrice.UpDate;
  {$ENDIF}
  Timer.Enabled := True;
end;

procedure TMainForm.SetStop;
begin
  ButtonStartUpData.Text := 'Старт Об.';
  Timer.Enabled := False;
end;

procedure TMainForm.ButtonStartTradeClick(Sender: TObject);
begin
  if TimerTradeUpdata.Enabled then
  begin
    TimerTradeUpdata.Enabled := False;
    ButtonStartTrade.Text := 'Старт Tr.';
  end
  else
  begin
    TimerTradeUpdata.Enabled := True;
    ButtonStartTrade.Text := 'Стоп Tr.';
  end;
end;

procedure TMainForm.ButtonStartUpDataClick(Sender: TObject);
begin
  if Timer.Enabled then
    SetStop
  else
    SetStart;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  FLongRSI   := 0;
  FMediumRSI := 0;
  FShortSRI  := 0;
  // Запрашиваем данные
  MemoLog.Lines.Clear;
  FEventCount := 0;
  {$IFDEF HISTORY_INDICATOR}
  HistoryIndicator.UpDate;
  {$ENDIF}
  {$IFDEF INSTRUMENT_PRICE}
  InstrumentPrice.UpDate;
  {$ENDIF}
end;


procedure TMainForm.SetLog(S: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:mm:ss.zzz',Time) + ' || ' + S;
  SetLogText(xS);
end;

procedure TMainForm.SetLogText(S: String);
begin
  MemoLog.Lines.Add(S);
end;

procedure TMainForm.EventResponse(ASander: TObject; ATypeObject: TTypeObject);
begin
  Inc(FEventCount);
  case ATypeObject of
    tobNullObject: begin
      SetLog('MainForm.EventResponse.NullObject [' + FEventCount.ToString + ']');
    end;
    tobHistoryIndicator: begin
      SetLog('MainForm.EventResponse: tobHistoryIndicator');
      {$IFDEF HISTORY_INDICATOR}
      FLongRSI   := HistoryIndicator.LongRSI.Current.AvgValue;
      FMediumRSI := HistoryIndicator.MediumRSI.Current.AvgValue;
      FShortSRI  := HistoryIndicator.ShortSRI.Current.AvgValue;
      {$ENDIF}
    end;
    tobInstrumentPrice: begin
      SetLog('MainForm.EventResponse: tobInstrumentPrice');
      {$IFDEF INSTRUMENT_PRICE}

      {$ENDIF}
    end;
  end;
  if FEventCount = 2 then
    SetOperationTrade;
  SetLog('  >>:' + FEventCount.ToString + ']');
end;

procedure TMainForm.SetOperationTrade;
begin
  {$IFDEF HISTORY_INDICATOR}
  SetLog('Шаг обновление Candel.Count = ' + HistoryIndicator.Candels.Count.ToString);
  {$ENDIF}

  SetLogText('  >> LongRSI  : ' + FLongRSI.ToString);
  SetLogText('  >> MediumRSI: ' + FMediumRSI.ToString);
  SetLogText('  >> ShortSRI : ' + FShortSRI.ToString);


  OrderBuyFrame.Symbol := EditSymbol.Text;
  //OrderBuyFrame.Side := TTypeSide.tsBuy;
  {$IFDEF INSTRUMENT_PRICE}
  SetLogText('  >> Ask : ' + InstrumentPrice.Ask.ToString);
  OrderBuyFrame.Price := InstrumentPrice.Ask;
  {$ENDIF}
  OrderSellFrame.Symbol := EditSymbol.Text;
  //OrderSellFrame.Side := TTypeSide.tsSell;
  {$IFDEF INSTRUMENT_PRICE}
  SetLogText('  >> Bid : ' + InstrumentPrice.Bid.ToString);
  OrderSellFrame.Price := InstrumentPrice.Bid;
  {$ENDIF}
end;

procedure TMainForm.TimerTradeUpdataTimer(Sender: TObject);
begin
  OrderBuyFrame.SetUpData(FMediumRSI);
  OrderSellFrame.SetUpData(FMediumRSI);
end;

end.
