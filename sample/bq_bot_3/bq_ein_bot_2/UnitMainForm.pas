unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,

  Lb.Indicator,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Bybit.SysUtils,
  Lb.Platform.Bybit,
  Lb.Journal.Trading.v2,
  Lb.Bot.V2,

  FMX.Objects,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Menus,
  FMX.Edit, FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart,
  FMX.ListBox, FMX.Memo.Types, FMX.Memo;

type
  TMainForm = class(TForm)
    ButtonStartOrStop: TButton;
    Rectangle: TRectangle;
    TextStatus: TText;
    Layout: TLayout;
    GridLayout: TGridPanelLayout;
    ButtonBuy: TButton;
    ButtonSell: TButton;
    ButtonClose: TButton;
    TabControl1: TTabControl;
    TabItemTrade: TTabItem;
    TabItemPosition: TTabItem;
    PopupMenu: TPopupMenu;
    MenuItemSaveFile: TMenuItem;
    Chart: TChart;
    SeriesValueWR: TLineSeries;
    SeriesValueMa2: TLineSeries;
    SeriesValueMa1: TLineSeries;
    LayoutMarket: TLayout;
    RectangleMarket: TRectangle;
    GridPanelPrice: TGridPanelLayout;
    Text1: TText;
    EditWR: TEdit;
    EditFastWR: TEdit;
    EditValueWR: TEdit;
    LayoutTrend: TLayout;
    RectangleTrend: TRectangle;
    Text2: TText;
    ListBox: TListBox;
    Layout1: TLayout;
    LayoutDemo: TLayout;
    EditDemo1: TEdit;
    EditDemo2: TEdit;
    EditDemo3: TEdit;
    DemoGrid: TStringGrid;
    StringGridCandel: TStringGrid;
    GridPanelLayout1: TGridPanelLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    TabItemInfoMsg: TTabItem;
    MemoInfoMsg: TMemo;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure TradingPlatformOnNewCandel(Sender: TObject);
    procedure TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure Strategy;
    procedure PositionClose(ASander: TObject);
  protected
    procedure WorkBotOnLongOpen(Sender: TObject);
    procedure WorkBotOnShortOpen(Sender: TObject);
  public
    WorkBotDeviation: TWorkBotDeviation;
    TypeDirection: TTypeDirection;
    DemoJournalManager: TJournalManager;
    TradingPlatform: TTradingPlatform;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
{$IFDEF DEBUG}
  Lb.Logger,
{$ENDIF}
  System.DateUtils;

constructor TMainForm.Create(AOwner: TComponent);

  procedure SetAddColumn(const AStrGrid: TStringGrid; const AHeader: String; const AWidth: Single = 80);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
    xCol.Width  := AWidth;
  end;

begin
  inherited;

  SetAddColumn(StringGridCandel,'Time',150);
  SetAddColumn(StringGridCandel,'Open');
  SetAddColumn(StringGridCandel,'High');
  SetAddColumn(StringGridCandel,'Low');
  SetAddColumn(StringGridCandel,'Close');
  SetAddColumn(StringGridCandel,'Vol');
  SetAddColumn(StringGridCandel,'WR');
  SetAddColumn(StringGridCandel,'MaRSI');
  SetAddColumn(StringGridCandel,'Ma2RSI');


  SetAddColumn(DemoGrid,'id',50);
  SetAddColumn(DemoGrid,'OpenTime',120);
  SetAddColumn(DemoGrid,'OpenPrice');
  SetAddColumn(DemoGrid,'CloseTime',120);
  SetAddColumn(DemoGrid,'ClosePrice');
  SetAddColumn(DemoGrid,'Qty');
  SetAddColumn(DemoGrid,'Side');
  SetAddColumn(DemoGrid,'SL');
  SetAddColumn(DemoGrid,'TK');
  SetAddColumn(DemoGrid,'Profit');
  SetAddColumn(DemoGrid,'TypeTrade');
  SetAddColumn(DemoGrid,'FeeRatesTaker');
  SetAddColumn(DemoGrid,'FeeRatesMaker');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel := TradingPlatformOnNewCandel;
  TradingPlatform.OnMsgInfo := TradingPlatformOnMsgInfo;


  // 'DdncwwQY6AVdShL008';
  // 'ldfYDnYhlVU5SU7w89mOnaHi0icy8XctNXtT';

  TPlatfomBybit(TradingPlatform).ApiKey := 't0YI4Ou0TKOTd7WrkE';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_60;

  WorkBotDeviation := TWorkBotDeviation.Create;
  WorkBotDeviation.OnLongOpen  := WorkBotOnLongOpen;
  WorkBotDeviation.OnShortOpen := WorkBotOnShortOpen;

  DemoJournalManager := TJournalManager.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(WorkBotDeviation);
  FreeAndNil(DemoJournalManager);
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
    TypeDirection := TTypeDirection.tdNull;

    ButtonStartOrStop.Text := 'Стоп';
    TradingPlatform.Symbol := 'ETHUSDT';
    TradingPlatform.StateMarket.Qty := 1;
    TradingPlatform.Start;
  end;
end;

procedure TMainForm.DoStop;
begin
  if TradingPlatform.IsActive then
  begin
    ButtonStartOrStop.Text := 'Старт';
    TradingPlatform.Stop;
  end;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Пробитие волатильности';
end;

procedure TMainForm.ButtonStartOrStopClick(Sender: TObject);
begin
  if TradingPlatform.IsActive then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.Strategy;
begin

end;

procedure TMainForm.PositionClose(ASander: TObject);
begin
  {todo: Сообщение о закрытие}
  ButtonCloseClick(nil);
end;

procedure TMainForm.TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);

  procedure _SetUpDataPosition(AJournalManager: TJournalManager);
  var
    i, iCount: Integer;
    xPosition: TJournalPosition;
  begin
    iCount := AJournalManager.Positions.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttOpen then
        begin
          case xPosition.Side of
            TTypeBuySell.tsBuy: xPosition.SetUpData(TradingPlatform.StateMarket.Bid);
            TTypeBuySell.tsSell: xPosition.SetUpData(TradingPlatform.StateMarket.Ask);
          end;
        end;
      end;
  end;

  procedure _ShowCandel;
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    iCount := AStateMarket.Candels.Count;
    StringGridCandel.RowCount := iCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := AStateMarket.Candels[i];
        StringGridCandel.Cells[0,i] := DateTimeToStr(UnixToDateTime(xCandel.Time));
        StringGridCandel.Cells[1,i] := xCandel.Open.ToString;
        StringGridCandel.Cells[2,i] := xCandel.High.ToString;
        StringGridCandel.Cells[3,i] := xCandel.Low.ToString;
        StringGridCandel.Cells[4,i] := xCandel.Close.ToString;
        StringGridCandel.Cells[5,i] := xCandel.Vol.ToString;
        StringGridCandel.Cells[6,i] := TradingPlatform.ValuesW.ValuesWR[i].ToString;
        StringGridCandel.Cells[7,i] := TradingPlatform.ValuesW.ValuesMa1[i].ToString;
        StringGridCandel.Cells[8,i] := TradingPlatform.ValuesW.ValuesMa2[i].ToString;
      end;
  end;

  procedure _ShowPosition(const AGrid: TStringGrid; const AJournalManager: TJournalManager);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    Count := AJournalManager.Positions.Count;
    AGrid.RowCount := Count;

    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := AJournalManager.Positions[i];
        if xPosition.TypeTrade = TTypeTrade.ttClose then
          xPosition.SetUpdata;

        AGrid.Cells[0,i] := (i + 1).ToString;
        AGrid.Cells[1,i] := DateTimeToStr(xPosition.OpenTime);
        AGrid.Cells[2,i] := FloatToStr(xPosition.OpenPrice);

        if xPosition.ClosePrice = 0 then
        begin
          AGrid.Cells[3,i] := '';
          AGrid.Cells[4,i] := '';
        end else
        begin
          AGrid.Cells[3,i] := DateTimeToStr(xPosition.CloseTime);
          AGrid.Cells[4,i] := FloatToStr(xPosition.ClosePrice);
        end;

        AGrid.Cells[5,i] := FloatToStr(xPosition.Qty);
        AGrid.Cells[6,i] := GetStrToSide(xPosition.Side);
        AGrid.Cells[7,i] := FloatToStr(xPosition.StopLoss);
        AGrid.Cells[8,i] := FloatToStr(xPosition.TakeProfit);
        AGrid.Cells[9,i] := FloatToStr(xPosition.Profit);
        AGrid.Cells[10,i] := GetStrToTypeTrade(xPosition.TypeTrade);
        AGrid.Cells[11,i] := FloatToStr(xPosition.ProfitFeeRatesTaker);
        AGrid.Cells[12,i] := FloatToStr(xPosition.ProfitFeeRatesMaker);
      end;
  end;

  procedure _ShowProfit(const AJournalManager: TJournalManager; var ASumValue1, ASumValue2, ASumValue3: Double);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
  begin
    ASumValue1 := 0;
    ASumValue2 := 0;
    ASumValue3 := 0;
    Count := AJournalManager.Positions.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xPosition := AJournalManager.Positions[i];
        ASumValue1 := ASumValue1 + xPosition.Profit;
        ASumValue2 := ASumValue2 + xPosition.ProfitFeeRatesTaker;
        ASumValue3 := ASumValue3 + xPosition.ProfitFeeRatesMaker;
      end;
  end;


  procedure _ShowChart;
  var
    i, iCount: Integer;
    xValueW, xValueMa1, xValueMa2: Double;
  begin
    SeriesValueWR.Clear;
    SeriesValueMa1.Clear;
    SeriesValueMa2.Clear;

    iCount := AStateMarket.Candels.Count;
    if iCount > 0 then
      for i := iCount - 1 downto 0 do
      begin
        xValueW := TradingPlatform.ValuesW.ValuesWR[i];
        xValueMa1 := TradingPlatform.ValuesW.ValuesMA1[i];
        xValueMa2 := TradingPlatform.ValuesW.ValuesMA2[i];

        SeriesValueWR.Add(xValueW);
        SeriesValueMa1.Add(xValueMa1);
        SeriesValueMa2.Add(xValueMa2);
      end;

  end;

  procedure _InfoPanel;
  begin
    if TradingPlatform.ValuesW.ValueWR < 50 then
    begin
      RectangleTrend.Fill.Color := TAlphaColorRec.Green
    end
    else
    begin
      RectangleTrend.Fill.Color := TAlphaColorRec.Red
    end;
  end;

var
  xSumValue1, xSumValue2, xSumValue3: Double;
begin
  // ***********************************************
  // Оценка состояния рынка
  TextStatus.Text :=
    'Price: ' + TradingPlatform.StateMarket.Ask.ToString + '/' + TradingPlatform.StateMarket.Bid.ToString + '; ' +
    'ValueRSI: ' + TradingPlatform.ValuesW.ValuesWR.ToString + ';';

  EditWR.Text := TradingPlatform.ValuesW.ValueWR.ToString;
  EditFastWR.Text := TradingPlatform.ValuesW.FastValueMa.ToString;
  EditValueWR.Text := TradingPlatform.ValuesW.SlowValueMa.ToString;

  Edit1.Text := TradingPlatform.ValueVolatility.DeviationValue.ToString;
  Edit2.Text := TradingPlatform.ValueVolatility.DeviationValueQuard.ToString;

  _ShowCandel;

  Strategy;

  _SetUpDataPosition(DemoJournalManager);
  _ShowPosition(DemoGrid, DemoJournalManager);


  xSumValue1 := 0.0;
  xSumValue2 := 0.0;
  xSumValue3 := 0.0;
  _ShowProfit(DemoJournalManager, xSumValue1, xSumValue2, xSumValue3);

  EditDemo1.Text := xSumValue1.ToString;
  EditDemo2.Text := xSumValue2.ToString;
  EditDemo3.Text := xSumValue3.ToString;


  _ShowChart;
  _InfoPanel;
end;

procedure TMainForm.WorkBotOnLongOpen(Sender: TObject);

  procedure _CloseShort;
  var
    xPrice: Double;
    xPosition: TJournalPosition;
    i, iCount: Integer;
  begin
    iCount := DemoJournalManager.Positions.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xPosition := DemoJournalManager.Positions[i];
        if (xPosition.TypeTrade = TTypeTrade.ttOpen) and (xPosition.Side = TTypeBuySell.tsSell) then
        begin
          with xPosition do
          begin
            xPrice := TradingPlatform.StateMarket.Ask;
            CloseTime := GetNewDateTime;
            ClosePrice := xPrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
            DoClose;
          end;
        end;
      end;
  end;

begin
  _CloseShort;
  ButtonBuyClick(nil);
  ListBox.Items.Add('Покупка: DeviationValue = ' + TradingPlatform.ValueVolatility.DeviationValue.ToString);
end;

procedure TMainForm.WorkBotOnShortOpen(Sender: TObject);

  procedure _CloseLong;
  var
    xPrice: Double;
    xPosition: TJournalPosition;
    i, iCount: Integer;
  begin
    iCount := DemoJournalManager.Positions.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xPosition := DemoJournalManager.Positions[i];
        if (xPosition.TypeTrade = TTypeTrade.ttOpen) and (xPosition.Side = TTypeBuySell.tsBuy) then
        begin
          with xPosition do
          begin
            xPrice := TradingPlatform.StateMarket.Bid;
            CloseTime := GetNewDateTime;
            ClosePrice := xPrice;
            IsActive := False;
            TypeTrade := TTypeTrade.ttClose;
            DoClose;
          end;
        end;
      end;
  end;

begin
  _CloseLong;
  ButtonSellClick(nil);
  ListBox.Items.Add('Продажа: DeviationValue = ' + TradingPlatform.ValueVolatility.DeviationValue.ToString);
end;

procedure TMainForm.TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:nn:ss.zzz',Time) + ': ' + AMsg;
  MemoInfoMsg.Lines.Add(xS);
end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);
var
  xCandel: TCandel;
  xValueWR, xFastValueMa, xSlowValueMa: Double;
begin

  // Реализуем тактику работу системы
  xValueWR  := TradingPlatform.ValuesW.ValueWR;
  xFastValueMa := TradingPlatform.ValuesW.FastValueMa;
  xSlowValueMa := TradingPlatform.ValuesW.SlowValueMa;

  {$IFDEF DEBUG}
  TLogger.Log('NewCandel: ' + xValueWR.ToString + '; ' + xFastValueMa.ToString + '; ' + xSlowValueMa.ToString);
  {$ENDIF}

  if TradingPlatform.StateMarket.Candels.Count > 0 then
  begin
    xCandel := TradingPlatform.StateMarket.Candels[0];
    WorkBotDeviation.SetUpDataParam(
      xCandel,
      0,
      TradingPlatform.ValueVolatility.DeviationValue
    );
  end;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);

  function _GetQty: Double;
//  var
//    iCount: Integer;
//    xPosition: TJournalPosition;
  begin
    Result := 1;
//    iCount := DemoJournalManager.Positions.Count;
//    if iCount > 0 then
//    begin
//      xPosition := DemoJournalManager.Positions[iCount - 1];
//      if (xPosition.TypeTrade = TTypeTrade.ttOpen) and (xPosition.Side = TTypeBuySell.tsSell) then
//        Result := 2;
//    end;
  end;

var
  xPrice, xQty: Double;
  xPosition: TJournalPosition;
begin
  xPrice := TradingPlatform.StateMarket.Ask;
  if xPrice > 0 then
  begin
    xQty := _GetQty;
    xPosition := DemoJournalManager.GetCreateJournalPosition;
    xPosition.OnClose := PositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := xQty;
      Side := TTypeBuySell.tsBuy;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := TradingPlatform.ValueVolatility.DeviationValue;
      Profit  := 2 * TradingPlatform.ValueVolatility.DeviationValue;
      DoOpen;
    end;


    TradingPlatform.SendTrade(
      xPosition.OpenTime,
      xPosition.OpenPrice,
      xPosition.Qty,
      xPosition.Side
    );


  end;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);

  function _GetQty: Double;
//  var
//    iCount: Integer;
//    xPosition: TJournalPosition;
  begin
    Result := 1;
//    iCount := DemoJournalManager.Positions.Count;
//    if iCount > 0 then
//    begin
//      xPosition := DemoJournalManager.Positions[iCount - 1];
//      if (xPosition.TypeTrade = TTypeTrade.ttOpen) and (xPosition.Side = TTypeBuySell.tsBuy) then
//        Result := 2;
//    end;
  end;

var
  xPrice, xQty: Double;
  xPosition: TJournalPosition;
begin
  xPrice := TradingPlatform.StateMarket.Bid;
  if xPrice > 0 then
  begin
    xQty := _GetQty;
    xPosition := DemoJournalManager.GetCreateJournalPosition;
    xPosition.OnClose := PositionClose;
    with xPosition do
    begin
      OpenTime := GetNewDateTime;
      OpenPrice := xPrice;
      Qty := xQty;
      Side := TTypeBuySell.tsSell;
      IsActive := True;
      TypeTrade := TTypeTrade.ttOpen;
      Triling := TradingPlatform.ValueVolatility.DeviationValue;
      Profit  := 2 * TradingPlatform.ValueVolatility.DeviationValue;
      DoOpen;
    end;

    TradingPlatform.SendTrade(
      xPosition.OpenTime,
      xPosition.OpenPrice,
      xPosition.Qty,
      xPosition.Side
    );

  end;
end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
var
  xPrice: Double;
  xPosition: TJournalPosition;
  i, iCount: Integer;
begin
  // Закрыть позицию
  iCount := DemoJournalManager.Positions.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPosition := DemoJournalManager.Positions[i];
      if xPosition.TypeTrade = TTypeTrade.ttOpen then
      begin

        with xPosition do
        begin
          case Side of
            TTypeBuySell.tsBuy: xPrice := TradingPlatform.StateMarket.Bid;
            TTypeBuySell.tsSell: xPrice := TradingPlatform.StateMarket.Ask;
          else
            xPrice := 0;
          end;

          if xPrice <= 0 then
            Exit;

          CloseTime := GetNewDateTime;
          ClosePrice := xPrice;
          IsActive := False;
          TypeTrade := TTypeTrade.ttClose;
          DoClose;
        end;


        TradingPlatform.SendTrade(
          xPosition.CloseTime,
          xPosition.ClosePrice,
          xPosition.Qty,
          GetCrossSide(xPosition.Side)
        );

      end;


    end;
end;


end.
