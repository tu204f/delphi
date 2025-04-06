unit UnitMainForm;

interface

//{$DEFINE REAL_TRADE}

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
  Lb.Journal.Trading,
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
    TabControl1: TTabControl;
    TabItemTrade: TTabItem;
    TabItemPosition: TTabItem;
    PopupMenu: TPopupMenu;
    MenuItemSaveFile: TMenuItem;
    LayoutMarket: TLayout;
    RectangleMarket: TRectangle;
    Layout1: TLayout;
    LayoutDemo: TLayout;
    EditDemo1: TEdit;
    EditDemo2: TEdit;
    EditDemo3: TEdit;
    DemoGrid: TStringGrid;
    StringGridCandel: TStringGrid;
    TabItemInfoMsg: TTabItem;
    MemoInfoMsg: TMemo;
    GridPanelLayout: TGridPanelLayout;
    Text1: TText;
    EditVolatility: TEdit;
    Text2: TText;
    EditVolatilityHigh: TEdit;
    Text3: TText;
    EditVolatilityLow: TEdit;
    StringGridRation: TStringGrid;
    procedure ButtonStartOrStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure TradingPlatformOnStateMarket(ASender: TObject; AStateMarket: TStateMarket);
    procedure TradingPlatformOnNewCandel(Sender: TObject);
    procedure TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
  protected
    procedure DoStart;
    procedure DoStop;
  public
    WorkBots: TWorkBotDeviationList;
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

  procedure SetCreateWorkBot(const ARatio: Double);
  var
    xWorkBot: TWorkBotDeviation;
  begin
    xWorkBot := TWorkBotDeviation.Create;
    xWorkBot.TradingPlatform := TradingPlatform;
    xWorkBot.Ratio := ARatio;
    WorkBots.Add(xWorkBot);
  end;

begin
  inherited;

  SetAddColumn(StringGridCandel,'Time',150);
  SetAddColumn(StringGridCandel,'Open');
  SetAddColumn(StringGridCandel,'High');
  SetAddColumn(StringGridCandel,'Low');
  SetAddColumn(StringGridCandel,'Close');
  SetAddColumn(StringGridCandel,'Vol');


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


  SetAddColumn(StringGridRation,'ID');
  SetAddColumn(StringGridRation,'Ratio');
  SetAddColumn(StringGridRation,'Deviation');

  TradingPlatform := TPlatfomBybit.Create;
  TradingPlatform.OnStateMarket := TradingPlatformOnStateMarket;
  TradingPlatform.OnNewCandel := TradingPlatformOnNewCandel;
  TradingPlatform.OnMsgInfo := TradingPlatformOnMsgInfo;


  // 'DdncwwQY6AVdShL008';
  // 'ldfYDnYhlVU5SU7w89mOnaHi0icy8XctNXtT';
  TPlatfomBybit(TradingPlatform).ApiKey := 't0YI4Ou0TKOTd7WrkE';
  TPlatfomBybit(TradingPlatform).ApiSecret := 'dWcdTGIulDoKOiK4mggPQIkYwmMFGxvFVusp';
  TPlatfomBybit(TradingPlatform).Interval  := TTypeInterval.ti_5;

  WorkBots := TWorkBotDeviationList.Create;
  SetCreateWorkBot(0.2);
  SetCreateWorkBot(0.5);
  SetCreateWorkBot(0.75);
  SetCreateWorkBot(1.0);
  SetCreateWorkBot(1.5);
  SetCreateWorkBot(2.0);
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(WorkBots);
  FreeAndNil(TradingPlatform);
  inherited;
end;

procedure TMainForm.DoStart;
begin
  if not TradingPlatform.IsActive then
  begin
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

procedure TMainForm.TradingPlatformOnMsgInfo(ASender: TObject; AMsg: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:nn:ss.zzz',Time) + ': ' + AMsg;
  MemoInfoMsg.Lines.Add(xS);
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
            TTypeBuySell.tsBuy: begin
              var xPrice := TradingPlatform.StateMarket.Bid;
              if xPrice > 0 then
                xPosition.SetUpData(xPrice);
            end;
            TTypeBuySell.tsSell: begin
              var xPrice := TradingPlatform.StateMarket.Ask;
              if xPrice > 0 then
                xPosition.SetUpData(xPrice);
            end;
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

  procedure _ShowPosition(const AGrid: TStringGrid; const AJournalManager: TJournalManager);
  var
    i, Count: Integer;
    xPosition: TJournalPosition;
    xSumValue1, xSumValue2, xSumValue3: Double;
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

    _ShowProfit(AJournalManager,xSumValue1, xSumValue2, xSumValue3);
    EditDemo1.text := xSumValue1.ToString;
    EditDemo2.text := xSumValue2.ToString;
    EditDemo3.text := xSumValue3.ToString;
  end;



  procedure _DeviationRationStringGrid(const AIndex: Integer; const ARatio: Double);
  begin
    StringGridRation.Cells[0,AIndex] := (AIndex + 1).ToString;
    StringGridRation.Cells[1,AIndex] := 'Ratio: ' + ARatio.ToString;
    StringGridRation.Cells[2,AIndex] := TradingPlatform.ValueVolatility.GetProbability(ARatio).ToString;
  end;

  procedure _DeviationRation;
  var
    xWorkBotDeviation: TWorkBotDeviation;
    i, iCount: Integer;
  begin
    StringGridRation.RowCount := WorkBots.Count;
    iCount := WorkBots.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xWorkBotDeviation := WorkBots[i];
        _DeviationRationStringGrid(i,xWorkBotDeviation.Ratio);
        _SetUpDataPosition(xWorkBotDeviation.JournalManager);
      end;
  end;

  procedure _DeviationShowPosition(const AIndex: Integer);
  var
    xWorkBotDeviation: TWorkBotDeviation;
  begin
    xWorkBotDeviation := WorkBots[AIndex];
    _ShowPosition(DemoGrid,xWorkBotDeviation.JournalManager);
  end;

  procedure _SetUpDataParam;
  var
    xWorkBotDeviation: TWorkBotDeviation;
    i, iCount: Integer;
  begin
    iCount := WorkBots.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xWorkBotDeviation := WorkBots[i];
        xWorkBotDeviation.SetUpDataParam;
      end;
  end;

var
  xRow: Integer;
begin
  // ***********************************************
  // Оценка состояния рынка
  TextStatus.Text :=
    'Price: ' + TradingPlatform.StateMarket.Ask.ToString + '/' + TradingPlatform.StateMarket.Bid.ToString + ';';
  EditVolatility.Text     := TradingPlatform.ValueVolatility.DeviationValue.ToString;
  EditVolatilityHigh.Text := TradingPlatform.ValueVolatility.DeviationHigh.ToString;
  EditVolatilityLow.Text  := TradingPlatform.ValueVolatility.DeviationLow.ToString;

  // Обнлвдение параметров свяи
  _SetUpDataParam;

  // Насчет откланение
  _DeviationRation;

  xRow := StringGridRation.Selected;
  if xRow >= 0 then
    _DeviationShowPosition(xRow);

  // Вывод свячи
  _ShowCandel;
end;

procedure TMainForm.TradingPlatformOnNewCandel(Sender: TObject);

  procedure _SetNewCandel;
  var
    xWorkBotDeviation: TWorkBotDeviation;
    i, iCount: Integer;
  begin
    iCount := WorkBots.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xWorkBotDeviation := WorkBots[i];
        xWorkBotDeviation.SetNewCandel;
      end;
  end;

begin
  _SetNewCandel;
end;

end.
